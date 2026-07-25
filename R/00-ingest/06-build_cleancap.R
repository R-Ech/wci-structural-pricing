# 06-build_cleancap.R
# `cleanCap`: the annual cap-accounting frame, 2013-2024.
#
# Built by hand from CARB primary sources, then encoded here so the package is
# self-contained. Columns and their provenance:
#
#   Auctions     vintage-t allowances sold at auction, on a VINTAGE basis:
#                the current-vintage allowances sold at year t's four auctions,
#                PLUS the vintage-t allowances sold at year (t-3)'s four advance
#                auctions. Source: CARB/WCI Joint Auction Summary Results Reports.
#                Note this is allowances *of vintage t*, not allowances *sold in
#                year t* -- the right convention for a cap/bank identity, since a
#                vintage-t allowance is supply against year-t obligations whenever
#                it was bought. 2013 and 2014 have no advance leg: the program's
#                first auction was November 2012, whose advance tranche was
#                vintage 2015, so nothing was sold three years ahead of 2013-14.
#   Allocations  freely allocated allowances of vintage t. Source: CARB Public
#                Allocation Summaries. Consignment (LUHA/EDU) allowances are
#                already inside CARB's total allocation figure and must NOT be
#                added again from the auction record.
#   Emissions    headline covered emissions, year t, under the scope IN FORCE
#                that year. Source: CARB annual GHG emissions reports.
#   Offsets      offset usage at the REGULATORY MAXIMUM, not actual surrender:
#                8% of the compliance obligation for 2013-2020, 4% for 2021-2025,
#                6% from 2026. This is an upper bound (see caveats).
#   Balance      Auctions + Allocations + Offsets - Emissions
#   Bank         cumsum(Balance)
#   Balance_NoOffsets, Bank_NoOffsets   the same with Offsets set to zero
#
# THREE CAVEATS, all load-bearing.
#
# (1) `Bank` IS NOT THE PAPER'S B_t. It is a hand-built approximation bracketed
#     by the zero-offset and max-offset variants. The paper's bank is the CIR
#     private-bank series from 01-build_bank.R, and build_forcing() takes
#     B_anchor = 379.1 Mt from that, not from here. cleanCap supplies the
#     allocation and emissions PATH; the bank level is anchored independently.
#     Do not substitute one for the other.
#
# (2) The frame starts in 2015, not 2013, and this is deliberate. Coverage
#     expanded to transport fuels that year, so 2013-2014 covered emissions are
#     ~146 Mt against ~340 Mt from 2015 -- a 133% step that is real but is an
#     artifact of scope, not of the market. Dropping those two years puts every
#     row on the post-2015 basis and makes cleanCap$Emissions directly comparable
#     to the paper's broad-scope series (340.3 Mt in 2015 to 257 Mt in 2024).
#     Do not extend the frame backwards without re-basing the earlier years.
#
# (3) `Offsets` is a ceiling. Actual offset surrender is not in the CARB emissions
#     files; it comes from the compliance instrument surrender reports. Every
#     result that depends on the offset assumption is reported against both the
#     max-offset and zero-offset columns for exactly this reason.
#
#   source("R/00-ingest/06-build_cleancap.R")
#   cleanCap <- build_cleancap()
#   validate_cleancap(cleanCap)
#   saveRDS(cleanCap, "data-derived/cleanCap.rds")

suppressPackageStartupMessages({ library(dplyr) })

# ---- raw inputs ---------------------------------------------------------------
# TO FILL: run dump_cleancap_source(cleanCap) against the live object (below) and
# paste its output over this block. That preserves the exact hand-entered figures
# rather than re-deriving them, which is the point -- these came from PDFs one at
# a time and are not reproducible by script.
.YEARS <- 2015:2024

.AUCTIONS <- c(
  176405234,   # 2015
  91497455,   # 2016
  150304775,   # 2017
  213720377,   # 2018
  165337168,   # 2019
  94233554,   # 2020
  175294922,   # 2021
  145787098,   # 2022
  141527378,   # 2023
  134310817   # 2024
)

.ALLOCATIONS <- c(
  201328536,   # 2015
  192960667,   # 2016
  184280929,   # 2017
  172324101,   # 2018
  171022436,   # 2019
  183327861,   # 2020
  164056929,   # 2021
  143096251,   # 2022
  141079568,   # 2023
  139935963   # 2024
)

.EMISSIONS <- c(
  340426791,   # 2015
  324065927,   # 2016
  320583401,   # 2017
  319901543,   # 2018
  311164101,   # 2019
  278604186,   # 2020
  292218803,   # 2021
  281651431,   # 2022
  272263270,   # 2023
  256558934   # 2024
)

# Statutory offset limit as a share of the compliance obligation.
.OFFSET_LIMIT <- function(y) ifelse(y <= 2020, 0.08, ifelse(y <= 2025, 0.04, 0.06))

# ---- build --------------------------------------------------------------------
# build_forcing() indexes the cap path by rownames(cleanCap), so the years must be
# the ROW NAMES, not a column. A `yr` column is carried as well for convenience,
# but if it is ever the only year carrier, build_forcing()'s lookup silently
# resolves against "1".."12" instead of "2013".."2024" -- hence the guard in
# validate_cleancap().
build_cleancap <- function(years      = .YEARS,
                           auctions   = .AUCTIONS,
                           allocations= .ALLOCATIONS,
                           emissions  = .EMISSIONS,
                           offsets    = NULL) {
  stopifnot(length(auctions) == length(years),
            length(allocations) == length(years),
            length(emissions) == length(years))
  if (is.null(offsets)) offsets <- emissions * .OFFSET_LIMIT(years)

  bal  <- auctions + allocations + offsets - emissions
  balN <- auctions + allocations - emissions

  out <- data.frame(
    Auctions          = auctions,
    Allocations       = allocations,
    Emissions         = emissions,
    Offsets           = offsets,
    Balance           = bal,
    Bank              = cumsum(bal),
    Balance_NoOffsets = balN,
    Bank_NoOffsets    = cumsum(balN),
    yr                = years
  )
  rownames(out) <- as.character(years)
  out
}

# ---- dump the live object as source -------------------------------------------
# Emits a pasteable data block with full precision, so the hand-entered figures
# survive into the repo exactly.
dump_cleancap_source <- function(cc) {
  f <- function(nm, v) {
    cat(sprintf(".%s <- c(\n", nm))
    cat(paste0("  ", format(v, scientific = FALSE, trim = TRUE),
               c(rep(",", length(v) - 1), ""),
               "   # ", rownames(cc), collapse = "\n"))
    cat("\n)\n\n")
  }
  yrs <- if (!is.null(cc$yr)) cc$yr else as.integer(rownames(cc))
  cat(sprintf(".YEARS <- %d:%d\n\n", min(yrs), max(yrs)))
  f("AUCTIONS",    cc$Auctions)
  f("ALLOCATIONS", cc$Allocations)
  f("EMISSIONS",   cc$Emissions)
  invisible(NULL)
}

# ---- validation ---------------------------------------------------------------
# The four accounting identities are exact by construction; checking them catches
# a mis-pasted vector. The rownames guard catches the shape change that would
# silently break build_forcing().
validate_cleancap <- function(cc, ref = NULL) {
  ok <- function(lab, cond) {
    cat(sprintf("  %-38s %s\n", lab, if (isTRUE(cond)) "OK" else "*** FAIL ***")); isTRUE(cond)
  }
  cat("cleanCap identities:\n")
  a <- c(
    ok("rownames are years",      all(grepl("^20\\d{2}$", rownames(cc)))),
    ok("Balance = A + L + O - E", isTRUE(all.equal(cc$Balance,
          cc$Auctions + cc$Allocations + cc$Offsets - cc$Emissions))),
    ok("Bank = cumsum(Balance)",  isTRUE(all.equal(cc$Bank, cumsum(cc$Balance)))),
    ok("BalanceNoOff = Bal - Off",isTRUE(all.equal(cc$Balance_NoOffsets,
          cc$Balance - cc$Offsets))),
    ok("BankNoOff = cumsum",      isTRUE(all.equal(cc$Bank_NoOffsets,
          cumsum(cc$Balance_NoOffsets)))),
    ok("offset share within band",all(abs(cc$Offsets / cc$Emissions -
          .OFFSET_LIMIT(as.integer(rownames(cc)))) < 5e-4))
  )
  if (!is.null(ref)) {
    cols <- intersect(names(cc), names(ref))
    d <- max(abs(unlist(cc[cols][sapply(cc[cols], is.numeric)]) -
                 unlist(ref[cols][sapply(ref[cols], is.numeric)])))
    a <- c(a, ok(sprintf("matches reference (max diff %.3g)", d), d < 1))
  }
  invisible(all(a))
}
