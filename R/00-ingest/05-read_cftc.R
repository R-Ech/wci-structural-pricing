# 05-read_cftc.R
# CFTC Disaggregated Commitments of Traders -> weekly CCA positioning panel.
#
# Produces `cca_wide` (676 x 10, weekly 2013-05-07 onward), the positioning series
# behind Prediction 4.4: Spearman rho = 0.53 (n = 44) against the auction
# settlement premium, rho = 0.70 (n = 42) excluding the 2025-26 policy-shock floor
# touches.
#
# TWO PATHS.
#   build_cca_wide()   raw yearly COT files -> cca -> cca_wide      (audit path)
#   load_cca_wide()    read the committed data-raw/cftc/cca_cot.csv (default path)
# The raw archive covers every commodity the Commission reports and runs to
# hundreds of MB; it is gitignored. The committed extract is ~50 KB and is what
# run_all.R uses. Anyone auditing the filter downloads the yearly files from the
# CFTC and runs build_cca_wide(), then validate_cca_wide() to confirm a match.
#
# CONTRACT IDENTIFICATION. CCA futures trade on ICE Futures U.S. as vintage-
# specific contracts, 1,000 allowances each, physically delivered. The COT files
# carry SEVEN distinct name forms across the sample, all of which must match:
#   1000 CALIFORNIA CARBON ALLOWANCE          (2013 generic, no vintage)
#   CALIF CARBON ALLOWANCE-2013
#   CALIFORNIA CARBON ALLOWANCE VINTAGE 2014
#   CALIF CARBON ALL VINTAGE 2016
#   CALIF CARBON VINTAGE 2020                 (no "ALL")
#   CALIF CARBON 21                           (two-digit vintage)
#   CALIF CARBON ALLOWANCE V2022
# Two exclusions, both deliberate and both reproducing the published series:
#   CALIF CARBON CURRENT AUCTION - auction-linked, not a vintage future
#   ... - NODAL EXCHANGE            - second venue, listings from Aug 2024
# Multiple vintage contracts are live in the same week from 2015 onward, which is
# what `n_contracts` counts; the panel sums positions across them.
#
# DEFINITIONS (verified against the workspace values, see validate_cca_wide):
#   speculative = managed money + other reportables, EXCLUDING swap dealers and
#                 EXCLUDING the spread legs (the disaggregated report carries
#                 spread separately from long/short).
#   hedging     = producer/merchant long - short. Swap dealers are excluded here
#                 too; spec_share_wswap is the variant that folds them in.
#   Mt scaling  = contracts / 1000, since one contract is 1,000 allowances
#                 = 1,000 tCO2e = 0.001 Mt.
#
#   source("R/00-ingest/05-read_cftc.R")
#   cca_wide <- load_cca_wide()                       # normal use
#   raw      <- build_cca_wide("data-raw/cftc_raw")   # audit / refresh
#   validate_cca_wide(raw)
#   write_cca_extract(raw)                            # regenerate the committed CSV

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tibble)
})

# Include: California carbon contracts on ICE Futures U.S.
# Exclude: CURRENT AUCTION (auction-linked, not a vintage future) and Nodal
#          Exchange listings (present from Aug 2024; the published rho = 0.53 /
#          0.70 is computed on the ICE-only series).
CCA_INCLUDE <- regex("CALIF(ORNIA)?\\s+CARBON", ignore_case = TRUE)
CCA_VENUE   <- regex("ICE\\s+FUTURES",          ignore_case = TRUE)
CCA_EXCLUDE <- regex("CURRENT\\s+AUCTION",      ignore_case = TRUE)
# The CCA vintage-future family carries CFTC codes 0063**: 006391 (2013 generic),
# 0063E5 (V2025), 0063EE (V2026), and so on. `CALIF CARBON VINTAGE SPEC 2028`
# (006WBY) is a separate product that clears the COT reporting threshold in only
# 8 scattered weeks of 2025; it is not part of the vintage series and is excluded,
# reproducing the published positioning panel.
CCA_CODE_PREFIX <- "^0063"

# ---- raw yearly files ---------------------------------------------------------
# The position columns in the CFTC files already carry the names used downstream
# (Prod_Merc_Positions_Long_All, M_Money_Positions_Long_All, ...). Only the date,
# market-name and code columns vary across vintages of the file format, so those
# three are aliased and everything else passes through.
.alias <- list(
  date = c("Report_Date_as_YYYY-MM-DD", "Report_Date_as_MM_DD_YYYY", "As_of_Date_In_Form_YYMMDD"),
  name = c("Market_and_Exchange_Names", "Market_and_Exchange_Name"),
  code = c("CFTC_Contract_Market_Code")
)

.pick <- function(d, key) {
  hit <- .alias[[key]][.alias[[key]] %in% names(d)]
  if (!length(hit)) stop("no column for '", key, "'; found: ",
                         paste(head(names(d), 8), collapse = ", "))
  hit[1]
}

.parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  if (all(grepl("^\\d{4}-\\d{2}-\\d{2}", x[!is.na(x)])))  return(as.Date(x))
  if (all(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}", x[!is.na(x)]))) return(as.Date(x, "%m/%d/%Y"))
  as.Date(as.character(x), "%y%m%d")   # As_of_Date_In_Form_YYMMDD
}

# Read as character throughout, then coerce only the columns used. CFTC writes "."
# for missing in older files, so per-file type inference disagrees across vintages
# (the _Old/_Other crop-year splits are the usual offenders and are not used here).
.NUMCOLS <- c(
  "Open_Interest_All",
  "Prod_Merc_Positions_Long_All",  "Prod_Merc_Positions_Short_All",
  "Swap_Positions_Long_All",       "Swap__Positions_Short_All",
  "Swap__Positions_Spread_All",
  "M_Money_Positions_Long_All",    "M_Money_Positions_Short_All",
  "M_Money_Positions_Spread_All",
  "Other_Rept_Positions_Long_All", "Other_Rept_Positions_Short_All",
  "Other_Rept_Positions_Spread_All",
  "NonRept_Positions_Long_All",    "NonRept_Positions_Short_All",
  "Traders_Tot_All", "Traders_M_Money_Long_All", "Traders_Prod_Merc_Long_All",
  "Conc_Gross_LE_4_TDR_Long_All",  "Conc_Gross_LE_8_TDR_Long_All",
  "Conc_Net_LE_4_TDR_Long_All",    "Conc_Net_LE_8_TDR_Long_All"
)

read_cftc_year <- function(path) {
  d <- suppressWarnings(read_csv(path, show_col_types = FALSE, progress = FALSE,
                                 name_repair = "minimal",
                                 col_types = cols(.default = "c")))
  nm_d <- .pick(d, "date"); nm_n <- .pick(d, "name"); nm_c <- .pick(d, "code")
  d <- d %>% filter(str_detect(.data[[nm_n]], CCA_INCLUDE),
                    str_detect(.data[[nm_n]], CCA_VENUE),
                    !str_detect(.data[[nm_n]], CCA_EXCLUDE),
                    str_detect(.data[[nm_c]], CCA_CODE_PREFIX))
  if (!nrow(d)) return(NULL)
  
  miss <- setdiff(.NUMCOLS, names(d))
  if (length(miss)) warning(basename(path), ": missing ", paste(miss, collapse = ", "))
  
  out <- tibble(
    date      = .parse_date(d[[nm_d]]),
    name      = d[[nm_n]],
    code      = as.character(d[[nm_c]]),
    file_year = as.integer(str_extract(basename(path), "\\d{4}"))
  )
  for (cn in intersect(.NUMCOLS, names(d)))
    out[[cn]] <- suppressWarnings(as.numeric(str_replace_all(d[[cn]], "[,\\s]", "")))
  out
}

# Vintage from the contract name. Strips the "1000" contract-size prefix (else it
# parses as a year) and the exchange suffix, then takes a 4-digit year, falling
# back to a trailing 2-digit vintage ("CALIF CARBON 21"). NA for the 2013 generic
# contract, which carries no vintage.
.vintage <- function(nm) {
  s  <- str_remove(nm, "^\\s*1000\\s+")
  s  <- str_remove(s, "\\s*-\\s*(ICE|NODAL).*$")
  v4 <- str_extract(s, "20\\d{2}")
  v2 <- str_extract(s, "(?<=CARBON )\\d{2}\\s*$")
  as.integer(ifelse(!is.na(v4), v4, ifelse(!is.na(v2), paste0("20", v2), NA)))
}

# ---- long panel: one row per contract-week ------------------------------------
build_cca <- function(raw_dir = "data-raw/cftc_raw") {
  files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) stop("no CSVs in ", raw_dir,
                           " -- the raw archive is gitignored; see DATA.md")
  
  if (!all(grepl("^0063", cca$code)))
    stop("non-0063 contract codes present: ",
         paste(unique(cca$code[!grepl("^0063", cca$code)]), collapse = ", "))
  
  cca <- bind_rows(lapply(files, read_cftc_year))

  # De-duplicate: the combined historical file (F_Disagg06_16.csv) overlaps the
  # yearly files. Same contract-week appearing twice would double every position.
  n0 <- nrow(cca)
  cca <- cca %>% distinct(date, code, .keep_all = TRUE)
  if (nrow(cca) < n0)
    message("dropped ", n0 - nrow(cca), " duplicated contract-weeks ",
            "(historical file overlaps the yearly files)")

  cca %>%
    mutate(
      vintage    = .vintage(name),
      spec_long  = M_Money_Positions_Long_All  + Other_Rept_Positions_Long_All,
      spec_short = M_Money_Positions_Short_All + Other_Rept_Positions_Short_All,
      spec_net   = spec_long - spec_short,
      hedge_long = Prod_Merc_Positions_Long_All,
      hedge_short= Prod_Merc_Positions_Short_All,
      spec_share = spec_long / Open_Interest_All,
      spec_share_wswap = (spec_long + Swap_Positions_Long_All) / Open_Interest_All,
      oi_Mt      = Open_Interest_All / 1000,
      spec_net_Mt= spec_net / 1000
    ) %>%
    arrange(date, code)
}

# ---- weekly wide panel --------------------------------------------------------
# Positions sum across the live vintage contracts; concentration ratios are ratios
# and cannot be summed, so the panel carries the worst case across contracts
# (hence the _max suffix).
#
# [VERIFY] traders_tot aggregation. With one live contract (2013-2014) it is
# unambiguous. From 2015 there are 2-3 contracts and the workspace value cannot be
# decomposed from the summary alone -- sum double-counts any trader active in more
# than one vintage, max understates the total. `sum` is the default here because it
# matches the treatment of every other count column; check one multi-contract week
# against the workspace before relying on this column. Nothing in the paper's
# reported results uses traders_tot, so this is a diagnostic-only ambiguity.
build_cca_wide <- function(raw_dir = "data-raw/cftc_raw", traders = c("sum", "max")) {
  traders <- match.arg(traders)
  cca <- build_cca(raw_dir)
  agg <- cca %>%
    group_by(date) %>%
    summarise(
      n_contracts    = n(),
      oi_Mt          = sum(Open_Interest_All) / 1000,
      spec_long_Mt   = sum(spec_long)  / 1000,
      spec_net_Mt    = sum(spec_net)   / 1000,
      hedge_net_Mt   = sum(hedge_long - hedge_short) / 1000,
      spec_share     = sum(spec_long) / sum(Open_Interest_All),
      conc4_long_max = max(Conc_Gross_LE_4_TDR_Long_All, na.rm = TRUE),
      conc8_long_max = max(Conc_Gross_LE_8_TDR_Long_All, na.rm = TRUE),
      traders_tot    = if (traders == "sum") sum(Traders_Tot_All, na.rm = TRUE)
                       else max(Traders_Tot_All, na.rm = TRUE),
      .groups = "drop"
    ) %>% arrange(date)
  attr(agg, "cca_long") <- cca
  agg
}

# ---- committed extract --------------------------------------------------------
write_cca_extract <- function(cca_wide, path = "data-raw/cftc/cca_cot.csv") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write_csv(cca_wide, path)
  message("wrote ", path, " (", nrow(cca_wide), " weekly observations)")
  invisible(path)
}

load_cca_wide <- function(path = "data-raw/cftc/cca_cot.csv") {
  d <- read_csv(path, show_col_types = FALSE) %>% mutate(date = as.Date(date))
  cat(sprintf("cca_wide: %d weekly obs, %s to %s\n", nrow(d), min(d$date), max(d$date)))
  d
}

# ---- validation ---------------------------------------------------------------
# Pinned against the 2013-05-07 print, where a single contract was live and every
# derivation is checkable by hand:
#   OI 5,971 contracts -> 5.971 Mt
#   producer/merchant 2,914 long - 2,051 short -> hedge_net 0.863 Mt
#   spec_long 2,560 contracts -> 2.560 Mt, spec_share 0.4287
#   spec_net -4 contracts -> -0.004 Mt
validate_cca_wide <- function(cca_wide) {
  r <- cca_wide[cca_wide$date == as.Date("2013-05-07"), ]
  if (!nrow(r)) { warning("2013-05-07 print absent; cannot validate"); return(invisible(FALSE)) }
  chk <- function(lab, got, want, tol = 5e-4) {
    ok <- abs(got - want) <= tol
    cat(sprintf("  %-16s %9.4f  want %9.4f  %s\n", lab, got, want, if (ok) "OK" else "*** FAIL ***"))
    ok
  }
  cat("2013-05-07 print:\n")
  all_ok <- c(
    chk("oi_Mt",        r$oi_Mt,        5.971),
    chk("spec_long_Mt", r$spec_long_Mt, 2.559),
    chk("spec_net_Mt",  r$spec_net_Mt, -0.004),
    chk("hedge_net_Mt", r$hedge_net_Mt, 0.863),
    chk("spec_share",   r$spec_share,   0.4287, 1e-3),
    chk("n_contracts",  r$n_contracts,  1)
  )
  cat(sprintf("  %-16s %9d  want %9d  %s\n", "total weeks", nrow(cca_wide), 676L,
              if (nrow(cca_wide) == 676L) "OK" else "*** CHECK FREEZE DATE ***"))
  invisible(all(all_ok))
}

# ---- join to auctions ---------------------------------------------------------
# Nearest COT report date ON OR BEFORE the auction, within 14 days: positioning
# going INTO the auction. The report date is the Tuesday, not the Friday release.
# Reproduces `panel` (45 x 13); `scatter_df$spec_net_Mt` is the identical series
# carried on the auction frame -- the two were confirmed byte-identical, so either
# may be used and Steps 1 and 2 of Prediction 4.4 run on the same data.
build_panel <- function(auc, cca_wide, max_lag_days = 14) {
  stopifnot(all(c("adate", "premium") %in% names(auc)))
  idx <- vapply(auc$adate, function(d) {
    k <- which(cca_wide$date <= d)
    if (!length(k)) return(NA_integer_)
    j <- k[which.max(cca_wide$date[k])]
    if (as.numeric(d - cca_wide$date[j]) > max_lag_days) NA_integer_ else j
  }, integer(1))
  out <- bind_cols(auc, cca_wide[idx, ]) %>% filter(!is.na(date))
  cat(sprintf("panel: %d of %d auctions matched within %d days\n",
              nrow(out), nrow(auc), max_lag_days))
  out
}
