# 01-build_bank.R
# Quarterly WCI private allowance bank B_t and the surrender-flow validation
# series, from the CARB Compliance Instrument Reports in ./cir_raw/.
#
# Bank definition (IEMAC / Cullenward et al. 2019, Burtraw et al. 2022a):
#   B_t = sum over vintages v <= cutoff(t) of [General_v + Compliance_v]
# over private Entity accounts only. Government-held, consigned (LUHA), and
# already-retired instruments are excluded by construction.
#
# WHICH COLUMN IS THE PAPER'S BANK. `bank_vintage_restricted`. Section 4.1 is
# explicit that summing all vintages "double-counts instruments owed to the
# regulator and yields a meaningless figure on the order of one billion" -- that
# is `bank_all_private` (831 Mt at Q4-2024) and, only marginally better,
# `bank_continuous` (802 Mt). Both are DIAGNOSTICS. The CIR-validated anchor that
# build_forcing() takes as B_anchor = 379.1 Mt is bank_vintage_restricted, and
# Table 4 bridges from it to the projection launch state by adding the open-period
# flows. Do not wire either diagnostic column into the model.
#
# Run 00-download_cir.R first. Then: source("01-build_bank.R"); out <- run_all()

suppressMessages({
  library(readxl); library(readr); library(stringr)
  library(dplyr); library(purrr); library(tibble); library(tidyr)
})

num <- function(x) suppressWarnings(as.numeric(str_replace_all(str_trim(as.character(x)), "[,\\$]", "")))

# ---- core extractor ------------------------------------------------------------
# Anchors on the header row carrying both "General" and "Compliance", then keeps
# every row whose first column is a four-digit vintage.
#
# GUARD ADDED. That row rule scans the WHOLE sheet, not just the block under the
# matched header. If a report ever carries a second account-type grid with its own
# vintage rows, both blocks are silently summed and the bank inflates with no
# error. Vintages must be unique; assert it rather than trust the layout.
extract_grid <- function(m) {
  m <- apply(m, 2, as.character); m[is.na(m)] <- ""
  is_hdr <- apply(m, 1, function(r)
    any(str_detect(r, regex("^\\s*General\\s*$", TRUE))) &&
      any(str_detect(r, regex("^\\s*Compliance\\s*$", TRUE))))
  hrow <- which(is_hdr)[1]
  if (is.na(hrow)) stop("no General/Compliance header row found")
  if (length(which(is_hdr)) > 1L)
    warning("multiple General/Compliance header rows (", length(which(is_hdr)),
            "): sheet may carry more than one account-type block")
  hdr <- str_trim(m[hrow, ])
  c_gen <- which(str_detect(hdr, regex("^General$", TRUE)))[1]
  c_cmp <- which(str_detect(hdr, regex("^Compliance$", TRUE)))[1]
  c_ret <- which(str_detect(hdr, regex("^Retirement", TRUE)))[1]
  v <- suppressWarnings(as.integer(str_extract(str_trim(m[, 1]), "^(19|20)\\d{2}$")))
  keep <- which(!is.na(v) & v >= 2012 & v <= 2050)
  if (any(duplicated(v[keep])))
    stop("duplicated vintages: ", paste(unique(v[keep][duplicated(v[keep])]), collapse = ", "),
         " -- more than one vintage grid captured; restrict the row scan to the ",
         "block under row ", hrow)
  tibble(
    vintage    = v[keep],
    general    = num(m[keep, c_gen]),
    compliance = num(m[keep, c_cmp]),
    retirement = if (!is.na(c_ret)) num(m[keep, c_ret]) else NA_real_
  )
}

read_grid <- function(path, sheet = 1) {
  if (str_detect(path, "\\.csv$")) {
    raw <- suppressWarnings(read_csv(path, col_names = FALSE, show_col_types = FALSE,
                                     col_types = cols(.default = "c")))
  } else {
    raw <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE,
                                       .name_repair = "minimal", col_types = "text"))
  }
  extract_grid(as.matrix(raw))
}

# ---- cutoff rule -----------------------------------------------------------------
# Highest vintage whose compliance period has fully closed as of the snapshot, NOT
# a calendar-rolling cutoff. WCI periods: 2013-14, 2015-17, 2018-20, 2021-23,
# 2024-26. A period trues up the following November 1. Reproduces 321.1 Mt at the
# 2020/21 boundary and 379.1 Mt at Q4-2024.
.period_end <- c(2014L, 2017L, 2020L, 2023L, 2026L, 2029L, 2032L, 2035L)
cutoff_vintage <- function(snapshot_date) {
  closed <- .period_end[ as.Date(sprintf("%d-11-01", .period_end + 1L)) <= snapshot_date ]
  if (length(closed) == 0) return(min(.period_end) - 1L)
  max(closed)
}

.period_end_all <- c(2014L, 2017L, 2020L, 2023L, 2026L, 2029L, 2032L, 2035L, 2038L)
current_period_end <- function(snapshot_date) {
  y <- as.integer(format(snapshot_date, "%Y"))
  min(.period_end_all[.period_end_all >= y])
}

bank_from_grid <- function(grid, snapshot_date, cutoff = NA) {
  cut  <- if (is.na(cutoff)) cutoff_vintage(snapshot_date) else as.integer(cutoff)
  ccut <- current_period_end(snapshot_date)
  gc   <- grid$general + grid$compliance
  tibble(
    snapshot = snapshot_date, cutoff = cut,
    # THE PAPER'S BANK. IEMAC closed-period metric; B_anchor = 379.1 Mt at Q4-2024.
    bank_vintage_restricted = sum(gc[grid$vintage <= cut], na.rm = TRUE),
    # DIAGNOSTIC ONLY: adds the open compliance period's vintages (802 Mt at
    # Q4-2024). Section 4.1 rejects figures of this order. Not a model input.
    bank_continuous         = sum(gc[grid$vintage <= ccut], na.rm = TRUE),
    # DIAGNOSTIC ONLY: all private holdings incl. pure forwards (831 Mt at Q4-2024).
    bank_all_private        = sum(gc, na.rm = TRUE),
    # Cumulative retirement on the SAME rolling cutoff as the bank. Kept for
    # continuity; do NOT difference it -- see retirement_cum_all.
    retirement_cum          = sum(grid$retirement[grid$vintage <= cut], na.rm = TRUE),
    # Cumulative retirement over ALL vintages, i.e. a fixed basis. This is the one
    # to difference: the rolling-cutoff series jumps by ~748 Mt at each true-up
    # (Q4-2024: 2.665 Gt -> 3.413 Gt) purely because three vintages enter the sum,
    # which is not a surrender flow. Differencing the rolling series manufactures a
    # phantom triennial spike.
    retirement_cum_all      = sum(grid$retirement, na.rm = TRUE)
  )
}

cutoff_scan <- function(grid, snapshot_date, offsets = 1:3) {
  y <- as.integer(format(snapshot_date, "%Y"))
  map_dfr(offsets, function(o) tibble(
    cutoff = y - o,
    bank   = sum((grid$general + grid$compliance)[grid$vintage <= (y - o)], na.rm = TRUE)
  ))
}

# ---- driver --------------------------------------------------------------------
run_all <- function(raw_dir = "cir_raw") {
  man <- readRDS(file.path(raw_dir, "_manifest.rds"))
  ind <- man %>% filter(report != "PRE2019")

  rows <- map_dfr(seq_len(nrow(ind)), function(i) {
    f <- file.path(raw_dir, sprintf("%s.%s", ind$report[i], ind$fmt[i]))
    if (!file.exists(f)) { message("missing ", f); return(NULL) }
    g <- tryCatch(read_grid(f), error = function(e){message("parse fail ",ind$report[i],": ",conditionMessage(e)); NULL})
    if (is.null(g)) return(NULL)
    bank_from_grid(g, as.Date(ind$snapshot[i])) %>% mutate(report = ind$report[i], .before = 1)
  })

  pre <- file.path(raw_dir, "PRE2019.xlsx")
  if (file.exists(pre)) {
    for (sh in excel_sheets(pre)) {
      g <- tryCatch(read_grid(pre, sheet = sh), error = function(e) NULL)
      if (is.null(g) || nrow(g) == 0) next
      yr <- as.integer(str_extract(sh, "20\\d{2}")); qq <- str_extract(sh, "[Qq]\\s?[1-4]")
      if (is.na(yr) || is.na(qq)) next
      q <- as.integer(str_extract(qq, "[1-4]"))
      snap <- as.Date(sprintf("%d-%02d-01", if (q==4) yr+1 else yr, (q %% 4)*3 + 1))
      rows <- bind_rows(rows,
                        bank_from_grid(g, snap) %>% mutate(report = sprintf("%dQ%d", yr, q), .before = 1))
    }
  }

  rows <- rows %>% arrange(snapshot) %>%
    mutate(surrender_flow = retirement_cum_all - lag(retirement_cum_all),
           cutoff_rolled  = cutoff != lag(cutoff))
  n_neg <- sum(rows$surrender_flow < 0, na.rm = TRUE)
  if (n_neg) warning(n_neg, " negative surrender flows: retirement is cumulative and ",
                     "should be non-decreasing; check for a re-stated report")
  out_dir <- "cir_out"; dir.create(out_dir, showWarnings = FALSE)
  write_csv(rows, file.path(out_dir, "bank_quarterly.csv"))
  message("wrote ", file.path(out_dir, "bank_quarterly.csv"), " (", nrow(rows), " quarters)")
  rows
}

# ---- embedded unit test (runs without any downloaded files) --------------------
test_bank <- function() {
  g <- tribble(
    ~vintage, ~general,   ~compliance, ~retirement,
    2013, 270990,    0,         183864164,
    2014, 139907,    0,         180931093,
    2015, 825654,    402,       440581944,
    2016, 4684078,   8810,      393908774,
    2017, 5914743,   229695,    400571998,
    2018, 9734754,   550242,    384702599,
    2019, 24624847,  2375311,   347869519,
    2020, 30629393,  564224,    333032335,
    2021, 81775285,  2639397,   280052158,
    2022, 64232300,  6346413,   276996795,
    2023, 90713036,  52839288,  190853476,
    2024, 178520507, 133898371, 3651940,
    2025, 57045083,  23355908,  2982443,
    2026, 30308000,  0,         1134079,
    2027, 28844000,  0,         1134079
  )
  b <- bank_from_grid(g, as.Date("2025-01-06"))
  stopifnot(b$cutoff == 2023L)
  stopifnot(b$bank_vintage_restricted == 379098769)   # = B_anchor 379.1 Mt
  stopifnot(b$bank_continuous         == 802226638)   # diagnostic, pinned
  stopifnot(b$bank_all_private        == 831070638)   # diagnostic, pinned
  cat("PASS  bank(<=2023) =", format(b$bank_vintage_restricted, big.mark=","),
      "| continuous =", format(b$bank_continuous, big.mark=","),
      "| all-private =", format(b$bank_all_private, big.mark=","), "\n")
  print(cutoff_scan(g, as.Date("2025-01-06")))
  invisible(b)
}

if (sys.nframe() == 0) test_bank()
