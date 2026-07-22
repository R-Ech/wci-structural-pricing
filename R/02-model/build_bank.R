# build_bank.R
# Construct the quarterly WCI private allowance bank B_t and the surrender-flow
# validation series from the CARB Compliance Instrument Reports in ./cir_raw/.
#
# Bank definition (Cullenward / IEMAC, vintage-restricted):
#   B_t = sum over vintages v <= cutoff(t) of [General_v + Compliance_v]   (Entity accounts only)
# Everything else in the report is government-held, consigned (LUHA), or already
# retired, and is excluded by construction -- this is what avoids the consignment
# double-counting trap and the future-vintage inflation.
#
# Run download_cir.R first. Then: source("build_bank.R"); out <- run_all()

suppressMessages({
  library(readxl); library(readr); library(stringr)
  library(dplyr); library(purrr); library(tibble); library(tidyr)
})

# ---- number cleaner: strips commas, $, spaces; "" / "-" -> NA -------------------
num <- function(x) suppressWarnings(as.numeric(str_replace_all(str_trim(as.character(x)), "[,\\$]", "")))

# ---- core extractor ------------------------------------------------------------
# Input: raw character matrix of a report sheet (no header assumptions).
# Output: tibble(vintage, general, compliance, retirement) for numeric-vintage rows.
# Anchors on the header row containing both "General" and "Compliance", so it is
# robust to leading blank/merged-header rows and to columns shifting across eras.
extract_grid <- function(m) {
  m <- apply(m, 2, as.character); m[is.na(m)] <- ""
  is_hdr <- apply(m, 1, function(r)
    any(str_detect(r, regex("^\\s*General\\s*$", TRUE))) &&
      any(str_detect(r, regex("^\\s*Compliance\\s*$", TRUE))))
  hrow <- which(is_hdr)[1]
  if (is.na(hrow)) stop("no General/Compliance header row found")
  hdr <- str_trim(m[hrow, ])
  c_gen <- which(str_detect(hdr, regex("^General$", TRUE)))[1]
  c_cmp <- which(str_detect(hdr, regex("^Compliance$", TRUE)))[1]
  c_ret <- which(str_detect(hdr, regex("^Retirement", TRUE)))[1]   # may carry footnote marks
  v <- suppressWarnings(as.integer(str_extract(str_trim(m[, 1]), "^(19|20)\\d{2}$")))
  keep <- which(!is.na(v) & v >= 2012 & v <= 2050)
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
# IEMAC / Cullenward-Burtraw definition: General + Compliance over vintages whose
# COMPLIANCE PERIOD HAS FULLY CLOSED as of the snapshot -- NOT a calendar-rolling
# cutoff. WCI compliance periods: 2013-14, 2015-17, 2018-20, 2021-23, 2024-26, ...
# A period's instruments are retired at its triennial true-up (the following Nov 1).
# Reproduces the published anchors: ~321M at the 2020/21 boundary; 379M at Q4-2024.
# Holding the rule fixed (rather than rolling each year) removes the spurious
# whole-vintage jumps while preserving the genuine surrender drawdown.
.period_end <- c(2014L, 2017L, 2020L, 2023L, 2026L, 2029L, 2032L, 2035L)
cutoff_vintage <- function(snapshot_date) {
  # latest period whose triennial surrender (Nov 1 of period_end + 1) has occurred
  closed <- .period_end[ as.Date(sprintf("%d-11-01", .period_end + 1L)) <= snapshot_date ]
  if (length(closed) == 0) return(min(.period_end) - 1L)  # before first true-up
  max(closed)
}

# current compliance period's final vintage (periods: 2013-14, 2015-17, then triennial)
.period_end_all <- c(2014L, 2017L, 2020L, 2023L, 2026L, 2029L, 2032L, 2035L, 2038L)
current_period_end <- function(snapshot_date) {
  y <- as.integer(format(snapshot_date, "%Y"))
  min(.period_end_all[.period_end_all >= y])
}

bank_from_grid <- function(grid, snapshot_date, cutoff = NA) {
  cut  <- if (is.na(cutoff)) cutoff_vintage(snapshot_date) else as.integer(cutoff)
  ccut <- current_period_end(snapshot_date)   # continuous-definition cut
  gc   <- grid$general + grid$compliance
  tibble(
    snapshot = snapshot_date, cutoff = cut,
    # MODEL STATE VARIABLE B_t: continuous, includes current-period vintages,
    # excludes next-period advance-auction forwards. No true-up jumps.
    bank_continuous         = sum(gc[grid$vintage <= ccut], na.rm = TRUE),
    # IEMAC / Cullenward closed-period bank: published cross-check at true-up points only.
    bank_vintage_restricted = sum(gc[grid$vintage <= cut], na.rm = TRUE),
    # all private holdings incl. pure forwards (diagnostic).
    bank_all_private        = sum(gc, na.rm = TRUE),
    retirement_cum          = sum(grid$retirement[grid$vintage <= cut], na.rm = TRUE)
  )
}

# diagnostic: bank at several cutoffs so you can see which matches the anchor
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
  
  # pre-2019 combined workbook: one sheet per quarter
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
    mutate(surrender_flow = retirement_cum - lag(retirement_cum))  # delta-Retirement
  out_dir <- "cir_out"; dir.create(out_dir, showWarnings = FALSE)
  write_csv(rows, file.path(out_dir, "bank_quarterly.csv"))
  message("wrote ", file.path(out_dir, "bank_quarterly.csv"), " (", nrow(rows), " quarters)")
  rows
}

# ---- embedded unit test (runs without any downloaded files) --------------------
# Reconstructs the Q4-2024 report grid (snapshot 2025-01-06) from the published
# Entity-account values and asserts the vintage-restricted bank == 379,098,769.
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
  stopifnot(b$bank_vintage_restricted == 379098769)
  cat("PASS  bank(<=2023) =", format(b$bank_vintage_restricted, big.mark=","),
      " | all-private =", format(b$bank_all_private, big.mark=","), "\n")
  print(cutoff_scan(g, as.Date("2025-01-06")))
  invisible(b)
}

if (sys.nframe() == 0) test_bank()   # `Rscript build_bank.R` runs the test
