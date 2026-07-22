# 03-splice_prices.R
# `prices_full`: the continuous spliced daily price series of Appendix D.2.
#
#   2013-2017  front-December futures settlement, current vintage   FuturesData.csv
#              (California Carbon Dashboard)                        daily
#   2018-2020  current-auction settlement                           auction reports
#              (CARB/WCI Joint Auction Summary Results)             quarterly
#   2021-2026  secondary-market settlement                          ICAP
#              (icap_secondary_prices.csv)                          daily
#
# This is NOT `clean`. `clean` is the single-instrument 2021+ series that the
# lambda_ell fit and the Prediction 4.1 volatility panel run on. `prices_full` is
# the spliced object, and exactly three results depend on it:
#
#   (1) Prediction 4.4, Step 2: the trailing twelve-month return control that
#       rejects trend-following (coefficient -1.26, t = -2.53). This is the load-
#       bearing one -- without it the rho = 0.53 co-movement does not identify the
#       regime mechanism as against generic level exposure.
#   (2) Footnote 17: floor-era rho = +0.24 (2013-2017) and pooled rho = 0.70
#       (n = 2,506).
#   (3) Table 8: the seam diagnostics themselves.
#
# Appendix D.2 states that no DAILY-frequency statistic crosses a seam, and that
# holds: the volatility panel uses the two daily segments separately. The trailing
# twelve-month return in (1) does span the bridge, but it is an annual-horizon
# statistic sampled at quarterly auction dates, not a daily one. The bridge is
# carried onto the daily grid by last observation carried forward -- never
# interpolated, since a quarterly settlement is a point observation and smoothing
# it would manufacture daily variation that does not exist.
#
#   source("R/00-ingest/03-splice_prices.R")
#   pf <- build_prices_full("data-raw/FuturesData.csv",
#                           auction_settlements,          # date, settle_cur
#                           "data-raw/icap_secondary_prices.csv")
#   splice_diagnostics(pf, auction_settlements)           # reproduces Table 8
#   saveRDS(pf, "data-derived/prices_full.rds")

suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr) })

# ---- segment 1: front-December futures, current vintage ----------------------
# For calendar year Y the current vintage is VY, quoted in column
# "V<yy> Dec<yy> - price". There is no roll gap in the source: no date in
# 2013-2017 has an empty current-vintage quote alongside a populated next-vintage
# quote, so a simple calendar-year map is the correct convention, not an
# approximation to one.
#
# DATA DEFECT, handled explicitly. The raw extract carries 17 duplicated dates
# from 2016-09-29 to 2016-10-21 whose prices disagree in the third decimal
# (e.g. 2016-09-29: 12.946 vs 12.940) -- two overlapping pulls or a vendor
# revision. That block sits inside the CalChamber litigation window, which is the
# floor-binding episode the regime classification turns on, so leaving it
# duplicated double-weights precisely the dates that matter most. Default rule is
# `first`; set dedup = "mean" to test sensitivity. Silently ignoring it is not an
# option.
read_futures <- function(path = "data-raw/FuturesData.csv",
                         years = 2013:2017, dedup = c("first", "last", "mean")) {
  dedup <- match.arg(dedup)
  raw <- readr::read_csv(path, show_col_types = FALSE,
                         name_repair = "minimal")
  raw <- raw[, names(raw) != ""]
  d <- raw %>% mutate(date = as.Date(date, format = "%m/%d/%Y")) %>% filter(!is.na(date))

  seg <- lapply(years, function(y) {
    cn <- sprintf("V%s Dec%s - price", substr(y, 3, 4), substr(y, 3, 4))
    if (!cn %in% names(d)) stop("missing column: ", cn)
    tibble(date = d$date, price = suppressWarnings(as.numeric(d[[cn]]))) %>%
      filter(!is.na(price), as.integer(format(date, "%Y")) == y)
  }) %>% bind_rows() %>% arrange(date)

  nd <- sum(duplicated(seg$date))
  if (nd) {
    rng <- range(seg$date[seg$date %in% seg$date[duplicated(seg$date)]])
    cat(sprintf("  futures: %d duplicated dates (%s to %s), resolved by '%s'\n",
                nd, rng[1], rng[2], dedup))
    seg <- switch(dedup,
      first = seg %>% group_by(date) %>% slice(1)   %>% ungroup(),
      last  = seg %>% group_by(date) %>% slice(n()) %>% ungroup(),
      mean  = seg %>% group_by(date) %>% summarise(price = mean(price), .groups = "drop"))
  }
  seg %>% mutate(segment = "futures")
}

# ---- segment 2: the quarterly auction bridge ---------------------------------
# `auctions` must carry `date` and `settle_cur` (current-vintage settlement),
# from the auction parser. Only 2018-2020 is used here; the rest of the auction
# record feeds the premium and positioning tests, not the price series.
read_bridge <- function(auctions, from = as.Date("2018-01-01"),
                        to = as.Date("2020-12-31")) {
  stopifnot(all(c("date", "settle_cur") %in% names(auctions)))
  auctions %>%
    transmute(date = as.Date(date), price = as.numeric(settle_cur)) %>%
    filter(!is.na(price), date >= from, date <= to) %>%
    arrange(date) %>% mutate(segment = "auction")
}

# ---- assemble ----------------------------------------------------------------
build_prices_full <- function(futures_path = "data-raw/FuturesData.csv",
                              auctions = NULL,
                              icap_path = "data-raw/icap_secondary_prices.csv",
                              icap_start = as.Date("2021-01-01"),
                              freeze = as.Date("2026-03-31"),
                              daily_grid = TRUE, dedup = "first") {
  f <- read_futures(futures_path, dedup = dedup)
  i <- readr::read_csv(icap_path, show_col_types = FALSE) %>%
    transmute(date = as.Date(date), price = as.numeric(price)) %>%
    filter(date >= icap_start, date <= freeze) %>% mutate(segment = "icap")
  b <- if (is.null(auctions)) {
    warning("no auction bridge supplied: 2018-2020 will be a hole, and the ",
            "trailing-return control of Prediction 4.4 cannot be reproduced")
    tibble(date = as.Date(character()), price = numeric(), segment = character())
  } else read_bridge(auctions)

  pf <- bind_rows(f, b, i) %>% arrange(date)

  if (daily_grid) {
    grid <- tibble(date = seq(min(pf$date), max(pf$date), by = "day"))
    pf <- grid %>% left_join(pf, by = "date") %>%
      tidyr::fill(price, segment, .direction = "down") %>%   # LOCF, never interpolate
      filter(!is.na(price))
  }

  cat(sprintf("prices_full: %d rows, %s to %s\n", nrow(pf), min(pf$date), max(pf$date)))
  print(pf %>% count(segment))
  as.data.frame(pf)
}

# ---- Table 8 seam diagnostics ------------------------------------------------
# Compares the daily instrument against the current-auction settlement on matched
# auction dates (nearest trading day, +/- 3 days) inside each overlap window, and
# reports the median level gap and the correlation of settlement-to-settlement
# changes. The 2017/18 figures are reported twice: as-is, and excluding the
# 2016-17 reserve-clearing episode, which is the splice diagnostic proper.
splice_diagnostics <- function(prices_full, auctions, tol_days = 3) {
  stopifnot(all(c("date", "settle_cur") %in% names(auctions)))
  daily <- prices_full %>% filter(segment %in% c("futures", "icap"))
  match_one <- function(d) {
    k <- which.min(abs(as.numeric(daily$date - d)))
    if (length(k) && abs(as.numeric(daily$date[k] - d)) <= tol_days) daily$price[k] else NA_real_
  }
  a <- auctions %>% mutate(date = as.Date(date),
                           inst = vapply(date, match_one, 0.0)) %>%
    filter(is.finite(inst), is.finite(settle_cur))

  rep_window <- function(sub, lab) {
    if (nrow(sub) < 3) { cat(sprintf("  %-34s n=%d (too few)\n", lab, nrow(sub))); return(invisible()) }
    gap <- median(sub$inst / sub$settle_cur - 1)
    cc  <- suppressWarnings(cor(diff(sub$inst), diff(sub$settle_cur)))
    cat(sprintf("  %-34s gap median %+.2f%% / change corr %.2f (n=%d)\n",
                lab, 100 * gap, cc, nrow(sub)))
  }
  cat("Seam diagnostics (Table 8):\n")
  s1 <- a %>% filter(date >= as.Date("2015-01-01"), date <= as.Date("2018-12-31"))
  rep_window(s1, "2017/18 seam, all")
  rep_window(s1 %>% filter(!(date >= as.Date("2016-01-01") & date <= as.Date("2017-04-30"))),
             "2017/18 seam, ex-2016-17 episode")
  rep_window(a %>% filter(date >= as.Date("2021-01-01")), "2021/26 interior window")
  invisible(a)
}
