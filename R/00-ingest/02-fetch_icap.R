# 02-fetch_icap.R
# `clean`: daily CCA secondary-market settlements, the 2021-2026 segment.
#
# Input is data-raw/icap_secondary_prices.csv, two columns, `date` and `price`,
# extracted from the ICAP Allowance Price Explorer (system id 6, CCA secondary).
# The extract runs 2020-11-02 to 2026-03-31; `clean` starts 2021-03-30.
#
# The 2021-03-30 start is a LIQUIDITY cut, not a data-availability cut. The 100
# dropped observations exist but predate the secondary market's lift-off and are
# not informative at daily frequency.
#
# `clean` carries `date` and `price` and nothing else. That is deliberate: those
# are the only columns fit_lambda_ell() reads. The model's floor is NOT taken from
# the data -- it is the analytic escalator R(t) = R0 exp(gR (t-2021)) built by
# build_forcing() with R0 = 17.71, gR = 0.075. If you want an observed reserve for
# plotting, attach it from the auction reserve schedule produced by the auction
# parser, not from ICAP; see check_floor_drift() for why the two differ.
#
# Anything needing pre-2021 daily prices uses `prices_full` from 03-splice_prices.R.
# Do not substitute one for the other: `clean` is a single instrument, `prices_full`
# is a three-instrument splice with two seams.
#
#   source("R/00-ingest/02-fetch_icap.R")
#   clean <- build_clean("data-raw/icap_secondary_prices.csv")
#   saveRDS(clean, "data-derived/clean.rds")

suppressPackageStartupMessages({ library(dplyr); library(readr) })

LIQUID_START <- as.Date("2021-03-30")

build_clean <- function(csv_path = "data-raw/icap_secondary_prices.csv",
                        start = LIQUID_START, freeze = as.Date("2026-03-31")) {
  d <- readr::read_csv(csv_path, show_col_types = FALSE)
  stopifnot(all(c("date", "price") %in% names(d)))
  d <- d %>%
    transmute(date = as.Date(date), price = as.numeric(price)) %>%
    filter(is.finite(price), !is.na(date)) %>%
    arrange(date)

  if (any(duplicated(d$date)))
    stop("duplicate dates in ICAP extract: ", sum(duplicated(d$date)),
         " -- resolve before use, do not silently de-duplicate a price series")

  out <- d %>% filter(date >= start, date <= freeze)
  gaps <- as.numeric(diff(out$date))
  cat(sprintf("clean: %d obs, %s to %s | $%.2f-$%.2f | %d dropped by liquidity cut | max gap %d d\n",
              nrow(out), min(out$date), max(out$date), min(out$price), max(out$price),
              sum(d$date < start), max(gaps)))
  as.data.frame(out)
}

# ---- diagnostic: the model's analytic floor vs. the statutory reserve ---------
# fit_lambda_ell() classifies a date as interior when price > 1.03 * R(t), with
# R(t) analytic. The statutory reserve escalates at 5% + realized CPI; the model
# uses a constant 7.5%, which understates 2022-23 and overstates thereafter. Pass
# the observed annual reserve schedule to see the size of the gap. This matters
# only for near-floor dates -- the 2025-05 and 2026-02 reserve touches -- and
# lambda_ell_hat is a median over interior dates, so a handful of borderline
# reclassifications will not move 0.10. Report the number, don't assume it.
check_floor_drift <- function(clean, reserve_by_year = NULL,
                              R0 = 17.71, gR = 0.075, base_year = 2021) {
  t   <- base_year + as.numeric(clean$date - as.Date("2021-01-01")) / 365.25
  Ran <- R0 * exp(gR * (t - base_year))
  cat(sprintf("dates within 3%% of analytic R(t): %d of %d\n",
              sum(clean$price < 1.03 * Ran), nrow(clean)))
  if (!is.null(reserve_by_year)) {
    obs <- reserve_by_year[as.character(as.integer(format(clean$date, "%Y")))]
    rel <- as.numeric(obs) / Ran - 1
    cat(sprintf("analytic vs statutory reserve: median %+.2f%%, max |gap| %.2f%%\n",
                100 * median(rel, na.rm = TRUE), 100 * max(abs(rel), na.rm = TRUE)))
    return(invisible(data.frame(date = clean$date, R_analytic = Ran,
                                reserve_obs = as.numeric(obs), rel_gap = rel)))
  }
  invisible(data.frame(date = clean$date, R_analytic = Ran))
}
