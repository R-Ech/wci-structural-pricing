# run_all.R -------------------------------------------------------------------
# Full reproduction of "A Structural Model of WCI Allowance Pricing" (layered banking model,
# version of November 2026). The PDE version of the paper is tag v1.0 of this repository.
#
#   Rscript run_all.R              # full pipeline
#   Rscript run_all.R --quick      # every 25th date; gates reported but not enforced
#   Rscript run_all.R --stage=3    # start at stage 3, reusing data-derived/
#
# Stages: 1 ingest, 2 inputs, 3 solver check, 4 model and tables, 5 extra figures and forecast.
# The gates reproduce numbers the paper reports. If one trips, find out what changed.

options(stringsAsFactors = FALSE, warn = 1)
args   <- commandArgs(trailingOnly = TRUE)
QUICK  <- "--quick" %in% args
STAGE0 <- as.integer(sub("--stage=", "", grep("^--stage=", args, value = TRUE)))
if (!length(STAGE0)) STAGE0 <- 1L

dir.create("data-derived", showWarnings = FALSE)
dir.create("paper/figures", recursive = TRUE, showWarnings = FALSE)

run_stage <- function(n) n >= STAGE0
say  <- function(...) cat(sprintf("\n[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")
gate <- function(label, got, want, tol) {
  got <- suppressWarnings(as.numeric(got)[1])
  ok  <- isTRUE(is.finite(got) && abs(got - want) <= tol)
  cat(sprintf("  %-46s %10s  (paper %.4f +/- %.4f)  %s\n", label,
              if (isTRUE(is.finite(got))) sprintf("%.4f", got) else "MISSING", want, tol, if (ok) "OK" else "*** FAIL ***"))
  if (!ok && !QUICK) stop("gate failed: ", label)
  invisible(ok)
}
need <- function(f, how) if (!file.exists(f)) stop(f, " is missing. ", how)

# =============================================================================
# STAGE 1 -- ingest.  raw sources -> data-derived/*.rds   (unchanged from v1.0, minus the
# futures splice and the CFTC panel, which the paper no longer uses)
# =============================================================================
if (run_stage(1)) {
  say("STAGE 1  ingest")

  source("R/00-ingest/01-build_bank.R")
  bank <- run_all(raw_dir = "data-raw/cir_raw")
  saveRDS(bank, "data-derived/bank_quarterly.rds")
  gate("B(Q4-2024) vintage-restricted, Mt",
       bank$bank_vintage_restricted[bank$report == "2024Q4"] / 1e6, 379.1, 0.1)

  source("R/00-ingest/06-build_cleancap.R")
  cleanCap <- build_cleancap()
  stopifnot(validate_cleancap(cleanCap))
  saveRDS(cleanCap, "data-derived/cleanCap.rds")

  source("R/00-ingest/02-fetch_icap.R")
  if (!file.exists("data-raw/icap_secondary_prices.csv"))
    stop("data-raw/icap_secondary_prices.csv not found. The daily ICE series is not redistributed, see DATA.md.")
  clean <- build_clean("data-raw/icap_secondary_prices.csv")
  saveRDS(clean, "data-derived/clean.rds")
  gate("clean: n daily obs", nrow(clean), 1275, 0)

  auc <- read.csv("data-raw/auctions.csv"); auc$date <- as.Date(auc$date)
  saveRDS(auc, "data-derived/auctions.rds")
  ins <- auc[auc$date <= as.Date("2026-03-31"), ]
  gate("auctions in sample", nrow(ins), 44, 0)
  gate("auctions settled at the reserve, in sample",
       sum(abs(ins$settlement_price_cur / ins$auction_reserve_price_cur - 1) < 1e-9, na.rm = TRUE), 9, 0)
}

# =============================================================================
# STAGE 2 -- inputs that are not built by the ingest scripts
# =============================================================================
# A (California budgets 2021-2045), floor_schedule and ceiling_schedule are small hand-entered
# tables. They are committed as .rds, like cleanCap, until 07-build_schedules.R exists.
# The Quebec totals and the reserve tiers are entered in pipeline.R, with their sources.
if (run_stage(2)) {
  say("STAGE 2  inputs")
  hint <- "Save it once from the working session with saveRDS() and commit it (see README, 'Not yet automated')."
  for (f in c("A", "floor_schedule", "ceiling_schedule")) need(sprintf("data-derived/%s.rds", f), hint)
  A <- readRDS("data-derived/A.rds")
  gate("California budget 2026, Mt", { b <- A$Budget[A$Year == 2026]; if (b > 1e4) b / 1e6 else b }, 254.0, 0.1)

  need("data-raw/DGS10.csv", "Download the 10-year Treasury constant maturity series from FRED.")
  # reads the FRED file whatever its separator, column order or date format
  l1  <- readLines("data-raw/DGS10.csv", n = 2)
  sep <- if (sum(lengths(regmatches(l1, gregexpr(";", l1)))) > 0) ";" else ","
  x   <- read.csv("data-raw/DGS10.csv", sep = sep, skip = as.integer(grepl("^sep=", l1[1])),
                  na.strings = c("", ".", "NA", "ND"), colClasses = "character")
  to_date <- function(v) { for (f in c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y")) {
      d <- as.Date(v, format = f); if (mean(!is.na(d)) > 0.95 && all(format(d[!is.na(d)], "%Y") > "1950")) return(d) }
    NULL }
  dcol <- NULL; for (j in seq_along(x)) { d <- to_date(x[[j]]); if (!is.null(d)) { dcol <- j; break } }
  if (is.null(dcol)) stop("DGS10.csv: no column reads as a date. First lines: ", paste(l1, collapse = " | "))
  ycol <- setdiff(seq_along(x), dcol)
  ycol <- ycol[which.max(sapply(ycol, function(j) mean(!is.na(suppressWarnings(as.numeric(sub(",", ".", x[[j]])))))))]
  rf   <- data.frame(date = to_date(x[[dcol]]), yield = suppressWarnings(as.numeric(sub(",", ".", x[[ycol]]))))
  rf   <- rf[!is.na(rf$date) & !is.na(rf$yield), ]
  cat(sprintf("rf: %d days, %s to %s, yield %.2f to %.2f percent\n", nrow(rf), min(rf$date), max(rf$date), min(rf$yield), max(rf$yield)))
  stopifnot(nrow(rf) > 1000, max(rf$date) >= as.Date("2026-03-31"), min(rf$date) <= as.Date("2021-03-30"), all(rf$yield > 0 & rf$yield < 20))
  saveRDS(rf, "data-derived/rf.rds")

  gxl <- "data-raw/borenstein/RetailElasticities2020LinearPathV6.xlsx"
  if (file.exists(gxl)) {
    source("R/01-calibrate/01-calibrate_gamma.R")
    gcal <- calibrate_gamma(gxl, tran_pooling = "as-published")
    saveRDS(gcal, "data-derived/gamma_calibration.rds")
    gate("gamma_hat (as-published pooling)", gcal$gamma_hat, 6.09, 0.05)
  } else message("Borenstein package absent: gamma = 6.1 and 2.8 are used as pinned values (see DATA.md)")
}

# =============================================================================
# STAGE 3 -- solver against the closed form (constant gap, no reserve, no ceiling)
# =============================================================================
if (run_stage(3)) {
  say("STAGE 3  solver check")
  source("R/02-model/solve_reserve.R")
  v <- validate_baseline()
  gate("S0, solver against closed form",  v[["S0_solver"]],  v[["S0_closed_form"]],  1e-3)
  gate("tau, solver against closed form", v[["tau_solver"]], v[["tau_closed_form"]], 5e-3)
}

# =============================================================================
# STAGE 4 -- model, tables and the three main figures (pipeline.R)
# =============================================================================
if (run_stage(4)) {
  say("STAGE 4  model")
  for (f in c("cleanCap", "clean", "A", "floor_schedule", "ceiling_schedule", "rf", "auctions"))
    need(sprintf("data-derived/%s.rds", f), "Run the earlier stages.")
  cleanCap <- readRDS("data-derived/cleanCap.rds"); clean <- readRDS("data-derived/clean.rds")
  A <- readRDS("data-derived/A.rds"); rf <- readRDS("data-derived/rf.rds")
  floor_schedule <- readRDS("data-derived/floor_schedule.rds"); ceiling_schedule <- readRDS("data-derived/ceiling_schedule.rds")
  auc <- readRDS("data-derived/auctions.rds")

  if (QUICK) options(wci.every = 25)
  source("R/03-analysis/pipeline.R")           # prints the PAPER NUMBERS block, leaves `out`
  saveRDS(out, "data-derived/results.rds")

  tol <- if (QUICK) 0.01 else 0.002
  gate("wedge, median, gamma 6.1",  out$tab_wedge["median", "6.1"], 0.1057, tol)
  gate("wedge, median, gamma 2.8",  out$tab_wedge["median", "2.8"], 0.0847, tol)
  gate("wedge, last date, gamma 6.1", out$tab_wedge["last", "6.1"], 0.1400, 0.002)
  gate("wedge, upper quartile, gamma 6.1", out$tab_wedge["q75", "6.1"], 0.1256, tol)
  sc <- out$tab_scen[out$tab_scen$scenario == "realized", ]
  gate("exhaustion, last date, gamma 6.1", sc$tau[sc$gamma == 6.1], 2038, 1)
  gate("exhaustion, last date, gamma 2.8", sc$tau[sc$gamma == 2.8], 2039, 1)
  gate("November 2026 forecast, gamma 6.1", out$tab_post$nov_forecast[out$tab_post$gamma == 6.1], 33.96, 0.02)
}

# =============================================================================
# STAGE 5 -- extra figures and the forecast file
# =============================================================================
# These scripts use the objects stage 4 leaves in the session, so stage 5 cannot be run alone:
# use --stage=4.
if (run_stage(5)) {
  say("STAGE 5  extra figures and forecast")
  if (!exists("out")) stop("Stage 5 needs stage 4 in the same run. Use --stage=4.")
  source("R/04-figures/figures_extra.R")
  source("R/03-analysis/forecast_nov2026.R")
}

say("done. figures in paper/figures/, checkpoints in data-derived/")
