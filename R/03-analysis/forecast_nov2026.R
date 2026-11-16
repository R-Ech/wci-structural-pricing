# forecast_nov2026.R : run after pipeline.R, in the same session. Writes forecast_2026-11-18.txt.
fc  <- out$tab_post
ta  <- dec_year(CFG$d_aug); tn <- dec_year(CFG$d_nov)
a   <- auc[order(auc$date), ]; a <- a[a$date >= as.Date("2021-01-01") & !is.na(a$settlement_price_cur), ]
sdq <- sd(diff(log(a$settlement_price_cur)))     # quarterly log change of the settlement since 2021
md5 <- function(f) { p <- tryCatch(wci_file(f), error = function(e) NA); if (is.na(p)) "not found" else unname(tools::md5sum(p)) }
txt <- c(
  "Forecast of the WCI joint auction settlement of 18 November 2026 (current vintage)",
  sprintf("Written on %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  "",
  "Rule: without news the price grows at the required return rho. The forecast is the August 2026",
  "settlement grown at the rho fitted to that settlement, and not below the reserve.",
  sprintf("August settlement: $%.2f on %s. Reserve in November: $%.2f.", CFG$p_aug, format(CFG$d_aug), R_fun(tn)),
  "",
  sprintf("gamma = %.1f: rho = %.4f, forecast $%.2f", fc$gamma, fc$rho_aug_settlement, fc$nov_forecast),
  "",
  sprintf("Yardstick: the standard deviation of the quarterly log change of the settlement since 2021 is %.3f,", sdq),
  sprintf("so one standard deviation around the gamma = %.1f forecast is $%.2f to $%.2f.",
          fc$gamma[1], fc$nov_forecast[1] * exp(-sdq), fc$nov_forecast[1] * exp(sdq)),
  sprintf("No-change benchmark: $%.2f.", CFG$p_aug),
  "",
  "Model version: WCI-wide cap and emissions (California and Quebec), gamma scaled by California's share,",
  "two reserve tiers, 10-year Treasury yield as the risk-free rate. Sample of the wedge: 30 March 2021 to 31 March 2026.",
  "An earlier forecast of $33.76 at gamma = 6.1 was posted in the record description on 11 November 2026,",
  "from the previous version of the model.",
  "",
  sprintf("md5 pipeline.R       %s", md5("pipeline.R")),
  sprintf("md5 solve_reserve.R  %s", md5("solve_reserve.R")))
writeLines(txt, "forecast_2026-11-18.txt"); cat(txt, sep = "\n")
