# =============================================================================
# figures_extra.R
# Extra figures for the paper. Run AFTER pipeline.R, in the same session:
#
#   source("pipeline.R"); source("figures_extra.R")
#
# Uses the objects pipeline.R leaves in the workspace (BASE, out, dates, px, CA, QC, ...),
# the auction table `auc`, and figstyle.R. Modifies none of them. Each figure is wrapped in
# try(), so one failure does not stop the others. Files go to CFG$fig_dir.
#
#   1 figure_market.pdf      price, reserve and ceiling, auctions settled at the reserve
#   2 figure_bank.pdf        cap, covered emissions and bank
#   3 figure_premium_ts.pdf  premium over the reserve, auction by auction
#   4 figure_vintage.pdf     advance minus current settlement, percent per year
#   5 figure_layers.pdf      what each layer of the model does to the price path
#   6 figure_placebo.pdf     30-day price changes, all dates against the two events
#   7 figure_postsample.pdf  no-news path against the settlements after the sample
#   8 figure_scenarios.pdf   price and bank under the three emission declines
#   9 figure_volreserve.pdf  realized volatility against the premium over the reserve (new test)
# =============================================================================

source(wci_file("figstyle.R")); dir.create(CFG$fig_dir, recursive = TRUE, showWarnings = FALSE)
FX <- list(
  penalty_bound = 1.6,     # percent per year: lower bound on the advance premium under the penalty models (old paper). CONFIRM.
  p_nov   = NA,            # settlement of the 18 November 2026 auction, once known
  layers_gamma = 6.1,
  ev_window = 30,
  vol_window = 21)
fx_file <- function(name) file.path(CFG$fig_dir, name)
GCOL <- c(PAL$navy, PAL$rust)
glab <- exprvec(lapply(CFG$gammas, function(g) as.expression(bquote(gamma == .(g)))))
cutT <- function(p) p[p$t < max(p$t) - 1e-9, ]     # the path without its last point, at T
zero_fun <- function(t) 0 * t
inf_fun  <- function(t) Inf + 0 * t

# auction table, current and advance legs
AU <- auc[order(auc$date), ]
AU$t        <- dec_year(AU$date)
AU$prem_cur <- AU$settlement_price_cur / AU$auction_reserve_price_cur - 1
AU$prem_adv <- AU$settlement_price_adv / AU$auction_reserve_price_adv - 1
AU$at_res   <- !is.na(AU$prem_cur) & abs(AU$prem_cur) < 1e-9
AU$post     <- AU$date > D_LAST                       # after the sample of the wedge

# ---- 1. market --------------------------------------------------------------------------------
try({
  f <- fx_file("figure_market.pdf"); fig_open(f, 6.8, 4.2)
  tt <- seq(min(AU$t, dec_year(min(dates))), dec_year(max(AU$date, D_LAST)), by = 1 / 52)
  Lh <- ifelse(tt >= 2021, L_fun(tt), NA)             # the ceiling exists from 2021
  yl <- range(c(AU$auction_reserve_price_cur, AU$settlement_price_cur, px, Lh), na.rm = TRUE)
  plot(tt, R_fun(tt), type = "n", log = "y", bty = "n", ylim = yl, xlab = "", ylab = "Price ($/tonne, log scale)")
  fig_frame()
  ar <- AU[!is.na(AU$auction_reserve_price_cur), ]
  Rh <- approx(ar$t, ar$auction_reserve_price_cur, floor(tt) + 0.5, method = "constant", rule = 2)$y   # reserve of the year, from the auction notices
  lines(tt, Rh, col = PAL$ref, lty = 2, lwd = 1); lines(tt, Lh, col = PAL$ref, lty = 2, lwd = 1)
  lines(dec_year(dates), px, col = PAL$navy, lwd = 1.2)
  points(AU$t[!AU$at_res], AU$settlement_price_cur[!AU$at_res], pch = 1, cex = 0.7, col = PAL$ink)
  points(AU$t[AU$at_res],  AU$settlement_price_cur[AU$at_res],  pch = 19, cex = 0.8, col = PAL$rust)
  text(max(tt), tail(Lh, 1), "ceiling", pos = 2, cex = 0.75, col = PAL$sub, offset = 0.3)
  text(max(tt), tail(Rh, 1), "reserve", adj = c(1, 1.6), cex = 0.75, col = PAL$sub)
  fig_legend("topleft", legend = c("Secondary price", "Auction settlement", "Auction settled at the reserve"),
             col = c(PAL$navy, PAL$ink, PAL$rust), lty = c(1, NA, NA), pch = c(NA, 1, 19), lwd = 1.4)
  fig_close(f)
})

# ---- 2. cap, emissions and bank ---------------------------------------------------------------
try({
  f <- fx_file("figure_bank.pdf"); fig_open(f, 6.8, 5.6)
  graphics::par(mfrow = c(2, 1), mar = c(2.4, 4.2, 1.2, 1.2))
  yr  <- 2021:CFG$T_end
  cap <- BASE$c(yr + 0.5) + BASE$q(yr + 0.5)
  em  <- merge(CA[, c("year", "e")], QC[, c("year", "total")], by = "year"); em$wci <- em$e + em$total
  plot(yr, cap, type = "n", bty = "n", xlim = range(c(CA$year, yr)), ylim = c(0, max(CA$e, em$wci, cap, na.rm = TRUE)),
       xlab = "", ylab = "Mt per year")
  fig_frame(); fig_ref(v = dec_year(D_LAST), lty = 3)
  lines(yr, cap, col = PAL$ink, lwd = 1.6, type = "s")
  lines(em$year, em$wci, col = PAL$navy, lwd = 1.6); points(em$year, em$wci, col = PAL$navy, pch = 19, cex = 0.5)
  lines(CA$year, CA$e, col = PAL$steel, lwd = 1.2, lty = 2)
  fig_legend("topright", legend = c("Cap, California and Quebec", "Covered emissions, California and Quebec", "Covered emissions, California"),
             col = c(PAL$ink, PAL$navy, PAL$steel), lty = c(1, 1, 2), lwd = 1.6)
  graphics::par(mar = c(3.4, 4.2, 0.6, 1.2))
  sol <- lapply(CFG$gammas, function(g) solve_v(BASE, D_LAST, tail(out$wedge[[as.character(g)]]$rho, 1), g))
  plot(bank_t, bank_v, type = "n", bty = "n", xlim = range(c(CA$year, yr)), ylim = c(0, max(bank_v, sol[[1]]$path$B, na.rm = TRUE)),
       xlab = "Year", ylab = "Bank (Mt)")
  fig_frame(); fig_ref(v = dec_year(D_LAST), lty = 3)
  for (k in 2:1) lines(sol[[k]]$path$t, sol[[k]]$path$B, col = GCOL[k], lwd = 1.4)
  lines(bank_t, bank_v, col = PAL$ink, lwd = 1.8); points(bank_t, bank_v, col = PAL$ink, pch = 19, cex = 0.5)
  fig_legend("topright", legend = c(expression("Observed"), glab[1], glab[2]), col = c(PAL$ink, GCOL), lwd = 1.6)
  fig_close(f)
})

# ---- 3. premium over the reserve, auction by auction -----------------------------------------
try({
  f <- fx_file("figure_premium_ts.pdf"); fig_open(f, 6.8, 3.8)
  a <- AU[!is.na(AU$prem_cur), ]
  plot(a$t, 100 * a$prem_cur, type = "n", bty = "n", xlab = "", ylab = "Settlement premium over the reserve (%)",
       ylim = c(0, 100 * max(a$prem_cur) * 1.05))
  fig_frame(hgrid = pretty(100 * a$prem_cur)); fig_ref(v = dec_year(CFG$events), lty = 3)
  segments(a$t, 0, a$t, 100 * a$prem_cur, col = ifelse(a$at_res, PAL$rust, PAL$navy), lwd = 2.2)
  points(a$t[a$at_res], rep(0, sum(a$at_res)), pch = 19, col = PAL$rust, cex = 0.9)
  fig_legend("topleft", legend = c("Settled above the reserve", "Settled at the reserve"),
             col = c(PAL$navy, PAL$rust), lty = c(1, NA), pch = c(NA, 19), lwd = 2.2)
  fig_close(f)
  cat(sprintf("\n[premium] %d auctions with a current-vintage leg, %d at the reserve exactly. Maximum premium %.0f%%.\n",
              nrow(a), sum(a$at_res), 100 * max(a$prem_cur)))
})

# ---- 4. vintage curve: advance minus current, percent per year --------------------------------
try({
  v <- AU[!is.na(AU$settlement_price_cur) & !is.na(AU$settlement_price_adv) & !is.na(AU$advance_vintage), ]
  v <- v[v$prem_cur > 1e-9 & v$prem_adv > 1e-9, ]                      # both legs cleared above the reserve
  v$years  <- v$advance_vintage - as.numeric(format(v$date, "%Y"))
  v$spread <- 100 * log(v$settlement_price_adv / v$settlement_price_cur) / v$years
  ins <- v[!v$post, ]
  mu <- mean(ins$spread); se <- sd(ins$spread) / sqrt(nrow(ins))
  f <- fx_file("figure_vintage.pdf"); fig_open(f, 6.8, 3.8)
  yl <- range(c(v$spread, FX$penalty_bound, mu - 2 * se, mu + 2 * se)) + c(-0.3, 0.3)
  plot(v$t, v$spread, type = "n", bty = "n", ylim = yl, xlab = "", ylab = "Advance minus current (% per year)")
  graphics::rect(min(v$t) - 1, mu - 1.96 * se, max(v$t) + 1, mu + 1.96 * se, col = col_alpha(PAL$band, 0.12), border = NA)
  fig_frame(); fig_ref(h = 0, lty = 1); fig_ref(h = FX$penalty_bound, lty = 2, col = PAL$rust)
  graphics::abline(h = mu, col = PAL$navy, lwd = 1.2)
  points(ins$t, ins$spread, pch = 19, cex = 0.8, col = PAL$navy)
  if (any(v$post)) points(v$t[v$post], v$spread[v$post], pch = 21, cex = 1.1, col = PAL$rust, bg = "white", lwd = 1.6)
  text(min(v$t), FX$penalty_bound, "lower bound under the penalty models", adj = c(0, -0.5), cex = 0.75, col = PAL$rust)
  fig_legend("bottomleft", legend = c("Auction, in sample", "Auction, after the sample", "Mean and 95% interval"),
             col = c(PAL$navy, PAL$rust, PAL$navy), pch = c(19, 21, NA), lty = c(NA, NA, 1), lwd = 1.2)
  fig_close(f)
  cat(sprintf("[vintage] %d auctions above the reserve on both legs in sample, mean spread %.3f%% per year, plain s.e. %.3f (the paper reports a HAC s.e.). After the sample: %s\n",
              nrow(ins), mu, se, paste(sprintf("%s %.2f", format(v$date[v$post]), v$spread[v$post]), collapse = ", ")))
})

# ---- 5. what each layer does -------------------------------------------------------------------
try({
  g0 <- FX$layers_gamma; gm <- g0 * BASE$gscale; t0 <- dec_year(D_LAST)
  B0 <- bank_of(BASE, gm)(t0); yf <- y_of(BASE, gm)
  rho_full <- tail(out$wedge[[as.character(g0)]]$rho, 1); r0 <- r_on(D_LAST)
  lay <- function(rho, R, L, tiers) tryCatch(suppressWarnings(
    solve_path(B0 = B0, t0 = t0, T = CFG$T_end, c_fun = BASE$c, q_fun = BASE$q, y_fun = yf, R_fun = R, L_fun = L,
               gamma = gm, rho = rho, dt = 1 / 52, tiers = tiers)), error = function(e) { message("layer failed: ", conditionMessage(e)); NULL })
  LAY <- list(
    "Banking only, price grows at r"     = lay(r0,       zero_fun, inf_fun, NULL),
    "With program risk"                  = lay(rho_full, zero_fun, inf_fun, NULL),
    "With the ceiling"                   = lay(rho_full, zero_fun, L_fun,   NULL),
    "With the reserve tiers and the auction reserve" = lay(rho_full, R_fun, L_fun, TIERS))
  LAY <- LAY[!sapply(LAY, is.null)]
  lc <- c(PAL$ref, PAL$gold, PAL$teal, PAL$navy)[seq_along(LAY)]
  f <- fx_file("figure_layers.pdf"); fig_open(f, 6.8, 4.4)
  tt <- LAY[[1]]$path$t
  yl <- range(c(P_LAST, R_fun(tt), unlist(lapply(LAY, function(s) { z <- cutT(s$path)$S; z[z > 0] }))), na.rm = TRUE)
  plot(tt, L_fun(tt), type = "n", log = "y", bty = "n", ylim = yl, xlab = "Year", ylab = "Price ($/tonne, log scale)")
  fig_frame(); for (z in list(L_fun(tt), R_fun(tt))) lines(tt, z, col = PAL$ref, lty = 2, lwd = 0.9)
  for (k in seq_along(LAY)) lines(cutT(LAY[[k]]$path)$t, pmax(cutT(LAY[[k]]$path)$S, 1e-6), col = lc[k], lwd = if (k == length(LAY)) 2 else 1.5)
  points(t0, P_LAST, pch = 19, col = PAL$ink, cex = 0.9); text(t0, P_LAST, "observed price", pos = 4, cex = 0.75, col = PAL$ink)
  fig_legend("bottomright", legend = sprintf("%s ($%.0f today)", names(LAY), sapply(LAY, function(s) s$S0)), col = lc, lwd = 1.8)
  fig_close(f)
  cat("[layers] price on the last date and exhaustion date, layer by layer:\n")
  print(data.frame(layer = names(LAY), S0 = round(sapply(LAY, function(s) s$S0), 2), tau = round(sapply(LAY, function(s) s$tau), 1),
                   regime = sapply(LAY, function(s) s$regime)), row.names = FALSE)
})

# ---- 6. placebo distribution of price changes -------------------------------------------------
try({
  win <- FX$ev_window; lp <- log(px)
  cand <- dates[dates >= min(dates) + win & dates <= max(dates) - win]
  plc  <- 100 * sapply(cand, function(z) ev_delta(dates, lp, z, win))
  evd  <- 100 * sapply(CFG$events, function(z) ev_delta(dates, lp, z, win))
  f <- fx_file("figure_placebo.pdf"); fig_open(f, 6.8, 3.8)
  h <- hist(plc, breaks = 40, plot = FALSE)
  plot(h, col = col_alpha(PAL$navy, 0.35), border = "white", main = "", xlab = sprintf("Change in the log price over %d days before and after (%%)", win),
       ylab = "Number of dates", xlim = range(c(plc, evd), na.rm = TRUE) * 1.08)
  fig_frame(); graphics::abline(v = evd, col = PAL$rust, lwd = 1.8)
  fig_ref(v = quantile(plc, c(0.05, 0.95), na.rm = TRUE), lty = 3)
  text(evd, max(h$counts) * c(0.95, 0.85), c("April 2025", "September 2025"), pos = c(4, 2), cex = 0.78, col = PAL$rust)
  fig_close(f)
})

# ---- 7. the price after the sample ------------------------------------------------------------
try({
  g0 <- CFG$gammas[1]; po <- out$tab_post[out$tab_post$gamma == g0, ]
  tl <- dec_year(D_LAST); ta <- dec_year(CFG$d_aug); tn <- dec_year(CFG$d_nov)
  keep <- dates >= D_LAST - 365
  t1 <- seq(tl, ta, length.out = 50); t2 <- seq(ta, tn, length.out = 30)
  path1 <- P_LAST * exp(po$rho_last * (t1 - tl))                                  # secondary price, no news
  path2 <- CFG$p_aug * exp(po$rho_aug_settlement * (t2 - ta))                     # settlement to settlement
  f <- fx_file("figure_postsample.pdf"); fig_open(f, 6.8, 3.8)
  tt <- seq(dec_year(min(dates[keep])), tn + 0.05, by = 1 / 52)
  yl <- range(c(px[keep], R_fun(tt), path1, path2, CFG$p_aug, po$aug_secondary_equiv, FX$p_nov), na.rm = TRUE); yl[2] <- yl[2] + 0.35 * diff(yl)
  plot(dec_year(dates[keep]), px[keep], type = "n", bty = "n", xlim = range(tt), ylim = yl, xlab = "", ylab = "Price ($/tonne)", xaxt = "n")
  qd <- seq(as.Date(format(min(dates[keep]), "%Y-%m-01")), CFG$d_nov + 31, by = "3 months")
  graphics::axis(1, at = dec_year(qd), labels = format(qd, "%b %Y"))
  fig_frame(); lines(tt, R_fun(tt), col = PAL$ref, lty = 2, lwd = 1); fig_ref(v = tl, lty = 3)
  lines(dec_year(dates[keep]), px[keep], col = PAL$navy, lwd = 1.3)
  lines(t1, path1, col = PAL$navy, lwd = 1.3, lty = 2); lines(t2, path2, col = PAL$rust, lwd = 1.3, lty = 2)
  points(ta, CFG$p_aug, pch = 19, col = PAL$rust, cex = 1); points(ta, po$aug_secondary_equiv, pch = 1, col = PAL$navy, cex = 1)
  points(tn, po$nov_forecast, pch = 4, col = PAL$rust, cex = 1.1, lwd = 1.6)
  if (!is.na(FX$p_nov)) points(tn, FX$p_nov, pch = 19, col = PAL$rust, cex = 1)
  text(max(tt), R_fun(max(tt)), "reserve", adj = c(1, 1.6), cex = 0.75, col = PAL$sub)
  fig_legend("topleft", legend = c("Secondary price and its no-news path", "Auction settlement and its no-news path",
                                   "August settlement at the secondary-price equivalent", "Forecast for 18 November 2026"),
             col = c(PAL$navy, PAL$rust, PAL$navy, PAL$rust), lty = c(1, 2, NA, NA), pch = c(NA, 19, 1, 4), lwd = 1.3)
  fig_close(f)
})

# ---- 8. scenarios -----------------------------------------------------------------------------
try({
  scen <- c(CFG$scen_gy[["slow"]], BASE$gy, CFG$scen_gy[["fast"]])
  slab <- sprintf("%.1f%% per year", -100 * scen); scol <- c(PAL$rust, PAL$navy, PAL$teal)
  SOL <- lapply(CFG$gammas, function(g) lapply(scen, function(gy) {
    rho <- fit_rho(BASE, D_LAST, P_LAST, g, gy)
    if (!is.na(rho) && rho <= CFG$g_R) cat(sprintf("[scenarios] gamma %.1f, decline %.1f%%: fitted rho %.3f is BELOW the escalator %.3f. The assumption rho > g_R fails in this cell.\n", g, -100 * gy, rho, CFG$g_R))
    if (is.na(rho)) NULL else solve_v(BASE, D_LAST, rho, g, gy) }))
  f <- fx_file("figure_scenarios.pdf"); fig_open(f, 6.8, 6.4)
  graphics::par(mfrow = c(2, 1), mar = c(2.6, 4.2, 1.6, 1.2))
  tt <- seq(dec_year(D_LAST), CFG$T_end, by = 1 / 52)
  allS <- unlist(lapply(SOL, function(a) lapply(a, function(s) if (!is.null(s)) cutT(s$path)$S)))
  plot(tt, L_fun(tt), type = "n", log = "y", bty = "n", ylim = range(c(R_fun(tt), L_fun(tt), allS[allS > 0])), xlab = "", ylab = "Price ($/tonne, log scale)")
  fig_frame(); for (z in list(L_fun(tt), R_fun(tt))) lines(tt, z, col = PAL$ref, lty = 2, lwd = 0.9)
  for (k in seq_along(CFG$gammas)) for (j in seq_along(scen)) if (!is.null(SOL[[k]][[j]]))
    lines(cutT(SOL[[k]][[j]]$path)$t, cutT(SOL[[k]][[j]]$path)$S, col = scol[j], lwd = 1.6, lty = k)
  fig_legend("topleft", legend = exprvec(c(lapply(slab, as.expression), list(glab[1], glab[2]))), col = c(scol, PAL$ink, PAL$ink), lty = c(1, 1, 1, 1, 2), lwd = 1.6)
  graphics::par(mar = c(3.6, 4.2, 0.6, 1.2))
  allB <- unlist(lapply(SOL, function(a) lapply(a, function(s) if (!is.null(s)) s$path$B)))
  plot(tt, tt, type = "n", bty = "n", ylim = c(0, max(allB)), xlab = "Year", ylab = "Bank (Mt)")
  fig_frame()
  for (k in seq_along(CFG$gammas)) for (j in seq_along(scen)) if (!is.null(SOL[[k]][[j]]))
    lines(SOL[[k]][[j]]$path$t, SOL[[k]][[j]]$path$B, col = scol[j], lwd = 1.6, lty = k)
  fig_close(f)
})

# ---- 9. new test: volatility against the distance to the reserve ------------------------------
# The inversion curve is flat near the reserve, so the model says the price reacts less to shocks
# there: realized volatility should be lower when the premium over the reserve is small.
try({
  ret <- c(NA, diff(log(px))); mon <- format(dates, "%Y-%m")
  M <- do.call(rbind, lapply(split(seq_along(dates), mon), function(i) if (sum(!is.na(ret[i])) >= 15) data.frame(
    month = mon[i[1]], vol = sd(ret[i], na.rm = TRUE) * sqrt(252), prem = mean(log(px[i] / R_fun(dec_year(dates[i])))), price = mean(px[i])) else NULL))
  M$event <- M$month %in% format(CFG$events, "%Y-%m")
  ct  <- suppressWarnings(cor.test(M$prem, M$vol, method = "spearman"))
  ct2 <- suppressWarnings(cor.test(M$prem[!M$event], M$vol[!M$event], method = "spearman"))
  ctp <- suppressWarnings(cor.test(M$price, M$vol, method = "spearman"))
  f <- fx_file("figure_volreserve.pdf"); fig_open(f, 6.8, 3.8)
  plot(100 * M$prem, M$vol, type = "n", bty = "n", xlab = "Log premium of the price over the reserve, monthly mean (%)", ylab = "Realized volatility, annualized")
  fig_frame(hgrid = pretty(M$vol))
  points(100 * M$prem[!M$event], M$vol[!M$event], pch = 19, col = col_alpha(PAL$navy, 0.7), cex = 0.9)
  points(100 * M$prem[M$event], M$vol[M$event], pch = 19, col = PAL$rust, cex = 1.1)
  lo <- lowess(100 * M$prem, M$vol, f = 0.8); lines(lo, col = PAL$ink, lwd = 1.2)
  fig_legend("topleft", legend = c("Month", "Month of a program event"), col = c(PAL$navy, PAL$rust), pch = 19, lty = NA)
  fig_close(f)
  cat(sprintf("[volatility] %d months. Rank correlation of volatility with the premium over the reserve: %.3f (p = %.3f). Without the two event months: %.3f (p = %.3f). With the price level, for comparison: %.3f (p = %.3f).\n",
              nrow(M), ct$estimate, ct$p.value, ct2$estimate, ct2$p.value, ctp$estimate, ctp$p.value))
  cat("             The model predicts a positive correlation with the premium. A negative or zero one is evidence against it.\n")
})
