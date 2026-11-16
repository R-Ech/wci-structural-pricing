# =============================================================================
# pipeline.R
# One entry point for every number and figure of the paper.
# Baseline: WCI-wide inputs (California + Quebec), gamma scaled to the WCI, two reserve
# tiers, risk-free rate = 10-year Treasury yield on each date.
#
# Needs in the workspace (raw objects, none is modified):
#   cleanCap   yr, Auctions, Allocations, Emissions, Bank      (any units, detected below)
#   clean      date, price
#   A          Year, Budget                                    (California budgets, Mt)
#   floor_schedule (start, floor), ceiling_schedule (start, ceiling)
#   rf         date, yield (percent). Optional; without it r is constant.
# Needs on disk: solve_reserve.R (with `tiers`), and figstyle.R for the figures.
#
#   source("pipeline.R")            # runs everything, prints the PAPER NUMBERS block
# Results are stored in the list `out`. Nothing else is created except functions and constants.
# =============================================================================

# finds a script next to this one or in the repository folders (replication package layout)
wci_file <- function(f) { p <- c(f, file.path("R/02-model", f), file.path("R/03-analysis", f), file.path("R/04-figures", f))
  p <- p[file.exists(p)]; if (!length(p)) stop("cannot find ", f); p[1] }
source(wci_file("solve_reserve.R"))

# ---- 0. configuration ---------------------------------------------------------------------------
CFG <- list(
  gammas   = c(6.1, 2.8),          # as calibrated on California
  r        = 0.04,                 # used only when rf is not loaded
  g_R      = 0.075,                # escalator after the last scheduled year (5% + 2.5% CPI)
  T_end    = 2045,
  every    = getOption("wci.every", 1),   # 1 = all dates. options(wci.every = 25) for a quick pass.
  offsets  = 0,                    # Mt per year of offsets in the bank flow after 2024
  discount = 0,                    # auction sells only if price >= (1 + discount) * reserve. 0.047 = observed auction discount
  events   = as.Date(c(april_2025 = "2025-04-08", sept_2025 = "2025-09-19")),
  reserve_auctions = as.Date(c("2025-05-21", "2026-02-18")),
  d_aug = as.Date("2026-08-19"), p_aug = 32.48, d_nov = as.Date("2026-11-18"),
  auction_discount = 0.047,        # median discount of the auction settlement to the secondary price
  scen_gy  = c(slow = -0.020, fast = -0.045),
  figures  = TRUE, fig_dir = "paper/figures")

# ---- 1. inputs ----------------------------------------------------------------------------------
dec_year <- function(d) { d <- as.Date(d); y <- as.numeric(format(d, "%Y"))
  d0 <- as.Date(paste0(y, "-01-01")); d1 <- as.Date(paste0(y + 1, "-01-01"))
  y + as.numeric(d - d0) / as.numeric(d1 - d0) }
to_Mt <- function(x) if (median(x, na.rm = TRUE) > 1e4) x / 1e6 else x

CA <- data.frame(year = cleanCap$yr, e = to_Mt(cleanCap$Emissions), auc = to_Mt(cleanCap$Auctions),
                 alloc = to_Mt(cleanCap$Allocations), bank = to_Mt(cleanCap$Bank))
last3   <- tail(order(CA$year), 3)
share_q <- sum(CA$auc[last3]) / sum(CA$auc[last3] + CA$alloc[last3])

# Quebec covered emissions (Mt), liste-etablissements-visesRSPEDE.xlsx, update of 1 October 2026.
# emit_const = the 66 establishments covered in every year 2015-2024.
QC <- data.frame(year = 2015:2025,
  emit       = c(18.200, 17.777, 18.370, 17.389, 18.969, 18.543, 19.832, 19.794, 19.735, 19.218, 18.945),
  emit_const = c(17.918, 17.529, 18.023, 17.091, 17.165, 16.507, 17.282, 17.256, 17.234, 16.839, 16.488),
  dist       = c(39.395, 40.600, 41.854, 42.963, 42.777, 37.598, 38.917, 40.211, 38.061, 37.805, 38.261))
QC$total <- QC$emit + QC$dist; QC$total_const <- QC$emit_const + QC$dist
# Quebec caps: decree for 2021-2030, same linear decline assumed after 2030.
qc_cap_fun <- approxfun(2021:2045 + 0.5, pmax(55.26 - (55.26 - 44.14) / 9 * (0:24), 0), rule = 2)

trend <- function(year, e) { k <- year >= 2015 & year <= 2024 & year != 2020; unname(coef(lm(log(e[k]) ~ year[k]))[2]) }
m <- merge(CA, QC, by = "year")
IN <- list(
  gy_ca = trend(m$year, m$e), gy_qc = trend(m$year, m$total_const), gy_wci = trend(m$year, m$e + m$total_const),
  e24_ca = CA$e[CA$year == 2024], e24_wci = CA$e[CA$year == 2024] + QC$total[QC$year == 2024],
  P_2024 = mean(clean$price[format(as.Date(clean$date), "%Y") == "2024"], na.rm = TRUE))
IN$ca_share <- IN$e24_ca / IN$e24_wci

c_ca <- approxfun(A$Year + 0.5, (1 - share_q) * A$Budget, rule = 2)
q_ca <- approxfun(A$Year + 0.5, share_q * A$Budget,       rule = 2)

extend_schedule <- function(start, value, g) {
  yr <- as.numeric(format(as.Date(start), "%Y")); o <- order(yr); yr <- yr[o]; value <- value[o]
  last <- max(yr); v_last <- value[length(value)]
  step <- approxfun(yr, value, method = "constant", rule = 2)
  function(t) ifelse(t < last + 1, step(t), v_last * (1 + g)^(t - last))
}
R_fun <- extend_schedule(floor_schedule$start,   floor_schedule$floor,     CFG$g_R)
L_fun <- extend_schedule(ceiling_schedule$start, ceiling_schedule$ceiling, CFG$g_R)
R_2026 <- R_fun(2026.5)
TIERS  <- list(K = list(function(t) 65.31 / R_2026 * R_fun(t), function(t) 83.92 / R_2026 * R_fun(t)),
               V = c(66.811, 89.537))
bank_t <- CA$year + 1; bank_v <- CA$bank          # end of year = start of the next

r_on <- function(d) {
  if (!(exists("rf") && is.data.frame(rf))) return(rep(CFG$r, length(d)))
  x <- rf[!is.na(rf$yield), ]; x <- x[order(as.Date(x$date)), ]
  approx(as.numeric(as.Date(x$date)), x$yield / 100, as.numeric(as.Date(d)), method = "constant", rule = 2)$y
}

# ---- 2. variants --------------------------------------------------------------------------------
# scope "WCI" or "CA"; scaled = gamma multiplied by California's share; tiers on or off.
variant <- function(scope = "WCI", scaled = TRUE, tiers = TRUE) {
  wci <- scope == "WCI"
  list(name = paste0(scope, if (wci && scaled) "_g" else "", if (!tiers) "_notiers" else ""),
       c = if (wci) function(t) c_ca(t) + (1 - share_q) * qc_cap_fun(t) else c_ca,
       q = if (wci) function(t) q_ca(t) + share_q * qc_cap_fun(t) else q_ca,
       e24 = if (wci) IN$e24_wci else IN$e24_ca, gy = if (wci) IN$gy_wci else IN$gy_ca,
       gscale = if (wci && scaled) IN$ca_share else 1, tiers = if (tiers) TIERS else NULL,
       cache = new.env())
}
y_of <- function(v, gm, gy = v$gy, shift = 0) { y0 <- v$e24 + IN$P_2024 / gm; function(t) y0 * exp(gy * (t - 2024)) + shift }

bank_of <- function(v, gm, gy = v$gy) {             # CIR bank through end-2024, model flow after
  key <- paste(gm, gy)
  if (!is.null(v$cache[[key]])) return(v$cache[[key]])
  t  <- dec_year(clean$date); ok <- t > 2025 & !is.na(clean$price)
  flow <- v$c(t) + v$q(t) * (clean$price >= (1 + CFG$discount) * R_fun(t)) - y_of(v, gm, gy)(t) + clean$price / gm + CFG$offsets
  B25  <- approx(bank_t, bank_v, 2025, rule = 2)$y; keep <- bank_t < 2025
  tt   <- c(2025, t[ok]); ff <- c(flow[ok][1], flow[ok])
  f    <- approxfun(c(bank_t[keep], tt), c(bank_v[keep], B25 + cumtrapz(tt, ff)), rule = 2)
  v$cache[[key]] <- f; f
}
# gm0 is the California value (6.1 or 2.8); the solver uses gm0 * v$gscale.
solve_v <- function(v, d, rho, gm0, gy = v$gy, B0 = NULL, shift = 0, after = TRUE) {
  gm <- gm0 * v$gscale; t0 <- dec_year(d)
  if (is.null(B0)) B0 <- bank_of(v, gm, gy)(t0)
  suppressWarnings(solve_path(B0 = B0, t0 = t0, T = CFG$T_end, c_fun = v$c, q_fun = v$q,
    y_fun = y_of(v, gm, gy, shift), R_fun = R_fun, L_fun = L_fun, gamma = gm, rho = rho, dt = 1 / 52, tiers = v$tiers, after = after))
}
fit_rho <- function(v, d, P, gm0, gy = v$gy, B0 = NULL)
  tryCatch(uniroot(function(x) solve_v(v, d, x, gm0, gy, B0, after = FALSE)$S0 - P, c(0.01, 0.6))$root, error = function(e) NA_real_)
fit_row <- function(v, d, P, gm0, gy = v$gy) {
  rho <- fit_rho(v, d, P, gm0, gy)
  if (is.na(rho)) return(data.frame(rho = NA, tau = NA, tier1 = NA, tier2 = NA, regime = NA, tau_final = NA, regime_final = NA, flags = "no root"))
  s <- solve_v(v, d, rho, gm0, gy); tt <- if (length(s$t_tier) == 2) s$t_tier else c(NA, NA)
  data.frame(rho = rho, tau = s$tau, tier1 = tt[1], tier2 = tt[2], regime = s$regime, tau_final = s$tau_final, regime_final = s$regime_final, flags = paste(s$flags, collapse = " | "))
}

# ---- 3. run ---------------------------------------------------------------------------------------
BASE  <- variant("WCI", scaled = TRUE, tiers = TRUE)
ok    <- !is.na(clean$price); dates <- as.Date(clean$date[ok]); px <- clean$price[ok]
idx   <- unique(c(seq(1, length(dates), by = CFG$every), length(dates)))
D_LAST <- max(dates); P_LAST <- px[which.max(dates)]
out   <- list(inputs = IN, share_q = share_q)

# 3a. daily wedge, baseline, both gammas
wedge_daily <- function(v, gm0) {
  w <- do.call(rbind, lapply(idx, function(i) { if (i %% 200 == 0) cat(".")
    cbind(date = dates[i], price = px[i], fit_row(v, dates[i], px[i], gm0)) }))
  w$r <- r_on(w$date); w$wedge <- w$rho - w$r; w
}
cat("daily wedge")
out$wedge <- lapply(CFG$gammas, function(g) wedge_daily(BASE, g)); names(out$wedge) <- as.character(CFG$gammas)
cat(" done\n")
summ <- function(w) c(median = median(w$wedge, na.rm = TRUE), q25 = unname(quantile(w$wedge, .25, na.rm = TRUE)),
  q75 = unname(quantile(w$wedge, .75, na.rm = TRUE)), min = min(w$wedge, na.rm = TRUE), max = max(w$wedge, na.rm = TRUE),
  last = tail(w$wedge, 1), rho_min = min(w$rho, na.rm = TRUE), tau_min = min(w$tau, na.rm = TRUE),
  tau_median = median(w$tau, na.rm = TRUE), tau_max = max(w$tau, na.rm = TRUE),
  n = nrow(w), n_NA = sum(is.na(w$rho)), horizon = sum(w$regime == "bank lasts until T (horizon binds)", na.rm = TRUE),
  near_horizon = sum(w$tau > CFG$T_end - 0.5, na.rm = TRUE), ceiling = sum(w$regime == "ceiling binds at exhaustion", na.rm = TRUE),
  at_tier = sum(grepl("reserve tier", w$regime)), tau_final_min = min(w$tau_final, na.rm = TRUE), tau_final_max = max(w$tau_final, na.rm = TRUE),
  final_ceiling = sum(w$regime_final == "ceiling binds at exhaustion", na.rm = TRUE))
out$tab_wedge <- round(sapply(out$wedge, summ), 4)

# 3b. scenarios on the last date
scen <- c(slow = CFG$scen_gy[["slow"]], realized = BASE$gy, fast = CFG$scen_gy[["fast"]])
out$tab_scen <- do.call(rbind, lapply(CFG$gammas, function(g) do.call(rbind, lapply(names(scen), function(s) {
  f <- fit_row(BASE, D_LAST, P_LAST, g, scen[[s]])
  data.frame(gamma = g, scenario = s, g_y = round(scen[[s]], 4), wedge = f$rho - r_on(D_LAST), tier1 = f$tier1,
             tier2 = f$tier2, tau = f$tau, regime = f$regime, tau_final = f$tau_final, regime_final = f$regime_final) }))))

# 3c. robustness on the last date: scope, gamma scaling, tiers
VARS <- list(BASE, variant("WCI", FALSE, TRUE), variant("CA", FALSE, TRUE), variant("WCI", TRUE, FALSE))
out$tab_robust <- do.call(rbind, lapply(VARS, function(v) do.call(rbind, lapply(CFG$gammas, function(g) {
  f <- fit_row(v, D_LAST, P_LAST, g)
  data.frame(variant = v$name, gamma = g, wedge = f$rho - r_on(D_LAST), tau = f$tau, regime = f$regime, tau_final = f$tau_final, regime_final = f$regime_final) }))))

# 3d. censoring: required return at which the model price equals the reserve, on the reserve auctions
out$tab_censor <- do.call(rbind, lapply(CFG$gammas, function(g) do.call(rbind, lapply(CFG$reserve_auctions, function(d) {
  i <- which.min(abs(dates - d)); Rd <- R_fun(dec_year(d))
  data.frame(gamma = g, auction = d, reserve = Rd, secondary = px[i], pct_above = 100 * (px[i] / Rd - 1),
             rho_from_price = fit_rho(BASE, dates[i], px[i], g), rho_at_reserve = fit_rho(BASE, dates[i], Rd, g)) }))))

# 3e. after the sample
out$tab_post <- do.call(rbind, lapply(CFG$gammas, function(g) {
  rho0 <- fit_rho(BASE, D_LAST, P_LAST, g); s0 <- solve_v(BASE, D_LAST, rho0, g)
  tl <- dec_year(D_LAST); ta <- dec_year(CFG$d_aug); tn <- dec_year(CFG$d_nov)
  B_aug <- approx(s0$path$t, s0$path$B, ta)$y
  sec_aug <- CFG$p_aug / (1 - CFG$auction_discount)             # secondary-equivalent of the settlement
  data.frame(gamma = g, rho_last = rho0, aug_no_news_secondary = P_LAST * exp(rho0 * (ta - tl)),
    aug_settlement = CFG$p_aug, aug_secondary_equiv = sec_aug,
    pct_above_no_news = 100 * (sec_aug / (P_LAST * exp(rho0 * (ta - tl))) - 1),
    rho_aug_settlement = fit_rho(BASE, CFG$d_aug, CFG$p_aug, g, B0 = B_aug),
    rho_aug_secondary  = fit_rho(BASE, CFG$d_aug, sec_aug, g, B0 = B_aug),
    nov_forecast = { ra <- fit_rho(BASE, CFG$d_aug, CFG$p_aug, g, B0 = B_aug); max(CFG$p_aug * exp(ra * (tn - ta)), R_fun(tn)) }) }))

# 3f. event windows, on the log price and on the wedge
ev_delta <- function(dd, x, d, win) { pre <- x[dd >= d - win & dd < d]; pst <- x[dd > d & dd <= d + win]
  if (length(pre) < 5 || length(pst) < 5) return(NA_real_); mean(pst, na.rm = TRUE) - mean(pre, na.rm = TRUE) }
ev_study <- function(dd, x, label) do.call(rbind, lapply(c(15, 30, 60), function(win) {
  cand <- dd[dd >= min(dd) + win & dd <= max(dd) - win]; plc <- sapply(cand, function(z) ev_delta(dd, x, z, win))
  do.call(rbind, lapply(seq_along(CFG$events), function(k) { del <- ev_delta(dd, x, CFG$events[k], win)
    data.frame(series = label, event = names(CFG$events)[k], window = win, delta = del,
               percentile = 100 * mean(plc <= del, na.rm = TRUE), n_placebo = sum(!is.na(plc))) })) }))
out$tab_events <- rbind(ev_study(dates, log(px), "log price"),
  do.call(rbind, lapply(names(out$wedge), function(g) ev_study(out$wedge[[g]]$date, out$wedge[[g]]$wedge, paste("wedge, gamma", g)))))

# ---- 4. figures -----------------------------------------------------------------------------------
if (CFG$figures) {
  source(wci_file("figstyle.R")); dir.create(CFG$fig_dir, recursive = TRUE, showWarnings = FALSE)
  GCOL <- c(PAL$navy, PAL$rust); glab <- exprvec(lapply(CFG$gammas, function(g) as.expression(bquote(gamma == .(g)))))
  w1 <- out$wedge[[1]]; w2 <- out$wedge[[2]]
  # wedge series
  f <- file.path(CFG$fig_dir, "figure_wedge.pdf"); fig_open(f, 6.8, 4.2)
  yl <- range(c(w1$wedge, w2$wedge), na.rm = TRUE)
  plot(w1$date, w1$wedge, type = "n", ylim = yl, bty = "n", xlab = "", ylab = lab_lambdaell())
  fig_frame(hgrid = pretty(yl)); fig_ref(v = CFG$events, lty = 3)
  lines(w2$date, w2$wedge, col = GCOL[2], lwd = 1.4); lines(w1$date, w1$wedge, col = GCOL[1], lwd = 1.4)
  fig_legend("topleft", legend = glab, col = GCOL, lwd = 1.8); fig_close(f)
  # paths from the last date
  sol <- lapply(seq_along(CFG$gammas), function(k) solve_v(BASE, D_LAST, tail(out$wedge[[k]]$rho, 1), CFG$gammas[k]))
  p1 <- sol[[1]]$path; K1 <- TIERS$K[[1]](p1$t); K2 <- TIERS$K[[2]](p1$t)
  f <- file.path(CFG$fig_dir, "figure_paths.pdf"); fig_open(f, 6.8, 6.4)
  graphics::par(mfrow = c(2, 1), mar = c(2.6, 4.2, 1.6, 1.2))
  plot(p1$t, p1$L, type = "n", log = "y", bty = "n", xlab = "", ylab = "Price ($/tonne, log scale)",
       ylim = range(c(p1$R, p1$L, sol[[1]]$path$S, sol[[2]]$path$S)))
  fig_frame(); for (z in list(p1$L, p1$R)) lines(p1$t, z, col = PAL$ref, lty = 2, lwd = 1)
  for (z in list(K1, K2)) lines(p1$t, z, col = PAL$ref, lty = 3, lwd = 1)
  for (k in 1:2) { lines(sol[[k]]$path$t, sol[[k]]$path$S, col = GCOL[k], lwd = 1.8); fig_ref(v = sol[[k]]$tau, lty = 3, col = GCOL[k]) }
  text(max(p1$t), tail(p1$L, 1), "ceiling", pos = 2, cex = 0.75, col = PAL$sub, offset = 0.3)
  text(max(p1$t), tail(K1, 1), "tiers", pos = 2, cex = 0.75, col = PAL$sub, offset = 0.3)
  text(max(p1$t), tail(p1$R, 1), "reserve", pos = 2, cex = 0.75, col = PAL$sub, offset = 0.3)
  fig_legend("topleft", legend = glab, col = GCOL, lwd = 1.8)
  graphics::par(mar = c(3.6, 4.2, 0.6, 1.2))
  plot(p1$t, p1$B, type = "n", bty = "n", xlab = "Year", ylab = "Bank B (Mt)", ylim = c(0, max(sol[[1]]$path$B, sol[[2]]$path$B)))
  fig_frame(); for (k in 1:2) { lines(sol[[k]]$path$t, sol[[k]]$path$B, col = GCOL[k], lwd = 1.8); fig_ref(v = sol[[k]]$tau, lty = 3, col = GCOL[k]) }
  fig_close(f)
  # inversion on the last date
  grid <- seq(0.06, 0.32, by = 0.005)
  S0 <- sapply(CFG$gammas, function(g) sapply(grid, function(x) solve_v(BASE, D_LAST, x, g, after = FALSE)$S0)); R0 <- R_fun(dec_year(D_LAST))
  xw <- grid - r_on(D_LAST)
  f <- file.path(CFG$fig_dir, "figure_inversion.pdf"); fig_open(f, 6.8, 4.2)
  plot(xw, S0[, 1], type = "n", bty = "n", ylim = range(c(S0, R0, P_LAST)), xlab = lab_lambdaell(), ylab = "Model price at the date ($/tonne)")
  fig_frame(); fig_ref(h = R0); fig_ref(h = P_LAST, lty = 1, col = PAL$ink)
  for (k in 1:2) lines(xw, S0[, k], col = GCOL[k], lwd = 1.8)
  dy <- 0.015 * diff(range(c(S0, R0, P_LAST)))
  text(max(xw), R0 - dy, "reserve", adj = c(1, 1), cex = 0.75, col = PAL$sub)
  text(max(xw), P_LAST + dy, "observed price", adj = c(1, 0), cex = 0.75, col = PAL$ink)
  fig_legend("topright", legend = glab, col = GCOL, lwd = 1.8); fig_close(f)
} else if (CFG$figures) message("figstyle.R not found in the working directory: figures skipped.")

# ---- 5. PAPER NUMBERS -----------------------------------------------------------------------------
cat("\n==================== PAPER NUMBERS ====================\n")
cat(sprintf("Sample: %s to %s, %d dates used. Time-varying r: %s\n", min(dates), D_LAST, length(idx), exists("rf") && is.data.frame(rf)))
cat(sprintf("Auction share of the cap: %.4f\n", share_q))
cat(sprintf("Emissions trend 2015-2024 without 2020: California %.4f, Quebec %.4f, WCI %.4f\n", IN$gy_ca, IN$gy_qc, IN$gy_wci))
cat(sprintf("Covered emissions 2024: California %.1f, WCI %.1f. California share %.3f. Gamma for the WCI: %s\n",
            IN$e24_ca, IN$e24_wci, IN$ca_share, paste(round(CFG$gammas * IN$ca_share, 2), collapse = ", ")))
cat(sprintf("Cap 2026: California %.1f, WCI %.1f. r on the last date: %.4f. Bank on the last date: %s\n",
            c_ca(2026.5) + q_ca(2026.5), BASE$c(2026.5) + BASE$q(2026.5), r_on(D_LAST),
            paste(sapply(CFG$gammas, function(g) round(bank_of(BASE, g * BASE$gscale)(dec_year(D_LAST)), 1)), collapse = ", ")))
cat("\n-- Table: wedge (columns = gamma as calibrated on California) --\n"); print(out$tab_wedge)
cat("\n-- Table: scenarios on the last date --\n"); print(out$tab_scen, digits = 4, row.names = FALSE)
cat("\n-- Robustness on the last date (baseline first) --\n"); print(out$tab_robust, digits = 4, row.names = FALSE)
cat("\n-- Censoring: required return at the reserve auctions --\n"); print(out$tab_censor, digits = 4, row.names = FALSE)
cat("\n-- After the sample --\n"); print(out$tab_post, digits = 4, row.names = FALSE)
cat("\n-- Event windows --\n"); print(out$tab_events, digits = 4, row.names = FALSE)
cat("\nFlags at the fitted wedge:\n"); print(unique(unlist(lapply(out$wedge, function(w) w$flags))))
cat("========================================================\n")
