# lambda_ell.R
# Program-risk discounting on the interior. Carrying Prop. 3.5 into the derivation of
# Prop. 3.7 with the survival state explicit, the PRE-CURTAILMENT price function
# satisfies the interior PDE with (r + lambda*ell) S on the right-hand side, not rS:
# the compensated TOTAL drift is r, but the PDE the observed (pre-jump) price solves
# carries the compensator. For constant lambda*ell this is a discount-rate
# substitution in solve_price -- no solver changes.
#
# Specification guards:
#   * Terminal p = 1: renewal certain CONDITIONAL on survival to T; lambda*ell carries
#     all program risk continuously. Fitting p and lambda*ell jointly double-counts
#     the survival process (Assumption 2.4 / 2.9 are functionals of the same N_t).
#   * Boundary and terminal data are contemporaneous clearing values conditional on
#     survival and take NO haircut; only the holding period is discounted.
#   * ell = 1 corresponds to total loss on curtailment; only the product lambda*ell is
#     identified here, consistent with Cor. 3.6.
#
# The observed interior price is read as  price_obs(t) = S^int(B_t, t; lambda_ell_t)
# and inverted date-by-date on a grid of solved surfaces (S^int monotone decreasing
# in lambda_ell). The per-date path lambda_ell_t is a filtration-update diagnostic of
# the SAME status as the retired implied-p_t series, but now consistent with the
# model's own no-arbitrage restriction. This script replaces calibrate.R.
#
# usage (after the boundary-fixed solver.R):
#   source("Scripts/lambda_ell.R")
#   fit <- fit_lambda_ell(clean, cleanCap)      # ~9 PDE solves; prints summary
#   lambda_crosscheck(fit)                      # vs floor-carry (Cor. 3.6, eta = 0)
#   lambda_event_windows(fit)                   # Apr-25 / Sep-25 / May-26 adoption
#   plot_lambda(fit)
#   pfl <- project_with_lambda(cleanCap, fit$ll_hat)   # bracket tightening
#   base <- project_exhaustion(cleanCap); compare_onset(base, pfl)

source("R/02-model/solver.R"); source("R/02-model/build_forcing.R")
source("R/02-model/fixed_point.R")

# proper decimal year (calibrate.R anchored min(date) to t0 exactly, which put
# 2021-03-30 at 2021.00 and shifted the floor/bank clocks ~0.25 yr early; fixed here)
.decyear <- function(d) {
  yr <- as.integer(format(d, "%Y"))
  yr + as.numeric(d - as.Date(paste0(yr, "-01-01"))) / 365.25
}

fit_lambda_ell <- function(clean, cleanCap,
                           ll_grid = seq(0, 0.24, by = 0.03),
                           gamma = 6.1, r = 0.045, T_hor = 2045, t0 = 2021,
                           Bmax = 800, NB = 400L, dt = 0.004,
                           interior_tol = 0.03, B_obs = NULL) {
  invisible(capture.output(f <- build_forcing(cleanCap)))
  tnum <- .decyear(clean$date)
  Bt   <- if (is.null(B_obs)) f$B(tnum) else B_obs
  Rt   <- f$R(tnum)

  cat(sprintf("solving %d net-feedback surfaces, r + ll for ll in [%.2f, %.2f] ...\n",
              length(ll_grid), min(ll_grid), max(ll_grid)))
  surfs <- lapply(ll_grid, function(ll)
    solve_price(f, gamma = gamma, p = 1, r = r + ll, T_hor = T_hor, t0 = t0,
                Bmax = Bmax, NB = NB, dt = dt, surface = TRUE, feedback = "net"))
  idx <- sapply(tnum, function(tt) which.min(abs(surfs[[1]]$ts - tt)))  # shared ts grid
  M <- sapply(surfs, function(res)
    vapply(seq_along(tnum), function(i)
      approx(res$B, res$Sint_surface[idx[i], ], xout = Bt[i], rule = 2)$y, 0.0))
  if (any(apply(M, 1, function(m) any(diff(m) >= 0))))
    warning("S^int not monotone decreasing in lambda_ell at some dates; check grid/CFL")

  interior <- clean$price > (1 + interior_tol) * Rt
  ll_t <- rep(NA_real_, nrow(clean)); cens_lo <- cens_hi <- 0L
  for (i in which(interior)) {
    m <- M[i, ]; o <- clean$price[i]
    if      (o >= max(m)) { ll_t[i] <- ll_grid[which.max(m)]; cens_lo <- cens_lo + 1L }
    else if (o <= min(m)) { cens_hi <- cens_hi + 1L }            # needs larger ll_grid
    else                    ll_t[i] <- approx(m, ll_grid, xout = o)$y
  }
  ll_hat <- median(ll_t[interior], na.rm = TRUE)
  Smod   <- vapply(seq_along(tnum), function(i)
              approx(ll_grid, M[i, ], xout = ll_hat, rule = 2)$y, 0.0)

  cat(sprintf("\n  interior dates: %d / %d  (premium > %.0f%%)\n",
              sum(interior), nrow(clean), 100 * interior_tol))
  cat(sprintf("  implied lambda*ell:  median %.4f   IQR [%.4f, %.4f]   range [%.4f, %.4f]\n",
              ll_hat, quantile(ll_t[interior], .25, na.rm = TRUE),
              quantile(ll_t[interior], .75, na.rm = TRUE),
              min(ll_t[interior], na.rm = TRUE), max(ll_t[interior], na.rm = TRUE)))
  cat(sprintf("  censored: %d at ll = 0 (price ABOVE frictionless S^int), %d above grid max\n",
              cens_lo, cens_hi))
  if (cens_hi > 0) cat("  -> extend ll_grid upward and re-fit.\n")
  cat(sprintf("  fit at ll_hat: median |log(S_mod/price)| on interior = %.3f\n",
              median(abs(log(Smod[interior] / clean$price[interior])), na.rm = TRUE)))

  invisible(list(df = data.frame(date = clean$date, t = tnum, price = clean$price,
                                 floor = Rt, B = Bt, interior = interior,
                                 ll_t = ll_t, S_mod = Smod),
                 ll_grid = ll_grid, M = M, ll_hat = ll_hat, f = f,
                 pars = list(gamma = gamma, r = r, NB = NB, dt = dt)))
}

# ---- cross-check against the floor-carry decomposition (Cor. 3.6) ----------------
lambda_crosscheck <- function(fit, gR = 0.075, r = NULL) {
  if (is.null(r)) r <- fit$pars$r
  cat("\n  Cross-check: one survival process, two readings\n")
  cat(sprintf("    floor-carry (2013-20, eta ~ 0):  lambda*ell ~ g_R - r = %.3f\n", gR - r))
  cat(sprintf("    interior level (2021-26 fit):    lambda*ell = %.3f\n", fit$ll_hat))
  cat(sprintf("    ratio %.1fx.", fit$ll_hat / (gR - r)))
  cat(" If >> 1, the market discounts the SCARCITY claim harder than the floor\n")
  cat("    claim: candidate readings are elevated lambda_t in 2025-26 (event windows),\n")
  cat("    cap-softening risk loading on S - R rather than S, or Quemin-Trotignon\n")
  cat("    bounded horizons (the standing alternative flagged at Prediction 2).\n")
  invisible(NULL)
}

# ---- event windows: mean implied lambda*ell, +/- win days -------------------------
lambda_event_windows <- function(fit,
                                 events = as.Date(c("2025-04-15", "2025-09-15",
                                                    "2026-05-28")),
                                 labels = c("federal EO threat", "AB 1207 reauth",
                                            "Cap-and-Invest adoption"),
                                 win = 30) {
  df <- fit$df
  cat(sprintf("\n  Event windows (+/- %d days), implied lambda*ell:\n", win))
  for (j in seq_along(events)) {
    e <- events[j]
    pre  <- mean(df$ll_t[df$date >= e - win & df$date <  e], na.rm = TRUE)
    post <- mean(df$ll_t[df$date >  e & df$date <= e + win], na.rm = TRUE)
    n_pre  <- sum(!is.na(df$ll_t) & df$date >= e - win & df$date < e)
    n_post <- sum(!is.na(df$ll_t) & df$date > e & df$date <= e + win)
    cat(sprintf("    %s  %-24s  %.3f -> %.3f  (%+.3f)  [n=%d/%d]\n",
                format(e), labels[j], pre, post, post - pre, n_pre, n_post))
    if (n_pre + n_post == 0)
      cat("      (no data in window -- extend `clean` past its current end)\n")
  }
  cat("  NOTE on the adoption event: pre-adoption prices bundle curtailment risk with\n")
  cat("  cap-adoption probability, while the surface assumes the adopted cap throughout;\n")
  cat("  read the pre-window level as an upper bound on pure curtailment risk.\n")
  invisible(NULL)
}

# ---- plot: price vs floor vs model at ll_hat; implied lambda*ell path -------------
plot_lambda <- function(fit) {
  df <- fit$df
  ev <- as.Date(c("2025-04-15", "2025-09-15", "2026-05-28"))
  op <- par(mfrow = c(2, 1), mar = c(3, 4, 2, 1))
  plot(df$date, df$price, type = "l", col = "steelblue", lwd = 1.5, ylab = "$/tonne",
       xlab = "", main = sprintf("WCI price, floor, and model at lambda*ell = %.3f", fit$ll_hat),
       ylim = range(df$price, df$floor, df$S_mod, na.rm = TRUE))
  lines(df$date, df$floor, col = "red", lty = 2)
  lines(df$date, df$S_mod, col = "darkorange", lwd = 1.2)
  abline(v = ev, col = "grey60", lty = 3)
  legend("topleft", c("observed", "floor R(t)", "model S^int(B_t,t; ll_hat)"),
         col = c("steelblue", "red", "darkorange"), lty = c(1, 2, 1), bty = "n", cex = .8)
  plot(df$date, df$ll_t, type = "l", col = "darkgreen", lwd = 1.5,
       ylab = expression(lambda * ell ~ "(implied)"), xlab = "",
       main = "Implied program-risk discount (interior dates)", ylim = c(0.02, 0.16))
  abline(h = fit$ll_hat, col = "grey70", lty = 3); abline(v = ev, col = "grey60", lty = 3)
  abline(h = 0.03, col = "grey40", lty = 2)
  par(op)
  invisible(NULL)
}

# ---- fixed-point projection at r + lambda*ell (bracket tightening) ----------------
# lambda*ell lowers the surface, hence xi = (min(S,L)-R)^+/gamma, hence pulls onset
# back toward the exogenous edge; report alongside the ll = 0 full-feedback edge.
# Onset distribution is CONDITIONAL on program survival.
project_with_lambda <- function(cleanCap, lambda_ell, r = 0.045, ...) {
  cat(sprintf("fixed-point projection at r + lambda*ell = %.3f + %.3f (survival-conditional)\n",
              r, lambda_ell))
  project_exhaustion_fb(cleanCap, r = r + lambda_ell, ...)
}
