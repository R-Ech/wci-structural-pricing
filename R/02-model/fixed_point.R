# fixed_point.R
# Self-consistent price-abatement feedback: the fixed point Sec. 4.6 defers.
#
# Convention (net anchor). y(t) is projected from REALIZED covered emissions, which
# already embed the abatement induced by the realized (~floor) price. The feedback is
# therefore the INCREMENTAL abatement above the floor-embedded response,
#     xi(B,t) = ( min(max(S^int, R), L) - R ) / gamma  >= 0,
# never gross S/gamma: gross double-counts the floor-level response already inside the
# 2024 emissions anchor and reproduces the un-netted "frictionless" fixed point the
# paper rejects (Sec. 4.6).
#
# No Picard iteration. With feedback = "net" the term sits inside mu_B of solve_price,
# so the surface S^int(B,t) is self-consistent in state by construction. What remains:
#   (i)  the deterministic self-consistent bank path = the characteristic
#           dB/dt = g(t) + xi(B(t),t),   B(t0) = f$B(t0),
#        integrated forward on the solved surface (fixed_point_path, monthly Euler);
#   (ii) the stochastic projection with the same per-path feedback
#           B_{k+1} = B_k + g_det(k) + xi(B_k, mid-year) - eps_k
#        (project_exhaustion_fb, annual step, mirrors project_exhaustion exactly).
# Consistency requirement: gamma = Inf must reproduce project_exhaustion()
# draw-for-draw under a common seed -- run fb_check_reduction() FIRST; nothing is
# citable until it passes.
#
# usage:
#   source("Scripts/fixed_point.R")
#   fb_check_reduction(cleanCap)                    # gamma=Inf == exogenous, per draw
#   ns  <- solve_net_surface(cleanCap)              # net surface, central drift
#   fp  <- fixed_point_path(ns)                     # (i) deterministic path + xi table
#   pfb <- project_exhaustion_fb(cleanCap)          # (ii) stochastic, all scenarios
#   projection_summary(pfb); xi_summary(pfb)
#   base <- project_exhaustion(cleanCap); compare_onset(base, pfb)
#   plot_projection_cdf(pfb, file = NULL); plot_projection_fan(pfb, file = NULL)

source("R/02-model/solver.R"); source("R/02-model/build_forcing.R")
source("R/04-figures/figure_projection.R") # Requires .SCEN, .cross and project_exhaustion() from figure_projection.R.

# incremental abatement flow [Mt/yr]; sint may sit below R (floored) or above L (capped)
.xi_net <- function(sint, R, L, gamma) (pmin(pmax(sint, R), L) - R) / gamma

# ---- net-feedback surface -------------------------------------------------------
solve_net_surface <- function(cleanCap, g_y = -0.02869, gamma = 6.1, p = 1, r = 0.045,
                              t0 = 2026, T_hor = 2045,
                              Bmax = 800, NB = 800L, dt = 0.002) {
  invisible(capture.output(f <- build_forcing(cleanCap, g_y = g_y)))
  res <- solve_price(f, gamma = gamma, p = p, r = r, T_hor = T_hor, t0 = t0,
                     Bmax = Bmax, NB = NB, dt = dt, surface = TRUE, feedback = "net")
  list(f = f, res = res, gamma = gamma, t0 = t0, T_hor = T_hor)
}

# ---- (i) deterministic self-consistent path: forward Euler on the characteristic --
fixed_point_path <- function(ns, dt = 1/12, verbose = TRUE) {
  f <- ns$f; res <- ns$res; gamma <- ns$gamma
  tt <- seq(ns$t0, ns$T_hor, by = dt); m <- length(tt)
  B <- S <- xi <- numeric(m); B[1] <- as.numeric(f$B(ns$t0))
  for (i in seq_len(m)) {
    S[i]  <- max(sint_at(res, max(B[i], 0), tt[i]), f$R(tt[i]))
    xi[i] <- .xi_net(S[i], f$R(tt[i]), f$L(tt[i]), gamma)
    if (i < m) B[i + 1] <- B[i] + dt * (f$g(tt[i]) + xi[i])
  }
  j <- which(B <= 0)[1]; onset <- if (is.na(j)) NA_real_ else tt[j]
  if (verbose) {
    cat(sprintf("self-consistent (net-feedback) deterministic path: B(%d) = %.0f Mt\n",
                ns$t0, B[1]))
    cat(sprintf("  onset (B<=0): %s\n",
                if (is.na(onset)) "none <= 2045" else sprintf("%.2f", onset)))
    on_yr <- if (is.na(onset)) ns$T_hor else ceiling(onset)
    yrs <- ns$t0:min(ns$T_hor, on_yr)
    idx <- sapply(yrs, function(y) which.min(abs(tt - y)))
    print(round(data.frame(year = yrs, B = pmax(B[idx], 0), S = S[idx],
                           R = f$R(yrs), L = f$L(yrs), xi = xi[idx]), 1),
          row.names = FALSE)
    cat(sprintf("  xi = (min(S,L)-R)/gamma [Mt/yr]; its ceiling cap at onset = %.1f Mt/yr\n",
                (f$L(on_yr) - f$R(on_yr)) / gamma))
  }
  invisible(data.frame(t = tt, B = pmax(B, 0), S = S, xi = xi))
}

# ---- (ii) stochastic projection with per-path feedback ---------------------------
# Mirrors project_exhaustion exactly: same seed handling, same eps draw shape and
# order, same unfloored evolution for onset detection, path floored at 0 only for the
# fan. gamma = Inf skips the surface solve and sets xi = 0 identically, so it must
# reproduce the exogenous projection draw-for-draw (fb_check_reduction).
project_exhaustion_fb <- function(cleanCap,
                                  mu_set = c(-0.02, -0.0287, -0.045),
                                  gamma = 6.1, p = 1, r = 0.045,
                                  sigma_y = 20, nsim = 5000,
                                  t0 = 2026, T_hor = 2045,
                                  mdi_per_year = 0, B0_offset = 0, seed = 1,
                                  Bmax = 800, NB = 800L, dt = 0.002) {
  set.seed(seed)
  yrs_fwd   <- (t0 + 1):T_hor
  years_all <- t0:T_hor
  n   <- length(yrs_fwd)
  use_fb <- is.finite(gamma)
  out <- list()
  for (mu in mu_set) {
    invisible(capture.output(f <- build_forcing(cleanCap, g_y = mu)))
    res <- if (use_fb)
      solve_price(f, gamma = gamma, p = p, r = r, T_hor = T_hor, t0 = t0,
                  Bmax = Bmax, NB = NB, dt = dt, surface = TRUE, feedback = "net")
    else NULL
    g_det <- sapply(yrs_fwd, f$g) + mdi_per_year
    B0    <- as.numeric(f$B(t0)) + B0_offset
    eps   <- matrix(rnorm(nsim * n, 0, sigma_y), nrow = nsim, ncol = n)

    Bcur  <- rep(B0, nsim)                       # evolves UNfloored (as in the base run)
    Bpath <- matrix(NA_real_, nsim, n + 1L); Bpath[, 1] <- B0
    ximat <- matrix(0, nsim, n)
    Bdet  <- numeric(n + 1L); Bdet[1] <- B0; bd <- B0   # deterministic (no-noise) twin
    for (k in seq_len(n)) {
      tk <- yrs_fwd[k] - 0.5                     # mid-year price drives year-k abatement
      if (use_fb) {
        it  <- which.min(abs(res$ts - tk))
        row <- res$Sint_surface[it, ]
        Sk  <- approx(res$B, row, xout = pmax(Bcur, 0), rule = 2)$y
        ximat[, k] <- .xi_net(Sk, f$R(tk), f$L(tk), gamma)
        Sd  <- approx(res$B, row, xout = max(bd, 0), rule = 2)$y
        bd  <- bd + g_det[k] + .xi_net(Sd, f$R(tk), f$L(tk), gamma)
      } else bd <- bd + g_det[k]
      Bcur <- Bcur + g_det[k] + ximat[, k] - eps[, k]
      Bpath[, k + 1L] <- Bcur
      Bdet[k + 1L] <- bd
    }
    exh <- apply(Bpath, 1, function(b) { i <- which(b <= 0)[1]
      if (is.na(i)) NA_integer_ else years_all[i] })
    cdf <- sapply(years_all, function(y) mean(!is.na(exh) & exh <= y))
    out[[as.character(mu)]] <- list(mu = mu, B0 = B0, years = years_all,
                                    Bpath = pmax(Bpath, 0), exh = exh,
                                    det = pmax(Bdet, 0), cdf = cdf,
                                    xi_mean = colMeans(ximat), xi_years = yrs_fwd)
  }
  attr(out, "sigma_y") <- sigma_y; attr(out, "nsim") <- nsim
  attr(out, "mdi_per_year") <- mdi_per_year; attr(out, "gamma") <- gamma
  out
}

# ---- consistency check: gamma = Inf must equal the exogenous projection ----------
fb_check_reduction <- function(cleanCap, nsim = 1000, seed = 1,
                               mu_set = c(-0.02, -0.0287, -0.045)) {
  base <- project_exhaustion(cleanCap, mu_set = mu_set, nsim = nsim, seed = seed)
  off  <- project_exhaustion_fb(cleanCap, mu_set = mu_set, nsim = nsim, seed = seed,
                                gamma = Inf)
  cat("gamma = Inf vs exogenous projection, per-draw onset comparison:\n")
  for (k in names(base)) {
    a <- base[[k]]$exh; b <- off[[k]]$exh
    mm <- sum(xor(is.na(a), is.na(b))) + sum(a != b, na.rm = TRUE)
    cat(sprintf("  %-8s mismatches: %d / %d   %s\n", k, mm, nsim,
                if (mm == 0) "PASS"
                else "CHECK (fp rounding at the B=0 knife edge; must be ~0)"))
  }
  invisible(NULL)
}

# ---- reporting helpers ------------------------------------------------------------
xi_summary <- function(proj) {
  cat(sprintf("\n  Mean induced abatement xi [Mt/yr] along simulated paths (gamma=%g)\n",
              attr(proj, "gamma")))
  cat("  scenario                       2027    2030    2033    max(path)\n")
  for (k in names(proj)) {
    s <- proj[[k]]; g <- function(y) s$xi_mean[which(s$xi_years == y)]
    cat(sprintf("  %-28s %6.1f  %6.1f  %6.1f  %8.1f\n",
                .SCEN[[k]]$lab, g(2027), g(2030), g(2033), max(s$xi_mean)))
  }
  invisible(NULL)
}

compare_onset <- function(base, fb) {
  fy <- function(x) if (is.na(x)) ">2045" else as.character(x)
  f1 <- function(s) sprintf("%s [%s,%s]", fy(.cross(s$years, s$cdf, .50)),
                            fy(.cross(s$years, s$cdf, .05)), fy(.cross(s$years, s$cdf, .95)))
  cat(sprintf("\n  %-28s %18s %18s %8s\n", "scenario", "exogenous", "net feedback", "shift"))
  for (k in names(base)) {
    m1 <- .cross(base[[k]]$years, base[[k]]$cdf, .5)
    m2 <- .cross(fb[[k]]$years,  fb[[k]]$cdf,  .5)
    cat(sprintf("  %-28s %18s %18s %8s\n", .SCEN[[k]]$lab, f1(base[[k]]), f1(fb[[k]]),
                if (is.na(m1) || is.na(m2)) "--" else sprintf("%+d yr", m2 - m1)))
  }
  invisible(NULL)
}
