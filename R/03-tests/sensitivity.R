# sensitivity.R
# One-at-a-time parameter sweep for the referee's robustness table.
# Reports, for each parameter varied around the baseline:
#   - bank-exhaustion (R3 onset) median and [5%,95%] band   [depends on mu_y, sigma_y, B0]
#   - frictionless fundamental S* median over 2021-2026      [depends on gamma, mu_y, p, B0]
#   - limits-to-arbitrage wedge nu = S* - P, median and range
#
# Computing every column for every row is deliberate: the FLAT columns are results.
# The p-rows' constant S* is the "essentially invariant to p" demonstration; the
# sigma-rows' constant S* confirms the surface solve does not depend on sigma_y.
#
# Requires: solver.R, build_forcing.R, figure_surface.R, figure_projection.R,
#           and `clean` (date, price) and `cleanCap` in scope.
#
#   source("Scripts/sensitivity.R"); run_sensitivity(clean, cleanCap)

source("Scripts/solver.R"); source("Scripts/build_forcing.R")
source("Scripts/figure_surface.R"); source("Scripts/figure_projection.R")

# ---- one cell: exhaustion band ------------------------------------------------
.exh_cell <- function(cleanCap, mu, sig, off, nsim) {
  invisible(capture.output(
    pr <- project_exhaustion(cleanCap, mu_set = c(mu), sigma_y = sig,
                             nsim = nsim, B0_offset = off, seed = 1)))
  s <- pr[[1]]; yr <- s$years
  cross <- function(q) { i <- which(s$cdf >= q)[1]; if (is.na(i)) NA_integer_ else yr[i] }
  list(B0 = s$B0, med = cross(0.50), lo = cross(0.05), hi = cross(0.95))
}

# ---- one cell: S* and wedge over the observed sample --------------------------
.wedge_cell <- function(cleanCap, clean, gamma, p, mu, off) {
  invisible(capture.output(
    s <- build_surface(cleanCap, gamma = gamma, p = p, mu_y = mu)))
  dec <- function(d) as.numeric(format(d, "%Y")) +
                     as.numeric(d - as.Date(paste0(format(d, "%Y"), "-01-01"))) / 365.25
  tnum  <- dec(clean$date)
  Bt    <- s$forcing$B(tnum) + off
  Sstar <- mapply(function(b, tt) max(sint_at(s$res, b, tt), s$forcing$R(tt)), Bt, tnum)
  nu    <- Sstar - clean$price
  list(Sstar = median(Sstar), nu = median(nu, na.rm = TRUE),
       nu_lo = min(nu, na.rm = TRUE), nu_hi = max(nu, na.rm = TRUE))
}

# ---- full row -----------------------------------------------------------------
.row <- function(label, cleanCap, clean, gamma, p, mu, sig, off, nsim) {
  e <- .exh_cell(cleanCap, mu, sig, off, nsim)
  w <- .wedge_cell(cleanCap, clean, gamma, p, mu, off)
  fy <- function(x) if (is.na(x)) ">2045" else as.character(x)
  cat(sprintf("%-22s | g=%4.1f mu=%5.1f s=%4.1f B0=%3.0f p=%4.2f | exh %5s [%5s,%5s] | S*=%6.1f | nu=%6.1f [%5.1f,%6.1f]\n",
              label, gamma, mu, sig, e$B0, p, fy(e$med), fy(e$lo), fy(e$hi),
              w$Sstar, w$nu, w$nu_lo, w$nu_hi))
}

# ---- the sweep ----------------------------------------------------------------
run_sensitivity <- function(clean, cleanCap, nsim = 4000) {
  g0 <- 6.1; p0 <- 1.0; mu0 <- -5.2; s0 <- 20; off0 <- 0
  cat("\n=== Sensitivity sweep (one-at-a-time around baseline) ===\n")
  cat("baseline: gamma=6.1, mu_y=-5.2, sigma_y=20, B0=429 (offset 0), p=1\n")
  cat("exh = R3-onset year; S* and nu = median over 2021-2026 ($/t); nu range in brackets\n\n")

  .row("BASELINE",          cleanCap, clean, g0,  p0,  mu0,  s0,  off0, nsim)
  cat("-- gamma (S*/wedge identification; exhaustion is gamma-invariant) --\n")
  .row("gamma=4.6 (lo)",    cleanCap, clean, 4.6, p0,  mu0,  s0,  off0, nsim)
  .row("gamma=7.5 (hi)",    cleanCap, clean, 7.5, p0,  mu0,  s0,  off0, nsim)
  cat("-- mu_y (drift band; both columns move) --\n")
  .row("mu_y=-3.2 (slow)",  cleanCap, clean, g0,  p0,  -3.2, s0,  off0, nsim)
  .row("mu_y=-11.7 (fast)", cleanCap, clean, g0,  p0,  -11.7,s0,  off0, nsim)
  cat("-- sigma_y (emissions noise; exhaustion band; S* should be flat) --\n")
  .row("sigma_y=12 (lo)",   cleanCap, clean, g0,  p0,  mu0,  12,  off0, nsim)
  .row("sigma_y=28 (hi)",   cleanCap, clean, g0,  p0,  mu0,  28,  off0, nsim)
  cat("-- starting bank (flow 429 vs vintage-restricted ~379) --\n")
  .row("B0=379 (vintage)",  cleanCap, clean, g0,  p0,  mu0,  s0,  -50,  nsim)
  cat("-- p (S* should be ~invariant: the demonstration) --\n")
  .row("p=0.7",             cleanCap, clean, g0,  0.7, mu0,  s0,  off0, nsim)
  .row("p=0.5",             cleanCap, clean, g0,  0.5, mu0,  s0,  off0, nsim)
  cat("\n(For gamma and p rows, the exhaustion columns repeat baseline by construction\n")
  cat(" -- exhaustion is exogenous-emissions and does not depend on gamma or p.)\n")
  invisible(NULL)
}
