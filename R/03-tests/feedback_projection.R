# feedback_projection.R
# Referee robustness: does omitting the price-induced abatement feedback Xi(S)=S/gamma
# bias the bank-exhaustion onset early, and by how much?
#
# The bank drift with feedback is dB = a(t) - ybar(t) + S/gamma - eps = g(t) + S/gamma - eps.
# The baseline projection sets the S/gamma term to zero (exogenous emissions). Three modes:
#
#   "none"       : abate = 0                  -- baseline; EARLIEST onset (lower bound on timing)
#   "floor"      : abate = R(t)/gamma         -- firms abate at the price they face (the floor).
#                                                Conservative, self-consistent. THE defensible run.
#   "structural" : abate = max(Sint,R)/gamma  -- abate at the frictionless fundamental.
#                                                NOT self-consistent: S* overprices because it does
#                                                not net out the abatement it would induce, so this
#                                                spuriously inflates abatement and can prevent
#                                                exhaustion. Included only to SHOW that inconsistency.
#
# The truth lies between "floor" and "structural" and approaches the latter only near the
# boundary, where scarcity lifts the price; so "none" upper-bounds how early onset can be and
# "floor" shows the bias is small. The self-consistent fixed point is the deferred future work.
#
#   source("Scripts/feedback_projection.R"); compare_feedback(clean=NULL, cleanCap)

source("Scripts/solver.R"); source("Scripts/build_forcing.R")
source("Scripts/figure_surface.R"); source("Scripts/figure_projection.R")

project_fb <- function(cleanCap, mu, sigma_y = 20, gamma = 6.1, nsim = 3000,
                       feedback = c("none", "floor", "structural"),
                       t0 = 2026, T_hor = 2045, seed = 1) {
  feedback <- match.arg(feedback)
  set.seed(seed)
  invisible(capture.output(f <- build_forcing(cleanCap, mu_y = mu)))
  surf <- NULL
  if (feedback == "structural")
    invisible(capture.output(surf <- build_surface(cleanCap, gamma = gamma, p = 1, mu_y = mu)))

  yrs_fwd   <- (t0 + 1):T_hor
  years_all <- t0:T_hor
  n   <- length(yrs_fwd)
  B0  <- as.numeric(f$B(t0))
  eps <- matrix(rnorm(nsim * n, 0, sigma_y), nsim, n)
  Bpath <- matrix(NA_real_, nsim, n + 1L); Bpath[, 1] <- B0
  abate_track <- numeric(n)

  for (k in seq_len(n)) {
    tk <- yrs_fwd[k]; Bprev <- Bpath[, k]
    abate <- switch(feedback,
      none       = rep(0, nsim),
      floor      = rep(f$R(tk) / gamma, nsim),
      structural = pmax(sint_at(surf$res, Bprev, tk), f$R(tk)) / gamma)
    abate_track[k] <- mean(abate)
    Bpath[, k + 1L] <- Bprev + f$g(tk) + abate - eps[, k]
  }
  exh <- apply(Bpath, 1, function(b) { i <- which(b <= 0)[1]
                                       if (is.na(i)) NA_integer_ else years_all[i] })
  cdf <- sapply(years_all, function(y) mean(!is.na(exh) & exh <= y))
  cross <- function(q) { i <- which(cdf >= q)[1]; if (is.na(i)) NA_integer_ else years_all[i] }
  list(med = cross(0.50), lo = cross(0.05), hi = cross(0.95),
       abate_2030 = abate_track[which(yrs_fwd == 2030)], B0 = B0)
}

compare_feedback <- function(clean = NULL, cleanCap, gamma = 6.1, nsim = 3000) {
  fy <- function(x) if (is.na(x)) ">2045" else as.character(x)
  cat("\n=== Abatement-feedback robustness: R3-onset year by feedback mode ===\n")
  cat(sprintf("gamma=%.1f, sigma_y=20, n=%d; abate(2030) = mean price-induced abatement in 2030 [Mt/yr]\n\n",
              gamma, nsim))
  cat("  scenario          mode        onset  [5%,   95%]   abate(2030)\n")
  for (mu in c(-3.2, -5.2, -11.7)) {
    lab <- sprintf("mu_y=%.1f", mu)
    for (mode in c("none", "floor", "structural")) {
      r <- project_fb(cleanCap, mu = mu, gamma = gamma, nsim = nsim, feedback = mode)
      cat(sprintf("  %-16s %-11s %5s  [%5s,%5s]   %5.1f\n",
                  if (mode == "none") lab else "", mode,
                  fy(r$med), fy(r$lo), fy(r$hi), r$abate_2030))
    }
    cat("\n")
  }
  cat("Read: 'none' is the baseline (earliest onset). 'floor' is the defensible feedback;\n")
  cat("the gap none->floor is the bias from omitting feedback. 'structural' uses the\n")
  cat("frictionless S* and is shown only to demonstrate it is not self-consistent.\n")
  invisible(NULL)
}
