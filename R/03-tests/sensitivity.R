# sensitivity.R
# Table 5: sensitivity of the bank-exhaustion (R3) onset year to one-at-a-time
# parameter variation around the baseline.
#
# Baseline: gamma = 6.1, g_y = -2.87%/yr, sigma_y = 20 Mt, B(2026) ~ 403 Mt, p = 1.
#
# FOUR PANELS, and they do not all come from the same engine:
#   gamma     project_exhaustion() -- EXOGENOUS. It has no gamma argument at all,
#             so these rows are identical to baseline BY CONSTRUCTION, not by
#             numerical coincidence. That is the claim being made ("onset is
#             gamma-invariant"), and the script asserts it rather than pretending
#             to have varied something. gamma enters only through the feedback
#             variants, which is what the bottom panel is for.
#   g_y       project_exhaustion() across the drift band. The dominant channel.
#   sigma_y   project_exhaustion() at sigma_y = 12 and 28. Widens the band, does
#             not move the median.
#   feedback  project_exhaustion_fb(). lambda_ell enters through r, NOT as its own
#             argument: solve_net_surface() and project_exhaustion_fb() both take
#             r = 0.045 and do NOT add lambda_ell internally, unlike
#             build_surface() and run_convergence() which do. Pass r = 0.045 + ll
#             explicitly or the row silently reports the frictionless case.
#
# This file REPLACES the pre-revision version, which used the superseded absolute
# drift parameterization (mu_y in Mt/yr, scenarios -3.2/-5.2/-11.7), a starting
# bank of 429 Mt, and a panel of p rows that the current Table 5 note says the
# feedback panel replaces. Nothing in the old version reproduced the printed table.
#
#   source("R/03-tests/sensitivity.R")
#   sens <- run_sensitivity(cleanCap, lambda_ell = 0.10)

source("R/02-model/solver.R"); source("R/02-model/build_forcing.R")
source("R/04-figures/figure_projection.R")    # project_exhaustion(), .SCEN, .cross
source("R/02-model/fixed_point.R")            # project_exhaustion_fb()

.onset <- function(s) c(med = .cross(s$years, s$cdf, 0.50),
                        lo  = .cross(s$years, s$cdf, 0.05),
                        hi  = .cross(s$years, s$cdf, 0.95))

.fmt <- function(x) if (is.na(x)) ">2045" else as.character(x)

.row <- function(rows, label, panel, s) {
  o <- .onset(s)
  cat(sprintf("  %-42s %6s  [%5s, %5s]\n", label, .fmt(o["med"]), .fmt(o["lo"]), .fmt(o["hi"])))
  rbind(rows, data.frame(panel = panel, variation = label,
                         med = o["med"], lo = o["lo"], hi = o["hi"],
                         row.names = NULL))
}

run_sensitivity <- function(cleanCap, lambda_ell = 0.10, gamma = 6.1,
                            g_central = -0.0287, sigma_y = 20, r = 0.045,
                            nsim_exo = 4000, nsim_fb = 5000, seed = 1) {
  one <- function(mu, sig = sigma_y, nsim = nsim_exo, off = 0)
    project_exhaustion(cleanCap, mu_set = c(mu), sigma_y = sig,
                       nsim = nsim, B0_offset = off, seed = seed)[[1]]
  onefb <- function(mu, gam, rr, nsim = nsim_fb, ...)
    project_exhaustion_fb(cleanCap, mu_set = c(mu), gamma = gam, r = rr,
                          sigma_y = sigma_y, nsim = nsim, seed = seed, ...)[[1]]

  rows <- NULL
  cat("\n=== Table 5: bank-exhaustion (R3) onset sensitivity ===\n")
  cat(sprintf("baseline: gamma=%.1f, g_y=%.4f, sigma_y=%.0f Mt, p=1, n=%d\n\n",
              gamma, g_central, sigma_y, nsim_exo))

  base <- one(g_central)
  cat(sprintf("  starting bank B(2026) = %.0f Mt\n", base$B0))
  rows <- .row(rows, "Baseline", "baseline", base)

  cat("\nAbatement cost gamma (exogenous variant)\n")
  cat("  project_exhaustion() takes no gamma argument: these rows are identical to\n",
      "  baseline by construction. gamma acts only through the feedback panel below.\n", sep = "")
  for (g in c(4.6, 7.5)) rows <- .row(rows, sprintf("gamma = %.1f", g), "gamma", base)

  cat("\nEmissions decline rate g_y (proportional, /yr)\n")
  for (mu in c(-0.02, -0.045, -0.06, -0.08))
    rows <- .row(rows, sprintf("g_y = %+.1f%%", 100 * mu), "g_y", one(mu))

  cat("\nEmissions volatility sigma_y (Mt)\n")
  for (sg in c(12, 28))
    rows <- .row(rows, sprintf("sigma_y = %.0f", sg), "sigma_y", one(g_central, sig = sg))

  cat("\nPrice-abatement feedback (central drift; discount wedge lambda*ell)\n")
  rows <- .row(rows, "exogenous (xi = 0)", "feedback",
               one(g_central, nsim = nsim_fb))
  rows <- .row(rows, sprintf("fitted lambda*ell = %.2f", lambda_ell), "feedback",
               onefb(g_central, gamma, r + lambda_ell))
  rows <- .row(rows, "frictionless (lambda*ell = 0)", "feedback",
               onefb(g_central, gamma, r))

  # P-bar row: requires pbar_mult to be threaded through project_exhaustion_fb()
  # into solve_price(). As written, project_exhaustion_fb() hardcodes the
  # solve_price() call without it, so the row is skipped rather than silently
  # reported as the pbar_mult = 1 case.
  if ("pbar_mult" %in% names(formals(project_exhaustion_fb))) {
    rows <- .row(rows, "P-bar = 1.22 R (window average)", "feedback",
                 onefb(g_central, gamma, r + lambda_ell, pbar_mult = 1.22))
  } else {
    cat("  [SKIPPED] P-bar = 1.22 R: project_exhaustion_fb() does not expose\n",
        "            pbar_mult. Add it to the signature and pass it through to\n",
        "            solve_price() to reproduce this row of Table 5.\n", sep = "")
  }

  cat("\nRead: exhaustion timing is governed by the cap, g_y, sigma_y and the starting\n")
  cat("bank; it is gamma-invariant in the exogenous variant. Feedback and the lambda*ell\n")
  cat("wedge enter through the abatement response and defer onset by one to three years.\n")
  invisible(rows)
}
