# convergence.R
# Numerical convergence diagnostics for the interior solver (referee: existence/regularity).
# Calls solve_price() UNCHANGED -- a later Rcpp port of the inner loop drops in transparently.
#
# Design (CFL-safe, one-at-a-time):
#   * Spatial refinement: NB in {200,400,800,1600} at a FIXED dt stable at the finest grid.
#   * Temporal refinement: dt halved in {0.008,...,0.001} at a FIXED coarse NB where the
#     largest dt still satisfies CFL. We never refine both at once into the unstable corner.
# The explicit upwind transport is first-order accurate, so successive-grid differences should
# roughly HALVE as the grid doubles (observed order ~1). That is correct for a monotone scheme
# and is exactly the consistency Barles--Souganidis requires; second-order would be suspect.
#
# Tracks, at each resolution: (a) S^int at a reference point (B=400, t0); (b) the median
# frictionless fundamental S* over 2026--2032 (the wedge quantity used in the paper); (c) the
# free-boundary location B*(t) where S^int meets R(t), where it exists; and the max-norm change
# in the whole price profile vs the next-finer grid.
#
#   source("Scripts/convergence.R"); run_convergence(cleanCap)

source("Scripts/solver.R"); source("Scripts/build_forcing.R")

run_convergence <- function(cleanCap, gamma = 6.1, p = 1, r = 0.045,
                            t0 = 2021, T_hor = 2045, Bmax = 800, g_y = -0.0287,
                            feedback = "net", lambda_ell = 0.10) {
  r <- r + lambda_ell
  invisible(capture.output(f <- build_forcing(cleanCap, g_y = g_y)))
  Bq  <- 400                       # reference bank for the scalar diagnostic
  yrs <- 2021:2026                 # window for the median-S* (wedge) diagnostic
  tb  <- 2043                      # a late year where the floor region (free boundary) exists
  cg  <- seq(0, Bmax, length.out = 1601)   # common grid for max-norm comparisons

  cfl_max <- function(NB) {
    dB   <- Bmax / NB
    Smax <- max(p * f$R(T_hor), f$R(t0), f$L(T_hor))
    gmax <- max(abs(sapply(seq(t0, T_hor, by = 0.25), f$g)))
    dB / (gmax + Smax / gamma)
  }
  bstar <- function(res, t) {                          # free-boundary B*: lowest B with price>floor
    j <- which.min(abs(res$ts - t)); pr <- pmax(res$Sint_surface[j, ], f$R(t))
    ab <- which(pr > f$R(t) + 1e-3)
    if (!length(ab)) NA_real_                     # fully floored
    else if (max(ab) == length(pr)) Inf           # no floor contact from above
    else res$B[max(ab)]                           # floor-contact boundary B*
  }
  solve_one <- function(NB, dt) {
    res <- suppressWarnings(solve_price(f, gamma = gamma, p = p, r = r, T_hor = T_hor,
                                        t0 = t0, Bmax = Bmax, NB = NB, dt = dt,
                                        surface = TRUE, cfl_check = FALSE, feedback = feedback))
    Smed <- median(sapply(yrs, function(y) max(sint_at(res, f$B(y), y), f$R(y))))
    list(sref = price_at(res, Bq), Smed = Smed, bstar = bstar(res, tb),
         prof = approx(res$B, res$price, cg, rule = 2)$y)
  }
  sweep <- function(grid, vary, NB_fix, dt_fix) {
    prev <- NULL; rows <- list()
    for (v in grid) {
      NB <- if (vary == "NB") v else NB_fix
      dt <- if (vary == "dt") v else dt_fix
      s  <- solve_one(NB, dt)
      dnorm <- if (is.null(prev)) NA_real_ else mean(abs(s$prof - prev))
      rows[[length(rows) + 1]] <- list(v = v, NB = NB, dt = dt, cfl = cfl_max(NB),
                                       sref = s$sref, Smed = s$Smed, bstar = s$bstar, dnorm = dnorm)
      prev <- s$prof
    }
    rows
  }
  pr_table <- function(rows, vary) {
    cat(sprintf("\n  %-9s | dt(CFLmax) | Sint(400,t0) | S*med | B*(%d) | ||dP||_1 vs prev | order\n",
                vary, tb))
    prevd <- NA
    for (i in seq_along(rows)) {
      x <- rows[[i]]
      ord <- if (i >= 2 && !is.na(prevd) && !is.na(x$dnorm) && x$dnorm > 0)
               sprintf("%.2f", log2(prevd / x$dnorm)) else "--"
      bs  <- if (is.na(x$bstar)) "interior" else sprintf("%6.1f", x$bstar)
      dn  <- if (is.na(x$dnorm)) "--" else sprintf("%8.4f", x$dnorm)
      stab <- if (x$dt < x$cfl) "ok" else "VIOL"
      cat(sprintf("  %-9s | %.4f(%s) | %11.4f | %5.1f | %8s | %16s | %s\n",
                  if (vary == "NB") as.character(x$NB) else sprintf("%.5f", x$dt),
                  x$dt, stab, x$sref, x$Smed, bs, dn, ord))
      prevd <- x$dnorm
    }
  }

  cat("=== Convergence diagnostics (interior solver) ===\n")
  cat(sprintf("baseline problem: gamma=%.1f, p=%g, g_y=%.4f, Bmax=%g, [t0,T]=[%g,%g]\n",
              gamma, p, g_y, Bmax, t0, T_hor))
  cat("\n-- Spatial refinement (NB doubling), dt=0.0025 fixed (stable at NB=1600) --")
  pr_table(sweep(c(200, 400, 800, 1600), "NB", NB_fix = NA, dt_fix = 0.0025), "NB")
  cat("\n-- Temporal refinement (dt halving), NB=400 fixed (dt=0.008 satisfies CFL) --")
  pr_table(sweep(c(0.008, 0.004, 0.002, 0.001), "dt", NB_fix = 400, dt_fix = NA), "dt")
  cat("\nRead: ||dP||_1 is the max-norm change in the t0 price profile vs the next-coarser grid;\n")
  cat("'order' = log2(prev/this) and should sit near 1 for the first-order upwind scheme.\n")
  cat("S*med (the wedge fundamental) and the free boundary B*(2043) should stabilize.\n")
  invisible(NULL)
}
