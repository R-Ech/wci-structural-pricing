# figure_surface.R
# The FRICTIONLESS fundamental price surface S*(B,t) at expected emissions.
#
# What this surface is (and is not):
#   It is the no-arbitrage banking value under E^Q[dS] = r S with the B=0
#   scarcity-clearing edge -- WITH NO limits-to-arbitrage correction applied.
#   Under the adopted (steep) cap the level is dominated by the g_R > r ceiling
#   carry: g goes deeply negative, the B=0 clamp pins to the escalating ceiling
#   L(t) from ~2028, and that p-independent injection -- carried back through the
#   solve faster than it discounts (g_R = 7.5% > r = 4.5%) -- sets the level.
#   So S*(B,t) is a BENCHMARK, not a fitted price, and it is ~invariant to p.
#
#   The empirical content is the WEDGE: observed prices sit far below S*, and
#       nu_t = S*(B_t, t) - P_t
#   is the implied limits-to-arbitrage convenience yield -- the paper's claimed
#   result. plot_wedge() visualizes it. We do NOT calibrate or rescale by an
#   "implied p": p-linearity fails in this regime (the clamp, not the terminal,
#   sets the level), so p is fixed at 1 (frictionless terminal S^ext = R(T)).
#
# The slice panel is a separate, friction-independent point: there is no penalty
# CHANNEL under WCI's 4:1 dominance, so S*(.,t) is continuous in B (a convex kink
# where it meets the floor, never HUH's discontinuous step). That holds whatever
# the friction wedge is.
#
#   source("solver.R"); source("build_forcing.R"); source("figure_surface.R")
#   surf <- build_surface(cleanCap)          # frictionless benchmark, p = 1
#   plot_surface(surf); plot_slices(surf)
#   plot_wedge(surf, clean)                  # observed price vs S*; shades nu_t

suppressWarnings(suppressMessages({ library(grDevices); library(graphics) }))

# ---- solve the FRICTIONLESS fundamental surface S*(B,t) -------------------------
# p is fixed at 1 (frictionless terminal); it is NOT a calibration lever here.
build_surface <- function(cleanCap,
                          gamma = 6.1, p = 1.0, r = 0.045, lambda_ell = 0.10, 
                          t0 = 2021, T_hor = 2045,
                          Bmax = 800, NB = 400L, dt = 0.005,   # solve on full domain so the
                          B_focus = 500,                       # floor plateau is contained;
                          g_y = -0.02869,                         # B_focus trims only the PLOT.
                          plot_years = NULL) {
  r   <- r + lambda_ell
  f   <- build_forcing(cleanCap, g_y = g_y)
  res <- solve_price(f, gamma = gamma, p = p, r = r, T_hor = T_hor, t0 = t0,
                     Bmax = Bmax, NB = NB, dt = dt, surface = TRUE, feedback = "net")
  
  if (is.null(plot_years)) plot_years <- seq(ceiling(t0), floor(T_hor) - 2, by = 1)
  # final ~2 yrs are pinned by the S^ext = R(T) terminal anchor, not priced states
  # map each plot year to its stored time-slice; apply the floor R(year) row-wise
  # (the solver carries S^int unfloored inside the loop; price = max(S^int, R)).
  idx   <- sapply(plot_years, function(yy) which.min(abs(res$ts - yy)))
  Rvec  <- f$R(plot_years)
  Z     <- sapply(seq_along(plot_years), function(k)
    pmax(res$Sint_surface[idx[k], ], Rvec[k]))   # [NB+1 x nYears], S*(B,t)
  rownames(Z) <- round(res$B, 3); colnames(Z) <- plot_years
  
  list(B = res$B, years = plot_years, price = Z, forcing = f, res = res,
       gamma = gamma, p = p, r = r, g_y = g_y, B_focus = B_focus)
}

# ---- persp surface (HUH Fig 1 style), labelled as the frictionless benchmark ----
plot_surface <- function(surf, file = "figs/figure_surface.pdf",
                         nB_plot = 70, nYr_plot = 60, theta = -38, phi = 22) {
  # subsample to a clean mesh, within the focus window (off the Bmax wall)
  bcap <- if (is.null(surf$B_focus)) max(surf$B) else surf$B_focus
  bsel <- which(surf$B <= bcap)
  bi <- bsel[unique(round(seq(1, length(bsel),     length.out = nB_plot)))]
  yi <-       unique(round(seq(1, length(surf$years), length.out = nYr_plot)))
  x  <- surf$B[bi]; y <- surf$years[yi]; z <- surf$price[bi, yi, drop = FALSE]
  
  # facet-height shading: pale -> steel by mean facet height (standard persp idiom)
  zf  <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]) / 4
  pal <- colorRampPalette(c("#f4f6f9", "#c8d4e3", "#6f88ab", "#34506e"))(100)
  fc  <- pal[cut(zf, 100, labels = FALSE)]
  
  if (!is.null(file)) pdf(file, width = 7.4, height = 5.6, pointsize = 11)
  op <- par(mar = c(1.6, 1.6, 1.2, 1.0))
  persp(x, y, z, theta = theta, phi = phi, expand = 0.62,
        col = fc, border = "#5a6b80", lwd = 0.35, shade = 0.28,
        ticktype = "detailed", nticks = 5,
        xlab = "\nBank B (Mt)", ylab = "\nYear", zlab = "\nS* ($/t)",
        cex.axis = 0.7, cex.lab = 0.85)
  title(main = sprintf("Risk-adjusted equilibrium surface S*(B,t) at lambda*ell = 0.10 (g_y=%.4f)",
                       surf$g_y), cex.main = 0.9, font.main = 1)
  par(op); if (!is.null(file)) { dev.off(); message("wrote ", file) }
  invisible(surf)
}

# ---- slice panel: S*(.,t) vs B -- the friction-independent no-step contrast -----
plot_slices <- function(surf, years = c(2026, 2032, 2038, 2044),
                        file = "figs/figure_surface_slices.pdf") {
  jj  <- sapply(years, function(yy) which.min(abs(surf$years - yy)))
  bcap <- if (is.null(surf$B_focus)) max(surf$B) else surf$B_focus
  bsel <- which(surf$B <= bcap)
  Bs  <- surf$B[bsel]
  Z   <- surf$price[bsel, jj, drop = FALSE]
  cols <- colorRampPalette(c("#9fb3c8", "#34506e"))(length(years))
  
  if (!is.null(file)) pdf(file, width = 6.6, height = 4.8, pointsize = 11)
  op <- par(mar = c(5.4, 4.2, 2.0, 1.0))
  matplot(Bs, Z, type = "l", lty = 1, lwd = 2, col = cols,
          xlab = "Bank B (Mt)", ylab = "S*(B,t) at fitted lambda*ell ($/t)",
          main = "S*(.,t) is continuous in B: no penalty step (cf. HUH)")
  # floor levels at each year (dashed) -- shows the plateau the curves rest on
  for (k in seq_along(years))
    abline(h = surf$forcing$R(years[k]), col = cols[k], lty = 3, lwd = 0.8)
  legend("topright", legend = sprintf("t = %d", years), col = cols,
         lwd = 2, bty = "n", title = "year", cex = 0.9)
  mtext("dashed = floor R(t); S* meets it in a convex kink, never a jump",
        side = 1, line = 4.1, cex = 0.72, col = "grey35")
  par(op); if (!is.null(file)) { dev.off(); message("wrote ", file) }
  invisible(Z)
}

# ---- numeric sanity print: S*(B,t) along the bank path -------------------------
surface_diagnostics <- function(surf) {
  yy <- c(2026, 2030, 2034, 2038, 2042)
  Bt <- pmax(surf$forcing$B(yy), 0)
  Sp <- mapply(function(b, y) {
    j <- which.min(abs(surf$years - y)); approx(surf$B, surf$price[, j], b, rule = 2)$y
  }, Bt, yy)
  Rt <- surf$forcing$R(yy)
  cat("\n  S* (risk-adjusted, fitted lambda*ell) along the no-feedback bank path:\n")
  cat("  year |  B(t)   S*(B,t)  R(t)   regime\n")
  for (i in seq_along(yy))
    cat(sprintf("  %4d | %6.1f  %6.2f  %6.2f   %s\n", yy[i], Bt[i], Sp[i], Rt[i],
                if (Sp[i] <= Rt[i] + 1e-2) "floor (R1)" else "interior (R2/R3)"))
  invisible(NULL)
}