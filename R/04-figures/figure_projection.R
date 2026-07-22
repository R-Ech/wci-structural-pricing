# figure_projection.R
# Validation (iv): when does the bank hit zero (R3 onset) under emissions uncertainty?
#
# Mechanism. The deterministic bank drift is g(t) = a(t) - ybar(t), already built by
# build_forcing on the ADOPTED CA Table 6-2 cap and the central drift mu_y. So the
# stochastic bank is just that drift plus annual emissions noise:
#
#     B_{t} = B_{t-1} + g(t) - eps_t,   eps_t ~ N(0, sigma_y),   B_{2026} = f$B(2026)
#
# (eps_t is the deviation of realized emissions from the drift; symmetric, so its
# sign is immaterial. Higher emissions -> lower bank -> earlier exhaustion.)
# y_t is EXOGENOUS by design: no S/gamma abatement feedback. That biases R3-onset
# slightly EARLY in the scarce scenarios (price-induced abatement would push it out
# ~1-2 yr); we keep the projection model-independent and treat that as the upper edge.
#
# Noise is iid annual. Real emissions are persistent (a high year predicts a high
# next year), which would WIDEN the exhaustion band; iid sigma_y=20 is the narrow,
# conservative choice. Flag this in the text; an AR(1)/RW variant is a robustness run.
#
# MDI: the adopted package adds up to ~118 Mt "above the cap" (conditional on uptake).
# mdi_per_year adds it back to the drift as the upper (later-exhaustion) edge of the
# band; default 0. ~118/20 = 5.9 Mt/yr spreads the full entitlement across 2026-2045.
#
# Headline figure = P(R3 onset by year t) across the drift band {-3.2, -5.2, -11.7}.
#
#   source("build_forcing.R"); source("figure_projection.R")
#   proj <- project_exhaustion(cleanCap)
#   plot_projection_cdf(proj); plot_projection_fan(proj); projection_summary(proj)

suppressWarnings(suppressMessages({ library(grDevices); library(graphics) }))

# scenario labels / colours (slow decarb is the headline: earliest scarcity)
.SCEN <- list(
  `-0.02`   = list(lab = "g_y = -2.0%/yr  (slow decarb)",  col = "#b4451f"),
  `-0.0287` = list(lab = "g_y = -2.87%/yr (central)",      col = "#34506e"),
  `-0.045`  = list(lab = "g_y = -4.5%/yr  (fast decarb)",  col = "#2e7d5b"),
  `-0.06`   = list(lab = "g_y = -6.0%/yr  (faster decarb)", col = "#2e3"),
  `-0.08`   = list(lab = "g_y = -8.0%/yr  (fastest decarb)", col = "#b4459f"))

# ---- Monte Carlo exhaustion timing per drift scenario --------------------------
project_exhaustion <- function(cleanCap,
                               mu_set = c(-0.02, -0.0287, -0.045),
                               sigma_y = 20, nsim = 5000,
                               t0 = 2026, T_hor = 2045,
                               mdi_per_year = 0, B0_offset = 0, seed = 1) {
  set.seed(seed)
  yrs_fwd   <- (t0 + 1):T_hor              # evolution years 2027..2045
  years_all <- t0:T_hor
  out <- list()
  for (mu in mu_set) {
    invisible(capture.output(f <- build_forcing(cleanCap, g_y = mu)))  # silence its prints
    g_det <- sapply(yrs_fwd, f$g) + mdi_per_year          # deterministic drift per fwd year
    B0    <- as.numeric(f$B(t0)) + B0_offset            # starting bank (offset for sensitivity)
    n     <- length(yrs_fwd)
    eps   <- matrix(rnorm(nsim * n, 0, sigma_y), nrow = nsim, ncol = n)
    dB    <- matrix(g_det, nrow = nsim, ncol = n, byrow = TRUE) - eps
    Bpath <- cbind(B0, B0 + t(apply(dB, 1, cumsum)))       # nsim x (n+1), years t0..T_hor
    exh   <- apply(Bpath, 1, function(b) { i <- which(b <= 0)[1]
    if (is.na(i)) NA_integer_ else years_all[i] })
    Bpath <- pmax(Bpath, 0)                                 # R3 absorbing: floor at 0 for the fan
    det   <- pmax(B0 + c(0, cumsum(g_det)), 0)             # deterministic (no-noise) path, floored
    cdf   <- sapply(years_all, function(y) mean(!is.na(exh) & exh <= y))
    out[[as.character(mu)]] <- list(mu = mu, B0 = B0, years = years_all,
                                    Bpath = Bpath, exh = exh, det = det, cdf = cdf)
  }
  attr(out, "sigma_y") <- sigma_y; attr(out, "nsim") <- nsim
  attr(out, "mdi_per_year") <- mdi_per_year
  out
}

# helper: first year the CDF crosses level q (NA if never)
.cross <- function(years, cdf, q) { i <- which(cdf >= q)[1]; if (is.na(i)) NA else years[i] }

# ---- HEADLINE: P(R3 onset by year t) across the drift band ----------------------
plot_projection_cdf <- function(proj, file = "figs/figure_projection.pdf") {
  years <- proj[[1]]$years
  if (!is.null(file)) pdf(file, width = 7.0, height = 4.8, pointsize = 11)
  op <- par(mar = c(4.0, 4.4, 2.4, 1.0))
  plot(NA, xlim = range(years), ylim = c(0, 1),
       xlab = "Year", ylab = "P(R3 onset by year t)",
       main = "Bank-exhaustion timing under emissions uncertainty")
  abline(h = c(0.5), col = "grey85", lty = 3)
  for (k in names(proj)) {
    sc <- .SCEN[[k]]; s <- proj[[k]]
    lines(years, s$cdf, col = sc$col, lwd = 2.2)
    my <- .cross(years, s$cdf, 0.5)                       # median exhaustion year
    if (!is.na(my)) points(my, 0.5, col = sc$col, pch = 19, cex = 0.9)
  }
  legend("topleft", bty = "n", cex = 0.85, lwd = 2.2,
         col = vapply(names(proj), function(k) .SCEN[[k]]$col, ""),
         legend = vapply(names(proj), function(k) .SCEN[[k]]$lab, ""))
  par(op); if (!is.null(file)) { dev.off(); message("wrote ", file) }
  invisible(proj)
}

# ---- SUPPORTING: bank fan (median + 90% band) -----------------------------------
plot_projection_fan <- function(proj, file = "figs/figure_projection_fan.pdf",
                                band_scenario = "-0.0287") {
  years <- proj[[1]]$years
  rib <- function(s) apply(s$Bpath, 2, quantile, probs = c(0.05, 0.5, 0.95))
  allB <- unlist(lapply(proj, function(s) rib(s)[c(1,3), ]))
  if (!is.null(file)) pdf(file, width = 7.0, height = 4.8, pointsize = 11)
  op <- par(mar = c(4.0, 4.4, 2.4, 1.0))
  plot(NA, xlim = range(years), ylim = range(0, allB, na.rm = TRUE),
       xlab = "Year", ylab = "Bank B (Mt)",
       main = "Bank trajectory fan (medians; 90% band on central)")
  abline(h = 0, col = "grey60", lty = 2)
  # 90% band on the chosen scenario
  if (!is.null(band_scenario) && band_scenario %in% names(proj)) {
    q <- rib(proj[[band_scenario]]); cc <- .SCEN[[band_scenario]]$col
    polygon(c(years, rev(years)), c(q[1, ], rev(q[3, ])),
            col = adjustcolor(cc, alpha.f = 0.13), border = NA)
  }
  for (k in names(proj)) {
    sc <- .SCEN[[k]]; q <- rib(proj[[k]])
    lines(years, q[2, ], col = sc$col, lwd = 2.2)         # median path
  }
  legend("topright", bty = "n", cex = 0.85, lwd = 2.2,
         col = vapply(names(proj), function(k) .SCEN[[k]]$col, ""),
         legend = vapply(names(proj), function(k) .SCEN[[k]]$lab, ""))
  par(op); if (!is.null(file)) { dev.off(); message("wrote ", file) }
  invisible(proj)
}

# ---- printed summary: median exhaustion + [5%,95%] + key probabilities ----------
projection_summary <- function(proj) {
  cat(sprintf("\n  Bank-exhaustion (R3 onset) summary  [sigma_y=%g, n=%d, MDI=%g Mt/yr]\n",
              attr(proj, "sigma_y"), attr(proj, "nsim"), attr(proj, "mdi_per_year")))
  cat("  scenario           B(2026)  median   [5%,    95%]   P(R3<=2035)  P(survive 2045)\n")
  for (k in names(proj)) {
    s <- proj[[k]]; yr <- s$years
    md  <- .cross(yr, s$cdf, 0.50); q05 <- .cross(yr, s$cdf, 0.05); q95 <- .cross(yr, s$cdf, 0.95)
    p35 <- s$cdf[which(yr == 2035)]; surv <- 1 - s$cdf[length(s$cdf)]
    fmt <- function(x) if (is.na(x)) ">2045" else as.character(x)
    cat(sprintf("  %-18s  %6.0f   %5s   [%5s, %5s]   %6.2f       %6.2f\n",
                .SCEN[[k]]$lab, s$B0, fmt(md), fmt(q05), fmt(q95), p35, surv))
  }
  invisible(NULL)
}