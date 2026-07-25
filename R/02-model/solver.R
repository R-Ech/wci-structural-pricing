# solver.R  (corrected)
# WCI allowance pricing: interior value S^int on the (B,t) plane.
#
#   interior PDE :  dS^int/dt + mu_B dS^int/dB = r S^int      (martingale)
#   bank drift   :  mu_B = a(t) - y(t) + Xi(price),  Xi(p)=p/gamma
#   price        :  S = max(S^int, R(t))                      (price recovery)
#
# Decoupling: S^int solved by the PDE alone, anchored at T to p*R(T) (baseline
# S^ext = R(T)); the floor is applied ONLY at output, never inside the loop, so
# S^int does not capitalize the escalator g_R > r into a spurious carry. The B=0
# scarcity-clearing edge stays in the loop (genuine scarcity value).
#
# Explicit upwind scheme: stable only under CFL  dt < dB / max|mu|.  Guarded below.

make_forcing <- function(gR = 0.075, R0 = 17.71, t0 = 2021,
                         L_ratio = 3.6693, g_const = 10) {
  list(
    R = function(t) R0 * exp(gR * (t - t0)),
    L = function(t) L_ratio * R0 * exp(gR * (t - t0)),
    g = function(t) g_const
  )
}

solve_price <- function(forcing, gamma, p, r, T_hor, t0,
                        Bmax = 800, NB = 400L, dt = 0.005, surface = FALSE,
                        cfl_check = TRUE, feedback = c("gross", "net"),
                        pbar_mult = 1) {
  feedback <- match.arg(feedback)
  B  <- seq(0, Bmax, length.out = NB + 1L); dB <- B[2] - B[1]
  nT <- as.integer(round((T_hor - t0) / dt))
  V  <- rep(p * forcing$R(T_hor), NB + 1L)   # terminal S^int = p*R(T), flat in B
  t  <- T_hor
  ts <- numeric(nT + 1L); ts[1] <- T_hor
  Vmat <- if (surface) matrix(NA_real_, nT + 1L, NB + 1L) else NULL
  if (surface) Vmat[1, ] <- V
  
  if (cfl_check) {
    Rmax     <- max(forcing$R(t0), forcing$R(T_hor))
    g_grid   <- sapply(seq(t0, T_hor, by = 0.25), forcing$g)
    Smax     <- max(p * forcing$R(T_hor), Rmax, max(g_grid))
    fb_bound <- if (feedback == "net")
      (forcing$L(T_hor) - forcing$R(t0)) / gamma
    else
      Smax / gamma
    mu_bound <- max(abs(g_grid)) + fb_bound
    dt_max   <- dB / max(mu_bound, 1e-8)
    if (dt > dt_max)
      warning(sprintf("CFL: dt=%.4g exceeds stable dt_max=%.4g (dB=%.3g, max|mu|~%.3g). Reduce dt.",
                      dt, dt_max, dB, mu_bound))
  }
  stopifnot(is.finite(gamma), is.finite(r), length(r) == 1L)
  
  for (n in seq_len(nT)) {
    g <- forcing$g(t); R <- forcing$R(t); L <- forcing$L(t)
    price <- pmax(V, R)
    xi    <- if (feedback == "net") pmax(pmin(price, L) - pbar_mult * R, 0) / gamma else price / gamma
    mu    <- g + xi
    VB <- numeric(NB + 1L)
    j  <- 2:NB
    VB[j] <- ifelse(mu[j] > 0, (V[j + 1L] - V[j]) / dB,
                    (V[j] - V[j - 1L]) / dB)
    VB[NB + 1L] <- 0
    VB[1]       <- if (mu[1] > 0) (V[2] - V[1]) / dB else 0   # outflow: no interior characteristic
    V <- V + dt * (mu * VB - r * V)
    if (g < 0) {
      bc   <- min((if (feedback == "net") pbar_mult * R else 0) + gamma * (-g), L)
      V[1] <- if (mu[1] > 0) max(V[1], bc) else bc            # inflow: S^cont vs clearing; outflow: clearing pins
    }
    t <- t - dt; ts[n + 1L] <- t
    if (surface) Vmat[n + 1L, ] <- V
  }
  S <- pmax(V, forcing$R(t))
  list(B = B, t = t, Sint = V, price = S, ts = ts, Sint_surface = Vmat)
}

sint_at <- function(res, B_query, t_query) {
  if (is.null(res$Sint_surface)) stop("solve with surface = TRUE")
  it <- which.min(abs(res$ts - t_query))
  approx(res$B, res$Sint_surface[it, ], xout = B_query, rule = 2)$y
}

price_at <- function(res, B_query) approx(res$B, res$price, B_query, rule = 2)$y

test_solver <- function() {
  f <- make_forcing(g_const = +10)
  r <- 0.045; T_hor <- 2045; t0 <- 2021
  cat(sprintf("floor R(2021) = %.2f\n", f$R(t0)))
  for (p in c(0.05, 0.5, 0.9)) {
    res <- solve_price(f, gamma = 1e12, p = p, r = r, T_hor = T_hor, t0 = t0)
    analytic <- p * f$R(T_hor) * exp(-r * (T_hor - t0))
    cat(sprintf("p=%.2f : Sint=%.3f  price=%.3f  (analytic Sint %.2f)  -> %s\n",
                p, mean(res$Sint), mean(res$price), analytic,
                if (mean(res$price) <= f$R(t0) + 1e-6) "FLOORED (Regime 1)" else "interior (Regime 2)"))
  }
}

verify_solver <- function() {
  r <- 0.045; T_hor <- 2045; t0 <- 2021
  f1 <- make_forcing(g_const = +10)
  res1 <- solve_price(f1, gamma = 1e12, p = 0.7, r = r, T_hor = T_hor, t0 = t0, surface = TRUE)
  got  <- mean(res1$Sint); want <- 0.7 * f1$R(T_hor) * exp(-r * (T_hor - t0))
  cat(sprintf("[1] degenerate discounting: got %.4f  want %.4f  err %.2e  %s\n",
              got, want, abs(got - want), if (abs(got - want) < 1e-2) "PASS" else "FAIL"))
  f2 <- make_forcing(g_const = +10)
  res2 <- solve_price(f2, gamma = 6.1, p = 0.05, r = r, T_hor = T_hor, t0 = t0)
  cat(sprintf("[2] floor respect (p=0.05): min price %.3f vs R(t0) %.3f  %s\n",
              min(res2$price), f2$R(t0),
              if (min(res2$price) >= f2$R(t0) - 1e-6) "PASS" else "FAIL"))
  vals <- sapply(c(0.2, 0.5, 0.9), function(pp)
    mean(solve_price(make_forcing(g_const = +10), gamma = 6.1, p = pp,
                     r = r, T_hor = T_hor, t0 = t0)$Sint))
  cat(sprintf("[3] monotone in p: Sint(0.2,0.5,0.9) = %.3f %.3f %.3f  %s\n",
              vals[1], vals[2], vals[3],
              if (all(diff(vals) > -1e-6)) "PASS" else "FAIL"))
  pr <- sapply(list(c(400L, 0.005), c(800L, 0.0025)), function(cfg)
    price_at(solve_price(make_forcing(g_const = +10), gamma = 6.1, p = 0.7,
                         r = r, T_hor = T_hor, t0 = t0,
                         NB = as.integer(cfg[[1]]), dt = cfg[[2]]), B_query = 400))
  cat(sprintf("[4] refinement (price@B=400): coarse %.4f  fine %.4f  rel-diff %.2e  %s\n",
              pr[1], pr[2], abs(pr[1]-pr[2])/pr[2],
              if (abs(pr[1]-pr[2])/pr[2] < 5e-3) "PASS" else "CHECK"))
}

if (sys.nframe() == 0) { test_solver(); cat("\n--- verification ---\n"); verify_solver() }