# =============================================================================
# solve_reserve.R
# Deterministic solver for Proposition "Price path with a reserve"
# (model_skeleton.tex, eq. reserve-system). Base R only. Defines functions and
# touches no existing object. Source it, then call solve_path().
#
# Model, from date t0:
#   price before exhaustion   S(t) = S0 * exp(rho * (t - t0))
#   auction volume sold       q(t) if S(t) >= R(t), 0 otherwise
#   bank                      dB/dt = c(t) + q_sold(t) - y(t) + S(t) / gamma
#   exhaustion at tau         B(tau) = 0
#   price continuity at tau   S(tau) = min(gamma * (y - c - q), L) at tau
#
# Unknowns: S0 and tau. Setting rho = r, R = 0 and L = Inf gives the Rubin
# baseline (Proposition "Baseline price path").
#
# Units: time in years (calendar dates as decimals), quantities in Mt per year,
# prices in dollars per ton, gamma in dollars per ton per (Mt per year).
# =============================================================================

# --- helpers -----------------------------------------------------------------

# cumulative trapezoid integral of f on grid t, starting at zero
cumtrapz <- function(t, f) {
  c(0, cumsum(diff(t) * (head(f, -1) + tail(f, -1)) / 2))
}

# first grid crossing of zero from above, linearly interpolated. NA if none.
first_zero <- function(t, B) {
  k <- which(B <= 0)[1]
  if (is.na(k)) return(NA_real_)
  if (k == 1) return(t[1])
  t[k - 1] + (t[k] - t[k - 1]) * B[k - 1] / (B[k - 1] - B[k])
}

# Bank path implied by an initial price S0 when the price grows at rho over the
# whole horizon (the auxiliary bank of the proofs, eq. auxbank and auxbankR).
aux_bank <- function(S0, p) {
  S    <- S0 * exp(p$rho * (p$t - p$t0))
  sold <- as.numeric(S >= p$R)
  flow <- p$c + p$q * sold - p$y + S / p$gamma
  B    <- p$B0 + cumtrapz(p$t, flow)
  # reserve tiers: the whole volume of a tier enters the bank on the first date the
  # price reaches the tier price (attack on a finite reserve, Hasegawa and Salant 2014)
  t_tier <- rep(NA_real_, length(p$tierV))
  for (j in seq_along(p$tierV)) {
    k <- which(S >= p$tierK[[j]])[1]
    if (!is.na(k)) { B[k:length(B)] <- B[k:length(B)] + p$tierV[j]; t_tier[j] <- p$t[k] }
  }
  list(S = S, sold = sold, B = B, t_tier = t_tier)
}

# --- main solver -------------------------------------------------------------

# Arguments
#   B0      bank at t0 (Mt)
#   t0, T   start date and horizon (decimal years)
#   c_fun   free allocation rate, function of t (Mt/yr)
#   q_fun   auction volume rate, function of t (Mt/yr)
#   y_fun   BAU emissions rate, function of t (Mt/yr)
#   R_fun   auction reserve, function of t ($/t). Use function(t) 0 * t for none.
#   L_fun   price ceiling, function of t ($/t). Use function(t) Inf + 0 * t for none.
#   gamma   abatement cost slope
#   rho     required return before a curtailment, r + lambda * ell
#   dt      time step in years
#   tiers   NULL, or list(K = list(K1_fun, K2_fun), V = c(V1, V2)): tier prices as functions
#           of t ($/t) and tier volumes (Mt). A tier is bought in full when the price reaches it.
#   after   TRUE to compute the path after the bank empties at a tier (slower). S0 and tau do not need it.
# Value: list with S0, tau, tau_R, regime, flags and the path as a data frame.
solve_path <- function(B0, t0, T, c_fun, q_fun, y_fun, R_fun, L_fun,
                       gamma, rho, dt = 1 / 365, tol = 1e-10, tiers = NULL, after = TRUE) {

  t <- seq(t0, T, by = dt)
  if (tail(t, 1) < T) t <- c(t, T)
  p <- list(t = t, t0 = t0, B0 = B0, gamma = gamma, rho = rho,
            c = c_fun(t), q = q_fun(t), y = y_fun(t), R = R_fun(t), L = L_fun(t),
            tierV = if (is.null(tiers)) numeric(0) else tiers$V,
            tierK = if (is.null(tiers)) list() else lapply(tiers$K, function(f) f(t)))
  G <- gamma * (p$y - p$c - p$q)          # clearing price with all supply sold
  flags <- character(0)

  # minimum of the auxiliary bank, increasing in S0
  m <- function(S0) min(aux_bank(S0, p)$B)

  # the cap must bind: with a price near zero the bank has to run out before T
  lo <- 1e-8
  if (m(lo) >= 0)
    stop("The bank is never exhausted even with a zero price. The cap does not bind before T.")
  hi <- 1
  while (m(hi) <= 0) {
    hi <- hi * 2
    if (hi > 1e7) stop("No initial price keeps the bank non-negative. Check units and inputs.")
  }

  # Case 1: the bank lands flat (minimum of the auxiliary bank equals zero)
  S0_flat  <- uniroot(m, c(lo, hi), tol = tol)$root
  ab       <- aux_bank(S0_flat, p)
  ride     <- FALSE
  if (length(p$tierV) > 0 && min(ab$B) > 1e-3) {
    # With tiers the minimum of the bank can jump in S0. The root is then the price at which
    # the bank empties exactly when the price reaches a tier. Step just below it.
    ride <- TRUE
    for (i in 1:60) { S0_flat <- S0_flat * (1 - 1e-7); ab <- aux_bank(S0_flat, p); if (min(ab$B) <= 0) break }
  }
  fz       <- first_zero(t, ab$B)
  if (length(p$tierV) > 0 && !ride) {
    # v3: uniroot can also stop just below the jump, where the minimum of the bank is close to
    # zero and the case used to be read as a flat landing. It is not one if the bank is still
    # falling when it empties and the price is at a tier price on that date.
    kz <- if (is.na(fz)) which.min(ab$B) else which.min(abs(t - fz)); Sz <- S0_flat * exp(rho * (t[kz] - t0))
    at_tier <- any(sapply(p$tierK, function(K) abs(log(Sz / K[kz])) < 3 * rho * dt + 1e-6))
    if (at_tier && Sz < G[kz] * (1 - 1e-3)) ride <- TRUE
  }
  k_flat   <- if (ride && !is.na(fz)) which.min(abs(t - fz)) else which.min(ab$B)
  tau_flat <- t[k_flat]
  S_at_tau <- S0_flat * exp(rho * (tau_flat - t0))

  if (k_flat == length(t))
    flags <- c(flags, "Minimum of the bank is at T. Exhaustion before T is assumed in the proposition.")

  if (S_at_tau <= p$L[k_flat]) {
    S0 <- S0_flat; tau <- tau_flat
    regime <- if (k_flat == length(t)) "bank lasts until T (horizon binds)" else "abatement clears at exhaustion"
    if (ride) {
      # The bank empties exactly when the price reaches a tier. S0 and tau are the equilibrium values.
      # After tau the price follows the tier price until the tier is sold. The returned path does not show that.
      j_ride <- which.min(sapply(p$tierK, function(K) abs(log(S_at_tau / K[k_flat]))))
      regime <- sprintf("bank empties at reserve tier %d", j_ride)
    }
    ride_ok <- ride
  } else {
    # Case 2: the price reaches the ceiling at exhaustion. Solve
    # S0 * exp(rho * (tau(S0) - t0)) = L(tau(S0)), tau(S0) the first empty date.
    h <- function(S0) {
      tau_S <- first_zero(t, aux_bank(S0, p)$B)
      if (is.na(tau_S)) return(Inf)
      S0 * exp(rho * (tau_S - t0)) - approx(t, p$L, tau_S)$y
    }
    up   <- S0_flat * (1 - 1e-9)
    h_up <- h(up)
    if (!is.finite(h_up)) { up <- S0_flat * (1 - 1e-6); h_up <- h(up) }
    if (is.finite(h_up) && h_up > 0) {
      S0     <- uniroot(h, c(lo, up), tol = tol)$root
      tau    <- first_zero(t, aux_bank(S0, p)$B)
      regime <- "ceiling binds at exhaustion"
    } else {
      # The landing price is at the ceiling to within the grid. The two cases coincide.
      S0 <- S0_flat; tau <- tau_flat
      regime <- "ceiling binds at exhaustion"
      if (S_at_tau > 1.01 * p$L[k_flat])
        flags <- c(flags, "Landing price is more than 1% above the ceiling and no ceiling solution was found. Single exhaustion probably fails.")
    }
  }

  if (!exists("ride_ok", inherits = FALSE)) ride_ok <- FALSE
  # --- assemble the path -----------------------------------------------------
  ab    <- aux_bank(S0, p)
  pre   <- t < tau
  S     <- ifelse(pre, ab$S, pmin(pmax(G, 0), p$L))
  B     <- ifelse(pre, pmax(ab$B, 0), 0)
  sold  <- ifelse(pre, ab$sold, 1)
  e     <- p$y - S / gamma                       # emissions
  ceil  <- ifelse(pre, 0, pmax(e - p$c - p$q, 0)) # ceiling units sold after tau
  below <- pre & ab$sold == 0
  tau_R <- if (any(below)) max(t[below]) else t0  # last date with unsold auctions

  # --- after the bank empties at a tier (v3) -----------------------------------
  # The price follows the tier price K_j and the tier covers the gap between emissions and the
  # cap. The tier would run out with the price below the clearing price, so what is left of it
  # is bought in full at a date t_attack (second attack), the price grows at rho again, and a
  # second bank is exhausted at tau_final. S0 and tau are not affected by any of this.
  t_attack <- NA_real_; tau_final <- tau; regime_final <- regime
  if (ride_ok && after) {
    Kj   <- p$tierK[[j_ride]]
    post <- which(t >= tau)
    d    <- (p$y - Kj / gamma - p$c - p$q)[post]            # tier sales per year while the price rides
    left <- p$tierV[j_ride] - cumtrapz(t[post], pmax(d, 0))
    k_end <- which(left <= 0 | d <= 0)[1]; if (is.na(k_end)) k_end <- length(post)
    rest <- if (j_ride < length(p$tierV)) list(K = tiers$K[-seq_len(j_ride)], V = tiers$V[-seq_len(j_ride)]) else NULL
    sub_at <- function(ta) suppressWarnings(solve_path(
      B0 = approx(t[post], left, ta)$y, t0 = ta, T = T, c_fun = c_fun, q_fun = q_fun, y_fun = y_fun,
      R_fun = R_fun, L_fun = L_fun, gamma = gamma, rho = rho, dt = dt, tol = tol, tiers = rest))
    gap <- function(ta) tryCatch(sub_at(ta)$S0 - approx(t, Kj, ta)$y, error = function(e) NA_real_)
    lo_t <- t[post][2]; hi_t <- t[post][max(k_end - 1, 3)]
    g_lo <- gap(lo_t); g_hi <- gap(hi_t)
    if (d[k_end] <= 0 && left[k_end] > 0) {
      # the clearing price comes down to the tier price before the tier is sold: no second attack
      S[post[1:k_end]] <- Kj[post[1:k_end]]; tau_final <- t[post[k_end]]; regime_final <- "abatement clears at exhaustion"
    } else if (is.finite(g_lo) && is.finite(g_hi) && g_lo < 0 && g_hi > 0) {
      t_attack <- uniroot(gap, c(lo_t, hi_t), tol = 1e-6)$root
      sub <- sub_at(t_attack)
      a1 <- t >= tau & t < t_attack; a2 <- t >= t_attack
      S[a1] <- Kj[a1]
      S[a2] <- approx(sub$path$t, sub$path$S, t[a2], rule = 2)$y
      B[a2] <- approx(sub$path$t, sub$path$B, t[a2], rule = 2)$y
      tau_final <- sub$tau; regime_final <- sub$regime
      e <- p$y - S / gamma; ceil <- ifelse(t < tau_final, 0, pmax(e - p$c - p$q, 0))
    } else {
      S[post[1:k_end]] <- Kj[post[1:k_end]]
      flags <- c(flags, "Bank empties at a tier and the date of the second attack was not found. The path after tau is approximate.")
    }
  }

  # --- checks on the hypotheses of the proposition ---------------------------
  if (any(diff(ab$sold[pre]) < 0))
    flags <- c(flags, "The price falls back below the reserve before tau. Single crossing fails (is rho > g_R on this path?).")
  if (tau_R >= tau)
    flags <- c(flags, "tau_R >= tau. The bank is exhausted while auctions are unsold, a case the proposition excludes.")
  post <- which(!pre & t < T)
  if (length(post) > 2) {
    g_post <- diff(log(pmax(S[post], 1e-12))) / diff(t[post])
    if (any(g_post > rho + 1e-6))
      flags <- c(flags, "After tau the clearing price grows faster than rho at some dates. Banking would restart.")
  }
  if (any(e < 0))
    flags <- c(flags, "Emissions are negative at some dates (abatement exceeds BAU). The constraint e >= 0 is not imposed.")
  # the auxiliary bank always recovers late (the price keeps growing), so only
  # downward crossings count
  nz <- sum(diff(as.numeric(aux_bank(S0 * 0.98, p)$B > 0)) < 0)
  if (nz > 1)
    flags <- c(flags, "At a slightly lower price the bank falls through zero more than once. Single exhaustion may fail.")
  if (length(flags) > 0) for (f in flags) warning(f, call. = FALSE)

  t_tier <- ab$t_tier
  t_tier[!is.na(t_tier) & t_tier >= tau] <- NA      # a tier reached after exhaustion is not used here
  if (ride_ok) t_tier[j_ride] <- tau                # the tier the bank empties at is reached at tau
  if (length(p$tierV) > 0 && any(is.na(t_tier)) && regime != "bank lasts until T (horizon binds)") {
    k_tau <- which.min(abs(t - tau))
    for (j in which(is.na(t_tier)))
      if (S0 * exp(rho * (tau - t0)) > p$tierK[[j]][k_tau] * 1.001)
        flags <- c(flags, sprintf("Price at exhaustion is above tier %d, which was not counted. Check.", j))
  }
  list(S0 = S0, tau = tau, tau_R = tau_R, t_tier = t_tier, regime = regime,
       t_attack = t_attack, tau_final = tau_final, regime_final = regime_final,
       S_at_tau = S0 * exp(rho * (tau - t0)), flags = flags,
       path = data.frame(t = t, S = S, R = p$R, L = p$L, B = B,
                         sold = sold, emissions = e, ceiling_units = ceil))
}

# --- validation against a closed form ----------------------------------------
# Constant deficit d = y - a, no reserve, no ceiling. The system reduces to
#   B0 - d * tau + d * (1 - exp(-r * tau)) / r = 0,   S0 = gamma * d * exp(-r * tau).
validate_baseline <- function(B0 = 300, d = 40, gamma = 5, r = 0.04, T = 40) {
  tau_cf <- uniroot(function(x) B0 - d * x + d * (1 - exp(-r * x)) / r,
                    c(1e-6, T), tol = 1e-12)$root
  S0_cf  <- gamma * d * exp(-r * tau_cf)
  out <- solve_path(B0 = B0, t0 = 0, T = T,
                    c_fun = function(t) 100 + 0 * t, q_fun = function(t) 0 * t,
                    y_fun = function(t) 100 + d + 0 * t,
                    R_fun = function(t) 0 * t, L_fun = function(t) Inf + 0 * t,
                    gamma = gamma, rho = r)
  c(S0_closed_form = S0_cf, S0_solver = out$S0,
    tau_closed_form = tau_cf, tau_solver = out$tau)
}
