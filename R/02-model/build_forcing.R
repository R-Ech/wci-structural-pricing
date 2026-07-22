# build_forcing.R
# Construct the model's deterministic forcing from cleanCap + the IEMAC level anchor.
#
#   g(t) = a(t) - y(t)  = ( Allocations + Auctions(sold) - Emissions + offset_use ) [Mt/yr]
#   B(t) = B_anchor + cumulative g, forward from the anchor year                   [Mt]
#
# Offsets ADD to g (they satisfy obligation without drawing the allowance bank, so
# allowance surrenders = Emissions - offset_use). Pre-anchor history -- scope
# expansion, undersold 2016-17 auctions, the ~159 Mt cumulative offsets -- is
# absorbed in B_anchor; we integrate only the clean broad-scope flows from 2021 on.
#
# Returns the forcing list solve_price() expects (g, R, L) plus B(t) and diagnostics.
# Units: cleanCap is raw instruments; everything here is converted to Mt.

build_forcing <- function(cleanCap,
                          B_anchor    = 379.1,        # Mt: IEMAC net surplus at end of anchor_year
                          anchor_year = 2024,
                          offset_use  = NULL,       # named numeric, Mt/yr for 2021+; NULL -> 0
                          g_y        = -0.02869,       # central emissions drift (Mt/yr) past data; scenarios {-3.2,-5.2,-11.7}
                          # floor / ceiling escalator (see reconciliation note below):
                          R0 = 17.71, gR = 0.075, base_year = 2021, L0 = 65) {

  Mt   <- function(x) x / 1e6
  yrs  <- as.integer(rownames(cleanCap))
  infl <- setNames(Mt(cleanCap$Allocations + cleanCap$Auctions - cleanCap$Emissions), yrs)

  off <- setNames(rep(0, length(yrs)), yrs)        # offsets, Mt/yr (default 0)
  if (!is.null(offset_use)) off[names(offset_use)] <- offset_use
  g_year <- infl + off                             # offsets add to drift

  keep <- yrs > anchor_year                        # observed flows strictly after the anchor
  gk   <- g_year[keep]; yk <- yrs[keep]
  last_yr <- anchor_year                           # forward projection starts at the anchor year
  cat("last_yr =", last_yr, " length(gk) =", length(gk), " length(yk) =", length(yk), "\n")
  
  B_series <- setNames(B_anchor, anchor_year)      # seed; forward years appended below
  if (length(yk)) {                                # only if observed post-anchor years exist
    B_series <- c(B_series, setNames(B_anchor + cumsum(gk), yk))
    last_yr  <- max(yk)
  }

  # ---- functions of calendar time t ----
  # NEW --------------------------------------------------------------
  # forward forcing: declining CARB cap a(t) minus projected emissions y(t).
  # a(t): legislated annual budget [Mt]. 2026-2031 from CARB; post-2031 linear at
  #       -6.7/yr to exactly 100 Mt in 2045 (AB 1279 / 2022 Scoping Plan path).
  # y(t): central-drift projection anchored at last observed emissions; g_y [%/yr].
  y_last  <- as.numeric(Mt(cleanCap$Emissions))[length(yrs)]    # last observed covered emissions [Mt]
  # Adopted CA GHG Allowance Budgets, Table 6-2 (2026 amendments, eff. Sept 1 2026).
  .a_path <- c(`2026`=254.0, `2027`=225.6, `2028`=200.8, `2029`=178.8, `2030`=158.8,
               `2031`=147.3, `2032`=136.8, `2033`=127.1, `2034`=118.1, `2035`=109.6,
               `2036`=101.5, `2037`= 93.7, `2038`= 86.0, `2039`= 78.4, `2040`= 70.8,
               `2041`= 63.1, `2042`= 55.3, `2043`= 47.3, `2044`= 39.0, `2045`= 30.3)
  a_budget <- function(t) as.numeric(.a_path[as.character(min(max(floor(t), 2026), 2045))])
  y_proj <- function(t) y_last*exp(g_y*(floor(t)-last_yr))
  g_fun <- function(t) { yf <- floor(t)
  if (yf <= last_yr)   { if (length(gk)) as.numeric(gk[as.character(min(max(yf, min(yk)), last_yr))]) else 0 }
  else if (yf < 2026)  a_budget(2026) - y_proj(2026)   # bridge 2025 (no observed flow, no 2025 budget)
  else                 a_budget(t) - y_proj(t) } # forward: a(t)-y(t)
  # extend deterministic B(t) to 2045 under the central drift, then interpolate
  fwd_yrs  <- (last_yr + 1):2045
  B_series <- c(B_series,
                setNames(as.numeric(B_series[as.character(last_yr)]) + cumsum(sapply(fwd_yrs, g_fun)),
                         fwd_yrs))
  B_fun <- function(t) approx(as.numeric(names(B_series)), B_series, xout = t, rule = 2)$y
  cat(sprintf("  central path (g_y=%.5f): B(2026)=%.0f  B(2045)=%.0f  %s\n", g_y,
              B_fun(2026), B_fun(2045),
              { e <- as.numeric(names(B_series))[which(B_series <= 0)[1]]
              if (is.na(e)) "no exhaustion <=2045" else sprintf("exhausts ~%d", e) }))
  R_fun <- function(t) R0 * exp(gR * (t - base_year))
  L_fun <- function(t) L0 * exp(gR * (t - base_year))

  cat(sprintf("Net drift g(t) [Mt/yr], post-%d:\n", anchor_year)); print(round(gk, 2))
  cat(sprintf("\nImplied bank B(t) [Mt], anchored %d = %.0f:\n", anchor_year, B_anchor))
  print(round(B_series, 1))

  list(g = g_fun, R = R_fun, L = L_fun, B = B_fun, g_year = gk, B_series = B_series)
}

# ---- 4%-regulatory-cap offset sensitivity (use instead of NULL to bound the drift) ----
# Offset use is capped at 4% of obligation in 2021-2025; realized was ~3%. Build the
# forcing at zero and at the cap; if the regime classification is unchanged between
# them, offsets provably don't matter and the exact annual series is unnecessary.
#   off_cap <- setNames(0.04 * (cleanCap$Emissions[ as.integer(rownames(cleanCap)) > 2020 ]/1e6),
#                       rownames(cleanCap)[ as.integer(rownames(cleanCap)) > 2020 ])
#   f_hi <- build_forcing(cleanCap, offset_use = off_cap)

# ---- usage ----
# source("solver.R"); source("build_forcing.R")
# f <- build_forcing(cleanCap)
# res <- solve_price(f, gamma = <calib>, p = <calib>, r = 0.045, T_hor = 2045, t0 = 2021)
# price_at(res, f$B(2021))           # model price at the observed 2021 bank
