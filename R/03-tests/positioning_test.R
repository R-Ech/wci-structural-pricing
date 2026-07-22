# positioning_test.R
# Formal inference for the positioning <-> regime co-movement (referee: empirical #4).
# A permutation test is appropriate here because auction dates are non-overlapping, so
# observations are exchangeable under the null of no association -- unlike the rolling
# realized-vol correlation, where overlap would make a naive bootstrap overstate precision.
#
# Null: positioning is independent of the pricing regime; the observed Spearman rho could
# arise by chance reassignment of positioning to auctions. We permute the positioning series
# across auctions and recompute rho B times.
#
# Input: a data.frame `aud` with columns
#   position : net speculative positioning at the auction (managed money + other reportable)
#   regime   : ordered/numeric regime code, OR premium (a monotone proxy: higher premium = more interior)
# Pass regime_is_premium = TRUE if you give it the premium column.
#
#   source("Scripts/positioning_test.R")
#   perm_test(aud$position, aud$premium, regime_is_premium = TRUE)              # all auctions
#   perm_test(aud$position, aud$premium, regime_is_premium = TRUE, drop = idx)  # excl. 2025-26 touches

perm_test <- function(position, regime, regime_is_premium = FALSE,
                      drop = NULL, B = 50000, seed = 1) {
  if (!is.null(drop)) { position <- position[-drop]; regime <- regime[-drop] }
  ok <- is.finite(position) & is.finite(regime)
  position <- position[ok]; regime <- regime[ok]
  n <- length(position)
  # regime as a rank variable: premium is a monotone proxy for "how interior", so
  # Spearman on (position, premium) equals Spearman on (position, regime rank).
  x <- regime
  rho_obs <- suppressWarnings(cor(position, x, method = "spearman"))

  set.seed(seed)
  rho_perm <- numeric(B)
  for (b in seq_len(B))
    rho_perm[b] <- suppressWarnings(cor(sample(position), x, method = "spearman"))

  # two-sided and one-sided (positive) permutation p-values
  p_two <- (1 + sum(abs(rho_perm) >= abs(rho_obs))) / (B + 1)
  p_pos <- (1 + sum(rho_perm >= rho_obs)) / (B + 1)
  ci <- quantile(rho_perm, c(0.025, 0.975))

  cat(sprintf("\n  n = %d auctions\n", n))
  cat(sprintf("  observed Spearman rho        : %+.3f\n", rho_obs))
  cat(sprintf("  permutation null mean [95%%]  : %+.3f  [%+.3f, %+.3f]\n",
              mean(rho_perm), ci[1], ci[2]))
  cat(sprintf("  permutation p (one-sided, +) : %.5f\n", p_pos))
  cat(sprintf("  permutation p (two-sided)    : %.5f\n", p_two))
  cat(sprintf("  B = %d permutations\n", B))
  invisible(list(rho = rho_obs, p_two = p_two, p_pos = p_pos, null_ci = ci, n = n))
}
