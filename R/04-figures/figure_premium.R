# figure_premium.R
# The premium-over-reserve distribution across the 45 WCI joint auctions (2015-2026).
# Shows the regime partition of Proposition (regimes) read directly off the primary
# market: a mass point at EXACTLY the reserve (the nine undersubscribed, floor-binding
# auctions), a near-reserve floor cluster, an EMPTY GAP at ~1.7-3.1%, and then the
# interior-regime continuum. The classification cutoff falls in the data gap; it is not
# imposed. No calibrated parameter enters.
#
#   source("Scripts/figure_premium.R"); plot_premium()              # uses built-in data
#   source("Scripts/figure_premium.R"); plot_premium(aud$premium)   # or pass your vector

# premium over reserve, averaged across current+advance legs, 45 auctions 2015Q2-2026Q2
.PREMIUM <- c(0.0157,0.0347,0.0521,0,0,0,0,0,0.0169,0.0870,0.110,0.00551,0.00826,
              0.0358,0.0537,0.00704,0.117,0.0986,0.0883,0.0713,0,0,0.0150,0.00508,
              0.0615,0.316,0.596,0.480,0.566,0.371,0.360,0.254,0.366,0.585,0.744,
              0.737,0.540,0.258,0.327,0.131,0,0.112,0.0947,0,0.0311)

plot_premium <- function(prem = .PREMIUM, file = "figs/figure_premium.pdf") {
  brk <- c(-0.001, 0.001, 0.017, 0.031, 0.05, 0.10, 0.15, 0.30, 0.45, 0.60, 0.75)
  lab <- c("=0", "0-1.7", "1.7-3.1", "3.1-5", "5-10", "10-15",
           "15-30", "30-45", "45-60", "60-75")
  cnt <- as.integer(table(cut(prem, breaks = brk, right = FALSE)))
  names(cnt) <- lab
  # colours: exact-reserve spike (deep red), near-floor cluster (pale red),
  # gap (drawn empty), interior continuum (steel)
  cols <- c("#b4451f", "#e0a899", "#ffffff",
            rep("#34506e", length(lab) - 3))

  if (!is.null(file)) pdf(file, width = 7.2, height = 4.6, pointsize = 11)
  op <- par(mar = c(4.6, 4.2, 2.4, 1.0))
  bp <- barplot(cnt, col = cols, border = "#5a6b80", space = 0.15,
                ylim = c(0, max(cnt) + 2), las = 1,
                ylab = "Number of auctions", xlab = "",
                main = "Premium over reserve: 45 WCI joint auctions, 2015--2026")
  # exact-reserve annotation
  text(bp[1], cnt[1] + 1.1, "9 cleared at\nexactly the\nreserve",
       cex = 0.72, col = "#b4451f", font = 2)
  # gap marker on the empty bar
  text(bp[3], 1.4, "gap", srt = 0, cex = 0.74, col = "grey45", font = 3)
  text(bp[3], 0.7, "(no\nmass)", cex = 0.6, col = "grey55")
  rect(bp[3] - 0.45, 0, bp[3] + 0.45, 0.05, border = "grey60", lty = 3)
  # regime brackets under the axis
  mtext("floor regime", side = 1, line = 2.5, at = mean(bp[1:2]),
        cex = 0.78, col = "#b4451f")
  mtext("interior regime", side = 1, line = 2.5, at = mean(bp[4:10]),
        cex = 0.78, col = "#34506e")
  mtext("Settlement premium over reserve (%)", side = 1, line = 3.6, cex = 0.85)
  par(op); if (!is.null(file)) { dev.off(); message("wrote ", file) }

  cat(sprintf("floor regime (premium <= 1.7%%): %d auctions (%d at exactly the reserve)\n",
              sum(cnt[1:2]), cnt[1]))
  cat(sprintf("gap [1.7%%, 3.1%%): %d auctions\n", cnt[3]))
  cat(sprintf("interior regime (premium >= 3.1%%): %d auctions\n", sum(cnt[4:10])))
  invisible(cnt)
}
