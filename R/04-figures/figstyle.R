# figstyle.R  ---------------------------------------------------------------
# Shared publication style for the WCI banking figures. Source this once at the
# top of every figure script; it fixes fonts, palette, margins, axis idiom,
# titles and legends so the whole set is visually uniform.
#
#   source("figstyle.R")
#   fig_open("figs/x.pdf", 6.8, 4.6); <plot>; fig_close()
#
# Everything is base graphics (no ggplot dependency), matching the pipeline.
# ---------------------------------------------------------------------------

# ---- typography ------------------------------------------------------------
# Latin Modern Roman = LaTeX Computer Modern, so figure text matches a CM
# manuscript. Swap FIG_FAMILY to "sans" (DejaVu Sans) or "Helvetica" if the
# paper is set in a sans face. Cairo is required for the embedded family.
FIG_FAMILY <- getOption("fig.family", "Latin Modern Roman")
FIG_PS     <- getOption("fig.pointsize", 10)

.fig_have_cairo <- isTRUE(capabilities("cairo"))

# ---- palette ---------------------------------------------------------------
# One coherent, print-safe qualitative set (navy anchor + warm/cool spokes),
# plus neutral greys for reference lines and de-emphasised furniture.
PAL <- list(
  ink    = "#1c1c1c",   # axes, text, main series when monochrome
  navy   = "#2f4b6e",   # primary series  (was "steelblue"/"#34506e")
  rust   = "#b4451f",   # accent / "slow" scenario / model line
  teal   = "#2e7d5b",   # secondary accent / "fast" scenario
  gold   = "#c0871a",   # 4th series / "faster"
  plum   = "#6c4a86",   # 5th series / "fastest"
  steel  = "#7f9bbd",   # light navy for fills / early gradient stop
  ref    = "#9a9a9a",   # reference lines (floor, medians)  -- grey, dashed
  grid   = "#e7e7e7",   # hairline gridlines
  sub    = "#666666",   # subtitle / caption grey
  band   = "#2f4b6e"    # ribbon base colour (used with alpha)
)

# sequential ramp for the persp surface and B-slices (pale -> navy)
PAL_SEQ <- c("#f3f6fa", "#c9d6e6", "#8aa2c0", "#4f6f97", "#2f4b6e")

# scenario colour map, keyed by g_y (as character), shared by CDF + fan so a
# given decarb speed is the SAME colour in every figure.
SCEN_COL <- c(
  "-0.02"   = PAL$rust,
  "-0.0287" = PAL$navy,
  "-0.045"  = PAL$teal,
  "-0.06"   = PAL$gold,
  "-0.08"   = PAL$plum)

scen_col <- function(key) {
  k <- as.character(key)
  if (k %in% names(SCEN_COL)) SCEN_COL[[k]] else PAL$ink
}

col_alpha <- function(col, a) grDevices::adjustcolor(col, alpha.f = a)

# concatenate a list of length-1 expressions into one expression vector,
# suitable for legend(legend = ...) with plotmath labels.
exprvec <- function(lst) Reduce(function(a, b) c(a, b), lst)

# ---- expression label helpers ----------------------------------------------
# Render the literals used in the scripts as proper math, uniformly.
lab_lambdaell <- function() expression(lambda*ell)
lab_S         <- function() expression(S^"*")
lab_Sbt       <- function() expression(S^"*"*(B*","*t))
lab_dollar_t  <- function() "$/tonne"

lab_gy <- function(v) {                       # g_y = -2.87%/yr, subscripted
  as.expression(bquote(g[y] == .(sprintf("%.2f", 100*v)) * "%/yr"))
}

# ---- device open / close ---------------------------------------------------
FIG_MAR <- c(4.0, 4.2, 2.6, 1.2)              # uniform outer margins (b,l,t,r)

fig_open <- function(file, width = 6.8, height = 4.6) {
  if (is.null(file)) return(invisible(FALSE))
  if (.fig_have_cairo) {
    grDevices::cairo_pdf(file, width = width, height = height,
                         pointsize = FIG_PS, family = FIG_FAMILY)
  } else {
    grDevices::pdf(file, width = width, height = height, pointsize = FIG_PS)
  }
  fig_par()
  invisible(TRUE)
}

fig_close <- function(file = NULL) {
  grDevices::dev.off()
  if (!is.null(file)) message("wrote ", file)
  invisible(NULL)
}

# base par theme: thin outward ticks, tight label gaps, horizontal y-labels off
fig_par <- function(...) {
  graphics::par(mar = FIG_MAR, mgp = c(2.4, 0.55, 0), tcl = -0.3,
                col.axis = PAL$ink, col.lab = PAL$ink, fg = PAL$ink,
                font.main = 1, las = 1, xaxs = "r", yaxs = "r",
                cex.axis = 0.85, cex.lab = 0.95, lend = 1, ljoin = 1, ...)
}

# ---- axis + frame idiom ----------------------------------------------------
# Clean L-frame (bottom + left spine only), thin, in ink. Optional light grid.
fig_frame <- function(hgrid = NULL, vgrid = NULL) {
  if (!is.null(hgrid)) graphics::abline(h = hgrid, col = PAL$grid, lwd = 0.7)
  if (!is.null(vgrid)) graphics::abline(v = vgrid, col = PAL$grid, lwd = 0.7)
  graphics::box(bty = "l", col = PAL$ink, lwd = 0.9)
}

# ---- titles ----------------------------------------------------------------
# Uniform title block: short left-aligned main + optional grey subtitle line.
fig_title <- function(main, sub = NULL) {
  graphics::title(main = main, adj = 0, cex.main = 1.0, font.main = 1,
                  line = if (is.null(sub)) 1.0 else 1.5, col.main = PAL$ink)
  if (!is.null(sub))
    graphics::mtext(sub, side = 3, adj = 0, line = 0.35,
                    cex = 0.78, col = PAL$sub)
}

# ---- legend ----------------------------------------------------------------
# Consistent legend: no box, tight, ink text. Default sits INSIDE at `pos`;
# set outer = TRUE to float it in the top margin (horizontal) so it never
# collides with data.
fig_legend <- function(pos = "topright", legend, col, lty = 1, lwd = 2.0,
                       pch = NA, cex = 0.82, outer = FALSE, horiz = FALSE,
                       ncol = 1, seg.len = 1.6, ...) {
  if (outer) {
    op <- graphics::par(xpd = NA)
    on.exit(graphics::par(op))
    graphics::legend(x = graphics::grconvertX(0.5, "npc", "user"),
                     y = graphics::grconvertY(1.14, "npc", "user"),
                     xjust = 0.5, yjust = 0,
                     legend = legend, col = col, lty = lty, lwd = lwd,
                     pch = pch, cex = cex, horiz = TRUE, bty = "n",
                     seg.len = seg.len, text.col = PAL$ink, ...)
  } else {
    graphics::legend(pos, legend = legend, col = col, lty = lty, lwd = lwd,
                     pch = pch, cex = cex, bty = "n", ncol = ncol,
                     seg.len = seg.len, text.col = PAL$ink,
                     inset = 0.01, ...)
  }
}

# reference-line helper (floors, zero, medians): grey, dashed, thin, uniform
fig_ref <- function(h = NULL, v = NULL, lty = 2, lwd = 0.8, col = PAL$ref) {
  if (!is.null(h)) graphics::abline(h = h, col = col, lty = lty, lwd = lwd)
  if (!is.null(v)) graphics::abline(v = v, col = col, lty = lty, lwd = lwd)
}
