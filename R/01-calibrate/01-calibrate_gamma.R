# 01-calibrate_gamma.R
# Abatement-cost slope gamma, calibrated to the price-responsive abatement supply
# of Borenstein, Bushnell, Wolak & Zaragoza-Watkins (2019), openICPSR replication
# package (NOT redistributed here -- see DATA.md for the DOI and file list).
#
# Method (paper, Section 4.2): under Xi(S) = S/gamma, cumulative abatement along a
# price path equals gamma^{-1} * sum_t S_t. Each sector is therefore fit by
# through-origin least squares of cumulative abatement on integrated price across
# the four allowance-price trajectories (floor and three reserve tiers), at the
# scenario elasticities; sectors aggregate harmonically because the aggregate
# abatement flow is the sum of the sector flows (Appendix C.3).
#
# Workbook geometry:
#   sheet "LinearPricePathWithSteps"; each path block is a one-row label followed
#   by 8 data rows (2013-2020); column 1 is the allowance price S, columns 13-17
#   are the five demand-elasticity variants of the abatement quantity A.
#
# Output: gam (per-draw sector gammas), gammas (point estimates), gamma_hat.
#
# THREE THINGS TO SETTLE BEFORE THIS IS CITABLE -- see the flagged blocks below:
#   [F1] dsl/S1 block anchor at row 74 was inferred, not matched. check_anchors()
#        now tests it directly; run it.
#   [F2] elasticity midpoint weights (elec/tran 0.5*(e01+e02); ng e02 alone) appear
#        in neither the paper nor any recovered function.
#   [F3] the gas+dsl -> "tran" pooling double-counts the price path. As published
#        this returns gamma_hat = 6.09; undoubled it returns 4.93. Both are inside
#        the paper's own [4.6, 7.5] sensitivity band, so no conclusion turns on it,
#        but the baseline number does. Default is "as-published" so nothing changes
#        silently; calibrate_gamma() prints both.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tibble); library(purrr); library(stringr)
})

S_COL  <- 1L
A_COLS <- 13:17

# --- block map -----------------------------------------------------------------
BLOCKS <- tribble(
  ~row, ~sector, ~path,
   12L, "gas",  "ARP",    22L, "gas",  "S1",    32L, "gas",  "S2",    42L, "gas",  "APCRT",
   64L, "dsl",  "ARP",    74L, "dsl",  "S1",    84L, "dsl",  "S2",    94L, "dsl",  "APCRT",
  139L, "elec", "ARP",   149L, "elec", "S1",   159L, "elec", "S2",   169L, "elec", "APCRT",
  215L, "ng",   "ARP",   225L, "ng",   "S1",   235L, "ng",   "S2",   245L, "ng",   "APCRT"
)

read_raw <- function(path, sheet = "LinearPricePathWithSteps")
  suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE,
                              col_types = "text", .name_repair = "minimal"))

grab_block <- function(raw, label_row, sector, pathname) {
  block <- raw[(label_row + 1L):(label_row + 8L), ]
  tibble(sector = sector, path = pathname, year = 2013:2020,
         S = as.numeric(block[[S_COL]])) %>%
    bind_cols(set_names(map_dfc(A_COLS, ~ as.numeric(block[[.x]])),
                        paste0("A_e0", 1:5)))
}

build_grid <- function(path_xlsx) {
  raw <- read_raw(path_xlsx)
  check_anchors(raw)                       # [F1] fail loudly before parsing
  pmap_dfr(BLOCKS, function(row, sector, path)
    grab_block(raw, row, sector, path))
}

# --- [F1] anchor validator -----------------------------------------------------
# Every entry in BLOCKS claims: row R is a LABEL (non-numeric in column S_COL) and
# rows R+1..R+8 are numeric and strictly increasing in S (the workbook's paths are
# monotone by construction). If row 74 were a data row rather than a label, the
# first test fails there and only there. This is the direct test the old anchor
# scan could not do.
check_anchors <- function(raw) {
  bad <- character(0)
  for (i in seq_len(nrow(BLOCKS))) {
    r <- BLOCKS$row[i]; tag <- paste0(BLOCKS$sector[i], "/", BLOCKS$path[i], " @", r)
    lab <- suppressWarnings(as.numeric(raw[[S_COL]][r]))
    if (!is.na(lab))
      bad <- c(bad, sprintf("%s: row is NUMERIC (%.4g) -- not a label row", tag, lab))
    s <- suppressWarnings(as.numeric(raw[[S_COL]][(r + 1L):(r + 8L)]))
    if (any(!is.finite(s)))
      bad <- c(bad, sprintf("%s: %d/8 data rows non-numeric", tag, sum(!is.finite(s))))
    else if (any(diff(s) <= 0))
      bad <- c(bad, sprintf("%s: S not strictly increasing", tag))
  }
  if (length(bad)) stop("block anchors failed:\n  ", paste(bad, collapse = "\n  "))
  cat("anchors: all 16 blocks are label + 8 monotone data rows\n")
  invisible(TRUE)
}

# --- geometry checks -----------------------------------------------------------
# (a) gasoline ARP elasticity sums.
# (b) cross-sector price-path identity: the SAME allowance-price trajectory is
#     applied to every sector, so gas/ARP, dsl/ARP, elec/ARP and ng/ARP must carry
#     identical S vectors. This catches a misplaced row in any block, which (a)
#     cannot -- (a) only looks at gasoline.
check_grid <- function(grid, tol = 0.15) {
  got  <- grid %>% filter(sector == "gas", path == "ARP") %>%
    summarise(across(starts_with("A_"), sum)) %>% unlist()
  want <- c(1.9, 3.8, 5.7, 7.6, 9.4)
  cat("gasoline ARP sums:", paste(sprintf("%.1f", got), collapse = " / "),
      " expected:", paste(want, collapse = " / "), "\n")
  if (max(abs(got - want)) > tol)
    stop("block geometry wrong: check BLOCKS row numbers and A_COLS")

  for (p in unique(grid$path)) {
    m <- grid %>% filter(path == p) %>%
      select(sector, year, S) %>% tidyr::pivot_wider(names_from = sector, values_from = S)
    sm <- as.matrix(m[, -1])
    if (max(abs(sweep(sm, 1, sm[, 1]))) > 1e-6)
      stop(sprintf("path %s: S differs across sectors -- a block row is misaligned", p))
  }
  cat("price-path identity: S identical across sectors within each path\n")
  invisible(TRUE)
}

# --- sector gammas -------------------------------------------------------------
# [F2] Elasticity selection (paper: "at their scenario elasticities"):
#   elec, tran -> midpoint of the -0.1/-0.2 pair, i.e. 0.5*(A_e01 + A_e02)
#   ng         -> A_e02 alone (-0.2 is the midpoint of its -0.1/-0.3 band)
pick_A <- function(sector, d)
  if (sector == "ng") d$A_e02 else 0.5 * (d$A_e01 + d$A_e02)

# NOTE: cur_data_all() was deprecated in dplyr 1.1.0 and is removed in recent
# versions; the A column is now materialised before the group_by, which is a pure
# drop-in and produces identical numbers.
sector_gamma <- function(grid, sec) {
  d <- grid %>% filter(sector == sec)
  d$A <- pick_A(sec, d)
  pts <- d %>% group_by(path) %>%
    summarise(S_int = sum(S), A_cum = sum(A), .groups = "drop")
  b <- coef(lm(A_cum ~ S_int + 0, data = pts))[[1]]   # A_cum = (1/gamma) * S_int
  1 / b
}

# [F3] Same regression, but S_int taken over the DISTINCT (year, path) price path
# rather than summed once per constituent sub-sector. For elec and ng the two are
# identical; for the pooled "tran" they differ by exactly the factor 2, because
# gas and dsl share one price path and summing over both rows counts it twice.
sector_gamma_undoubled <- function(grid, sec) {
  d <- grid %>% filter(sector == sec)
  d$A <- pick_A(sec, d)
  pts <- d %>% group_by(path) %>%
    summarise(S_int = sum(S[!duplicated(year)]), A_cum = sum(A), .groups = "drop")
  b <- coef(lm(A_cum ~ S_int + 0, data = pts))[[1]]
  1 / b
}

calibrate_gamma <- function(path_xlsx,
                            tran_pooling = c("as-published", "undoubled"),
                            verbose = TRUE) {
  tran_pooling <- match.arg(tran_pooling)
  grid <- build_grid(path_xlsx)
  check_grid(grid)
  grid <- grid %>% mutate(sector = if_else(sector %in% c("gas", "dsl"), "tran", sector))
  secs <- c("elec", "tran", "ng")

  gj_pub <- vapply(secs, function(s) sector_gamma(grid, s), 0.0)
  gj_und <- vapply(secs, function(s) sector_gamma_undoubled(grid, s), 0.0)
  h <- function(g) 1 / sum(1 / g)
  gj <- if (tran_pooling == "as-published") gj_pub else gj_und

  if (verbose) {
    cat(sprintf("\n[as-published] elec %.1f  tran %.1f  ng %.1f  -> gamma_hat %.2f",
                gj_pub["elec"], gj_pub["tran"], gj_pub["ng"], h(gj_pub)))
    cat("   [paper: 34.9 / 26.0 / 10.3 -> 6.1]\n")
    cat(sprintf("[undoubled  ] elec %.1f  tran %.1f  ng %.1f  -> gamma_hat %.2f\n",
                gj_und["elec"], gj_und["tran"], gj_und["ng"], h(gj_und)))
    cat(sprintf("  -> using '%s': gamma_hat = %.2f\n", tran_pooling, h(gj)))
    if (abs(gj_pub["tran"] / gj_und["tran"] - 2) < 0.05)
      cat("  [F3] tran ratio is exactly 2: the pooled S_int double-counts the shared\n",
          "       price path. Decide which is intended and record it in DATA.md.\n")
  }
  list(grid = grid, sector_gamma = gj, gamma_hat = h(gj),
       gamma_hat_published = h(gj_pub), gamma_hat_undoubled = h(gj_und),
       pooling = tran_pooling)
}

# --- usage ---------------------------------------------------------------------
# base    <- calibrate_gamma("data-raw/borenstein/RetailElasticities2020LinearPathV6.xlsx")
# nocomp  <- calibrate_gamma("data-raw/borenstein/RetailElasticities2020LinearPathV6_nocomp.xlsx")
#   base$gamma_hat_published -> 6.09  (paper baseline)
#   nocomp$gamma_hat         -> 2.8   (no-complementary-policies scenario)
# The draw-based band [4.6, 7.5] comes from repeating this over the simulation
# draws in `abat`; see 01-build_bau.R for the draw loop (STILL MISSING).
