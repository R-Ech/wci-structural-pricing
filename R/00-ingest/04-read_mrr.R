# 04-read_mrr.R
# Covered emissions e_t from the CARB Mandatory Reporting Regulation workbooks.
#
# read_mrr() is the per-file reader: one MRR workbook -> one facility-level tibble.
# build_emissions() is the driver: all workbooks -> the annual covered series.
#
# SCOPE. This produces the BROAD-scope covered series used to calibrate the
# emissions process (349 Mt in 2013 declining to 257 Mt in 2024), holding coverage
# fixed at the post-2015 definition. It is NOT cleanCap$Emissions, which tracks
# coverage as it actually stood each year and therefore steps at 2015 when
# transport fuels entered. Two different objects; see DATA.md.
#
# The workbooks are not schema-stable across vintages: the header row moves, the
# ID column is variously "ARB ID" / "CARB ID" / "Entity ID", and covered emissions
# appear either as a single total column or as several columns to be summed.
# read_mrr() locates all three by pattern and reports which columns it used --
# read those messages rather than trusting the totals blind.
#
#   source("R/00-ingest/04-read_mrr.R")
#   mrr <- build_mrr("data-raw/mrr")
#   e_t <- build_emissions(mrr)
#   saveRDS(e_t, "data-derived/e_t.rds")

suppressPackageStartupMessages({
  library(readxl); library(readr); library(stringr)
  library(dplyr); library(purrr); library(tibble)
})

read_mrr <- function(path, year) {
  sheets <- excel_sheets(path)
  data_sheet <- sheets[str_detect(sheets, regex("ghg data", ignore_case = TRUE))][1]
  raw <- read_excel(path, sheet = data_sheet, col_names = FALSE, col_types = "text")

  hdr <- which(apply(raw[1:15, ], 1, \(r)
                     any(str_detect(tolower(coalesce(as.character(r), "")),
                                    "^(c?arb|entity) id"), na.rm = TRUE)))[1]
  if (is.na(hdr)) stop(sprintf("year %d: no 'ARB ID' header row found", year))

  nm   <- tolower(str_squish(as.character(unlist(raw[hdr, ]))))
  nm[nm == "" | is.na(nm)] <- paste0("x", which(nm == "" | is.na(nm)))
  df   <- set_names(raw[(hdr + 1):nrow(raw), ], make.unique(nm))

  num   <- function(col) parse_number(as.character(df[[col]]))
  pick1 <- function(p) { x <- names(df)[str_detect(names(df), p)]; if (length(x)) x[1] else NA_character_ }

  cov_cols <- names(df)[str_detect(names(df), "covered")]
  tot_col  <- cov_cols[str_detect(cov_cols, "total")]
  covered  <- if (length(tot_col)) num(tot_col[1])
  else if (length(cov_cols)) reduce(map(cov_cols, ~ coalesce(num(.x), 0)), `+`)
  else NA_real_

  message(sprintf("%d: covered from [%s]", year,
                  paste(if (length(tot_col)) tot_col[1] else cov_cols, collapse = " + ")))

  rep_col <- pick1("total reported|reported emissions|total emissions")
  tibble(
    year     = year,
    entity   = as.character(df[[pick1("name")]]),
    sector   = if (!is.na(pick1("sector|industry|naics"))) as.character(df[[pick1("sector|industry|naics")]]) else NA_character_,
    covered  = covered,
    reported = if (!is.na(rep_col)) num(rep_col) else NA_real_
  ) %>%
    filter(!is.na(covered) | !is.na(reported),
           !str_detect(tolower(coalesce(entity, "")), "^total"))
}

# ---- driver -------------------------------------------------------------------
build_mrr <- function(dir = "data-raw/mrr") {
  files <- list.files(dir, pattern = "^mrr-\\d{4}\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("no mrr-YYYY.xlsx in ", dir)
  yrs <- as.integer(str_extract(basename(files), "\\d{4}"))
  out <- map2_dfr(files, yrs, function(f, y)
    tryCatch(read_mrr(f, y),
             error = function(e) { message("FAILED ", basename(f), ": ", conditionMessage(e)); NULL }))
  got <- sort(unique(out$year))
  miss <- setdiff(yrs, got)
  if (length(miss)) warning("no rows parsed for: ", paste(miss, collapse = ", "))
  cat(sprintf("mrr: %d facility-years, %d to %d\n", nrow(out), min(got), max(got)))
  out
}

# Annual totals. `covered` is the compliance-relevant figure; `reported` is the
# wider MRR universe and is carried only as a sanity check (reported >= covered
# in every year, since not everything reported is covered).
build_emissions <- function(mrr) {
  e <- mrr %>%
    group_by(year) %>%
    summarise(covered_Mt  = sum(covered,  na.rm = TRUE) / 1e6,
              reported_Mt = sum(reported, na.rm = TRUE) / 1e6,
              n_entities  = n(), .groups = "drop") %>%
    arrange(year)

  bad <- e$reported_Mt > 0 & e$covered_Mt > e$reported_Mt
  if (any(bad)) warning("covered exceeds reported in: ", paste(e$year[bad], collapse = ", "),
                        " -- likely a column-matching error in those workbooks")

  print(as.data.frame(e), row.names = FALSE)
  if (any(e$year == 2015))
    cat(sprintf("\n2015 covered: %.1f Mt   [Borenstein et al. (2019) broad-scope: 340.3]\n",
                e$covered_Mt[e$year == 2015]))
  e
}

# Log-linear drift, the g_y in build_forcing(). Paper: -0.02869/yr.
fit_drift <- function(e, from = 2015, to = 2024) {
  d <- e %>% filter(year >= from, year <= to, covered_Mt > 0)
  fit <- lm(log(covered_Mt) ~ year, data = d)
  g <- unname(coef(fit)[2])
  cat(sprintf("g_y = %+.5f /yr over %d-%d (n=%d, R2=%.3f)   [paper -0.02869]\n",
              g, from, to, nrow(d), summary(fit)$r.squared))
  invisible(list(g_y = g, fit = fit))
}
