# download_cir.R
# Pulls the complete CARB Compliance Instrument Report archive into ./cir_raw/.
# Run this on your machine (the sandbox can't reach arb.ca.gov).
# URLs are irregular (host, path, nc- prefix, _1 suffix all vary), so they are hardcoded.

dir.create("cir_raw", showWarnings = FALSE)

man <- tibble::tribble(
  ~report,   ~snapshot,     ~fmt,   ~url,
  # ---- individual XLSX (Q1-2021 .. Q4-2025) ----
  "2025Q4","2026-01-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2026-01/nc-2025_q4_complianceinstrumentreport.xlsx",
  "2025Q3","2025-10-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2025-10/nc-2025_q3_complianceinstrumentreport.xlsx",
  "2025Q2","2025-07-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2025-07/nc-2025_q2_complianceinstrumentreport.xlsx",
  "2025Q1","2025-04-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2025-04/nc-2025_q1_complianceinstrumentreport.xlsx",
  "2024Q4","2025-01-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2025-01/nc-2024_q4_complianceinstrumentreport.xlsx",
  "2024Q3","2024-10-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2024-10/nc-2024_q3_complianceinstrumentreport_1.xlsx",
  "2024Q2","2024-07-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2024-07/nc-2024_q2_complianceinstrumentreport.xlsx",
  "2024Q1","2024-04-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2024-04/nc-2024_q1_complianceinstrumentreport.xlsx",
  "2023Q4","2024-01-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2024-01/nc-2023_q4_complianceinstrumentreport.xlsx",
  "2023Q3","2023-10-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2023-11/nc-2023_q3_complianceinstrumentreport.xlsx",
  "2023Q2","2023-07-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2023-11/nc-2023_q2_complianceinstrumentreport.xlsx",
  "2023Q1","2023-04-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2023-04/nc-2023_q1_complianceinstrumentreport.xlsx",
  "2022Q4","2023-01-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2023-01/nc-2022_q4_complianceinstrumentreport.xlsx",
  "2022Q3","2022-10-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2022-10/nc-2022_q3_complianceinstrumentreport.xlsx",
  "2022Q2","2022-07-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2022-07/nc-2022_q2_complianceinstrumentreport.xlsx",
  "2022Q1","2022-04-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2022-04/2022_q1_complianceinstrumentreport.xlsx",
  "2021Q4","2022-01-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2022-01/nc-2021_q4_complianceinstrumentreport.xlsx",
  "2021Q3","2021-10-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2021-10/nc-2021_q3_complianceinstrumentreport.xlsx",
  "2021Q2","2021-07-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/2021-07/nc-2021_q2_complianceinstrumentreport.xlsx",
  "2021Q1","2021-04-01","xlsx","https://ww2.arb.ca.gov/sites/default/files/cap-and-trade/2021_q1_complianceinstrumentreport.xlsx",
  # ---- individual CSV (Q3-2019 .. Q4-2020) ----
  "2020Q4","2021-01-01","csv","https://www.arb.ca.gov/cc/capandtrade/2020_q4_complianceinstrumentreport.csv",
  "2020Q3","2020-10-01","csv","https://ww2.arb.ca.gov/sites/default/files/2020-10/2020_q3_complianceinstrumentreport.csv",
  "2020Q2","2020-07-01","csv","https://ww2.arb.ca.gov/sites/default/files/2020-07/2020_q2_complianceinstrumentreport.csv",
  "2020Q1","2020-04-01","csv","https://ww3.arb.ca.gov/cc/capandtrade/2020_q1_complianceinstrumentreport.csv",
  "2019Q4","2020-01-01","csv","https://ww3.arb.ca.gov/cc/capandtrade/2019_q4_complianceinstrumentreport.csv",
  "2019Q3","2019-10-01","csv","https://ww3.arb.ca.gov/cc/capandtrade/2019_q3_complianceinstrumentreport.csv",
  # ---- combined multi-sheet workbook (everything before Q3-2019) ----
  "PRE2019",NA,        "xlsx","https://ww3.arb.ca.gov/cc/capandtrade/complianceinstrumentreport.xlsx"
)

# Robust fetch: the default libcurl method fails on some Windows / proxied
# networks at the connection level. Try the browser's stack (wininet) first,
# then libcurl, then the curl package. Reject tiny files (saved error pages).
# The CARB host returns 403 to non-browser clients: the fix is browser headers,
# not the transport method. Send a real UA + Accept via base libcurl; fall back
# to the curl package if present. (libcurl is standard in Windows R; check with
# capabilities("libcurl").)
options(timeout = max(600, getOption("timeout")))
BUA <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
             "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
HDRS <- c("User-Agent" = BUA,
          "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Language" = "en-US,en;q=0.9")

get_one <- function(url, dest) {
  ok <- tryCatch({                                  # base R, libcurl + headers
    suppressWarnings(download.file(url, dest, mode = "wb", quiet = TRUE,
                                   method = "libcurl", headers = HDRS))
    file.exists(dest) && file.info(dest)$size > 1000
  }, error = function(e) FALSE)
  if (isTRUE(ok)) return("libcurl")
  if (file.exists(dest)) file.remove(dest)
  if (requireNamespace("curl", quietly = TRUE)) {   # backup: curl package
    ok <- tryCatch({
      h <- curl::new_handle(); do.call(curl::handle_setheaders, c(list(h), as.list(HDRS)))
      curl::curl_download(url, dest, handle = h, quiet = TRUE)
      file.exists(dest) && file.info(dest)$size > 1000
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return("curl")
    if (file.exists(dest)) file.remove(dest)
  }
  NA_character_
}

for (i in seq_len(nrow(man))) {
  dest <- file.path("cir_raw", sprintf("%s.%s", man$report[i], man$fmt[i]))
  if (file.exists(dest) && file.info(dest)$size > 1000) next
  m <- get_one(man$url[i], dest)
  if (is.na(m)) message("FAILED  ", man$report[i])
  else          message("ok [", m, "]  ", man$report[i])
  Sys.sleep(0.4)  # be polite
}
saveRDS(man, "cir_raw/_manifest.rds")
message("done. files in cir_raw/")
