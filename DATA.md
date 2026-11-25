# DATA.md: sources, terms and choices

**Empirical freeze.** Daily prices (nearest-December futures settlement) from 30 March 2021 to 31 March 2026. Auctions
2015Q2 to 2026Q1 (44 observations) in sample. The May and August 2026 auctions are used only
after the sample, and the 18 November 2026 auction is forecast.

---

## 1. Committed in `data-raw/`

| Path | Source | Terms | Feeds |
|---|---|---|---|
| `Auctions/`, `auctions.csv` | CARB / WCI Joint Auction Summary Results Reports, 2015Q2 onward | Government work, public | `auc`: reserve auctions, premium, vintage curve, post-sample check |
| `cir_raw/` | CARB Compliance Instrument Reports, quarterly | Public | bank, 379.1 Mt at end-2024 |
| `mrr/`, `mrr.csv` | CARB Mandatory Reporting Regulation | Public | California covered emissions |
| `nc-OverallCaps.csv` | CARB allocation schedule | Public | `cleanCap` |
| `nc-allowance_prices.csv` | CARB reserve, tier and ceiling schedule | Public | `floor_schedule`, `ceiling_schedule`, tier prices |
| `DGS10.csv` | FRED, 10-year Treasury constant maturity | Public | risk-free rate `r_t` |
| `quebec/liste-etablissements-visesRSPEDE.xlsx` | Données Québec, covered emissions under the RSPEDE, update of 1 October 2026 | Public (check the licence on the dataset page) | Québec covered emissions, level and trend |

No longer used, kept in tag `v1.0` only: `FuturesData.csv`, `cftc/`, `DGS1.csv`, `cpu_pu.csv`,
`Calendar.csv`.

---

## 2. Not redistributed

**Daily prices.** ICE end-of-day settlement of the nearest December California Carbon Allowance
futures contract, 30 March 2021 to 31 March 2026, as displayed by the ICAP Allowance Price
Explorer (system id 6). ICE licenses the end-of-day series, and ICAP's terms make it
view-only. It is not redistributed. Place it at `data-raw/icap_secondary_prices.csv`, with
columns `date` and `price`. It feeds `clean`: the wedge, the event windows and volatility.
The series rolls to the next December contract on the first trading day of each year.

**Borenstein, Bushnell, Wolak and Zaragoza-Watkins (2019)**, "Expecting the Unexpected:
Emissions Uncertainty and Environmental Market Design," *American Economic Review* 109(11),
doi:10.1257/aer.20161218. Replication package at openICPSR. Place under `data-raw/borenstein/`:

```
RetailElasticities2020LinearPathV6.xlsx           # gamma, baseline (6.1)
RetailElasticities2020LinearPathV6_nocomp.xlsx    # gamma, no complementary policies (2.8)
```

> VERIFY: the openICPSR deposit ID is not recorded here. Take it from the article page.

---

## 3. Choices a reader has to know about

**WCI-wide inputs.** The cap and covered emissions are California plus Québec. The bank in the
Compliance Instrument Reports is already WCI-wide.

**Québec emissions: level and trend from different sets.** The set of covered establishments
changed between 2015 and 2024. The level (about 57 Mt) is the sum of all covered establishments
and fuel distributors. The trend (-0.74% per year) is taken on the 66 establishments covered in
every year, plus the distributors. For California only the published total is used.

**Québec cap after 2030.** Caps are set from 55.26 Mt (2021) to 44.14 Mt (2030). After 2030 the
same linear decline is continued. This is an assumption.

**Québec auction share.** Set to California's (0.4985, the average of the last three years).

**gamma is scaled.** 6.1 and 2.8 are calibrated on California. The solves use these values
times California's share of covered emissions (0.818), that is 4.99 and 2.29. Tables and
figures keep the labels 6.1 and 2.8.

**Sector pooling in the gamma calibration.** As in `v1.0`: `calibrate_gamma(tran_pooling =
"as-published" | "undoubled")` gives 6.09 or 4.93.

**Bank after 2024.** The end-2025 report follows a partial surrender and leaves out the open
period, so it is not comparable with end-2024. From 2025 the bank is carried forward by the
flow of the model at observed prices. `bank_vintage_restricted` is the paper's bank through
2024. Do not use `bank_continuous` or `bank_all_private`.

**Reserve and ceiling.** Published schedule through 2026. After 2026 they grow at 7.5% per
year (5% plus an assumed 2.5% inflation), continuously. In reality they step once a year.

**Reserve tiers.** 66.811 Mt at $65.31 and 89.537 Mt at $83.92 in 2026, from the 2026 Reserve
Sale notice. The ratios to the reserve (2.34 and 3.00) are kept after 2026. Each tier is bought
in full on the date the price reaches it.

**Unsold auction volume.** Treated as withheld. Part of it is in fact offered again later.

**Events.** 8 April 2025 (federal executive action) and 19 September 2025 (AB 1207).

**Auction discount.** Auctions settle 4.7% below the daily (nearest-December futures) price at the median over
2021-2026. It is used to compare the August 2026 settlement with the secondary-price path, and
not in the supply step of the model (`CFG$discount = 0`).

**Emissions trend.** Log-linear over 2015-2024 without 2020: California -2.87%, Québec -0.74%,
together -2.53% per year. The scenarios use -2.0%, -2.53% and -4.5%.

---

## 4. Regenerating the committed derivatives

```bash
python3 R/00-ingest/01-extract_auctions.py data-raw/Auctions data-raw/auctions.csv
```

The printed count of auctions settled at the reserve on the current leg must be 9 in sample,
with no `PARSE-FAIL` and no `FILENAME-MISMATCH` rows.

`cleanCap.rds`, `A.rds`, `floor_schedule.rds` and `ceiling_schedule.rds` are committed in
`data-derived/`. Everything else there is rebuilt by `run_all.R`.
