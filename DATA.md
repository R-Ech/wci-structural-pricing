# DATA.md — sources, licensing, and conventions

Provenance for every input to the replication package, the two source files that
are **not** redistributed here, and the conventions a reader has to know about to
interpret the numbers.

Guiding rule: commit what reproduces the result, at proportionate size. Public
data that is load-bearing and small is committed outright. Public data that is
load-bearing but enormous is committed as a filtered analysis sample together
with the script that produces it. Data under redistribution restrictions is
linked, never copied.

**Empirical freeze.** Auctions 2015Q2–2026Q1 (44 observations) plus May 2026 as a
designated out-of-sample check. Daily secondary prices through 2026-03-31. All
sources accessed on or before that date; later revisions by the publishers will
not be reflected.

---

## 1. Committed in `data-raw/`

| Path | Source | Terms | Feeds |
|---|---|---|---|
| `Auctions/` | CARB / WCI Joint Auction Summary Results Reports, 2015Q2 onward | US state / QC government work, public | `auctions.csv` via `01-extract_auctions.py`; settlement premium, Table 7, Figure 5, splice bridge |
| `cir_raw/` | CARB Compliance Instrument Reports, quarterly | Public | private bank `B_t` via `01-build_bank.R` |
| `icap_secondary_prices.csv` | ICAP Allowance Price Explorer, system id 6 (CCA secondary) | Public, attribution requested | `clean`; λℓ fit, Prediction 4.1 volatility panel |
| `FuturesData.csv` | California Carbon Dashboard, front-December CCA futures | Public | 2013–2017 segment of `prices_full` |
| `mrr/`, `mrr.csv` | CARB Mandatory Reporting Regulation | Public | covered emissions `e_t` |
| `nc-OverallCaps.csv` | CARB allocation schedule | Public | `cleanCap`, the regulatory cap path `a(t)` |
| `nc-allowance_prices.csv` | CARB reserve / APCR tier schedule | Public | floor and ceiling cross-checks |
| `DGS1.csv` | FRED, 1-year Treasury constant maturity | Public | discount rate `r` |
| `cpu_pu.csv`, `Calendar.csv` | CARB auction calendar and purchase-limit schedule | Public | auction timing, per-auction purchase limit |
| `e_t_covered_broadscope.csv` | Derived from MRR, broad-scope covered total | — | emissions series; `04-read_mrr.R` rebuilds it, 2015 total 340.4 Mt reconciles to Borenstein et al. (2019) 340.3 |
| `cftc/cca_cot.csv` | CFTC Disaggregated Commitments of Traders — **filtered extract**, see §3 | US government work, public domain | net positioning; Prediction 4.4 |

The pre-2015Q2 auction reports use a two-column California-only layout that the
parser does not read (four USD/CAD columns are required); this is one reason the
primary-market frame starts 2015Q2. Any `Auctions2/` folder of 2012–2017 reports
is superseded by `Auctions/` for the overlap and is not part of the package.

---

## 2. Not redistributed

**Borenstein, Bushnell, Wolak & Zaragoza-Watkins (2019)**, "Expecting the
Unexpected: Emissions Uncertainty and Environmental Market Design," *American
Economic Review* 109(11):3953–3977, doi:10.1257/aer.20161218. Replication package
deposited at openICPSR under the AEA data availability policy; its terms permit
use but not redistribution.

Download it and place the following under `data-raw/borenstein/`:

```
RetailElasticities2020LinearPathV6.xlsx           # gamma, baseline
RetailElasticities2020LinearPathV6_nocomp.xlsx    # gamma, no-complementary-policies
BAU/annual_abatement.dta                          # BAU drift and volatility
```

> **VERIFY:** the openICPSR deposit ID is not recorded here. Fill in the exact
> DOI from the AER article page before posting — do not guess it.

Without these files the pipeline still runs: `run_all.R` detects their absence,
falls back to the pinned `gamma = 6.1`, prints a notice, and skips the gamma gate.
Only the gamma calibration itself becomes unreproducible.

**Bloomberg CCA series.** A Bloomberg-sourced price file existed in the working
directory and is **not** used by any result in the paper. It is excluded, and no
number depends on it. Recorded here only so a reader who finds it referenced in
older drafts knows it was retired rather than suppressed.

---

## 3. Filtered extract: CFTC

The raw CFTC disaggregated COT archive covers every commodity the Commission
reports and runs to hundreds of megabytes across 2010–2026. The paper uses CCA
vintage futures only — 676 weekly observations, 2013-05-07 onward.

Committed: `data-raw/cftc/cca_cot.csv`, the filtered analysis sample (~50 KB).
Not committed: the raw yearly files in `data-raw/cftc_raw/` (gitignored).

`R/00-ingest/05-read_cftc.R` regenerates the extract. Scope is California carbon
**vintage futures on ICE Futures U.S.**, identified by CFTC contract-code family
`0063` (006391 for the 2013 generic contract, then 0063E5 for V2025, 0063EE for
V2026, and so on per vintage). Seven distinct market-name forms appear across the
sample and all are matched; the script header lists them.

Three exclusions, each reproducing the published series:

- `CALIF CARBON CURRENT AUCTION` — auction-linked instrument, not a vintage future.
- `... - NODAL EXCHANGE` — second venue, listings from August 2024.
- `CALIF CARBON VINTAGE SPEC 2028` (code 006WBY) — separate product outside the
  `0063` family; clears the COT reporting threshold in only 8 scattered weeks of
  2025.

Net positioning is managed money plus other reportables, excluding swap dealers
and excluding the spread legs (Section 4.4). One contract is 1,000 allowances, so
Mt = contracts / 1000. The reconstruction matches the original panel exactly:
max absolute difference 1.4e-14 Mt on `spec_net_Mt`, identical `n_contracts` in
all 676 weeks, validated against the 2013-05-07 print (`validate_cca_wide()`).

Anyone auditing the filter downloads the raw yearly files from the CFTC and runs
`build_cca_wide()` then `validate_cca_wide()`; anyone reproducing the paper's
numbers uses the committed extract. Committed rather than linked because CFTC
restates COT reports — trader reclassifications and retroactive corrections both
occur — so a link alone would leave a later reader unable to distinguish a genuine
discrepancy from a publisher revision.

The committed extract ends 2026-06-02, past the 2026-03-31 empirical freeze; the
auction join uses only observations on or before each auction date, so the extra
weeks do not enter any result.

---

## 4. Conventions requiring a decision

Two places where the code admits more than one defensible reading. Both are
exposed as arguments; both defaults reproduce the paper as written.

### 4.1 Sector pooling in the gamma calibration

After the gasoline/diesel merge into a combined transport sector,
`sector_gamma()` sums the integrated price `S_int` across both constituent fuels
even though they share one allowance-price trajectory. The abatement quantity
`A_cum` correctly sums both; the price integral is counted twice. This halves the
regression coefficient and doubles `gamma_tran`. Single-block sectors
(electricity, natural gas) are unaffected.

| Pooling | elec / tran / ng | `gamma_hat` |
|---|---|---|
| As published (default) | 34.9 / 26.0 / 10.3 | 6.09 |
| Undoubled | 34.9 / 13.0 / 10.3 | 4.93 |

Both lie inside the paper's own `[4.6, 7.5]` sensitivity band, exhaustion timing
is gamma-invariant in the exogenous variant, and the feedback deferral moves less
than the range already reported. The Table 2 baseline is keyed to the first.
`calibrate_gamma(tran_pooling = "as-published" | "undoubled")` computes both and
prints the ratio.

### 4.2 Duplicate block in `FuturesData.csv`

Seventeen dates from 2016-09-29 to 2016-10-21 appear twice with prices that
disagree in the third decimal (2016-09-29: 12.946 vs 12.940), consistent with two
overlapping vendor pulls or a revision. The block falls inside the CalChamber
litigation window — the floor-binding episode the regime classification turns on
— so leaving it duplicated double-weights exactly the dates that matter most.

`read_futures(dedup = "first" | "last" | "mean")`; default `"first"`, and the
count is printed on every run. A further duplicate at 2018-03-29 falls outside
the sample.

---

## 5. Known data characteristics

**Liquidity cut, not availability cut.** `clean` starts 2021-03-30. The ICAP
extract reaches back to 2020-11-02; the 100 earlier observations exist but predate
the secondary market's lift-off and are not informative at daily frequency.
Anything needing pre-2021 daily prices uses the spliced `prices_full`.

**`clean` and `prices_full` are different objects.** `clean` is a single
instrument. `prices_full` is a three-segment splice with two seams (futures →
auction bridge → secondary). No daily-frequency statistic crosses a seam. The
trailing twelve-month return in Prediction 4.4 does span the bridge, but it is an
annual-horizon statistic sampled at quarterly auction dates. Seam diagnostics are
in Appendix D.2 and reproduced by `splice_diagnostics()`.

**The model floor is analytic, not observed.** The solver uses
`R(t) = 17.71 · exp(0.075 (t − 2021))`. The statutory reserve escalates at 5% plus
realized CPI, so the two diverge — materially in 2022–23. `fit_lambda_ell()`
classifies a date as interior when `price > 1.03 · R(t)` against the *analytic*
floor, which matters only for near-floor dates (the 2025-05 and 2026-02 touches).
λℓ̂ is a median over interior dates and is robust to a few borderline
reclassifications; `check_floor_drift()` reports the size of the gap.

**Bank definition.** `bank_vintage_restricted` is the paper's `B_t` and the
source of the 379.1 Mt anchor. `bank_continuous` (802 Mt at Q4-2024) and
`bank_all_private` (831 Mt) are diagnostics; Section 4.1 explicitly rejects
figures of that order as double-counting instruments owed to the regulator.
Do not wire either into `build_forcing()`.

**Surrender flow.** Difference `retirement_cum_all` (fixed all-vintage basis), not
`retirement_cum` (rolling cutoff). The rolling series steps by ~748 Mt at each
triennial true-up purely because vintages enter the sum; differencing it
manufactures a phantom spike.

**Emissions drift `g_y`.** `04-read_mrr.R` fits log-linear covered emissions over
2015–2024 and returns −0.02906/yr (R² = 0.92). The pinned baseline used throughout
the model is −0.02869. Reconcile the window (2013–2024, or excluding the 2020
pandemic trough) or report both; the pinned value drives `build_forcing()`.

**`lambda_ell` is pinned, not propagated from the fit.** `fit_lambda_ell()`
returns median 0.102 (IQR [0.091, 0.116]) over 1,268 interior dates. The model
stages use 0.10, matching the paper. It is a bounded Q-measure object (Duffie–
Singleton), not a precisely measured econometric quantity, so the rounded value is
the honest one. Passing the raw 0.102 raises the effective rate by 19 bp and moves
`S^int(400,t0)` from 21.60 to ~21.00 — correct behaviour, not an error, and the
reason the model stages pin 0.10.

**MRR 2008–2010 do not parse** (`Sheet 'NA' not found`) and are skipped with a
notice. Irrelevant: the emissions fit starts 2015. Not a defect.

---

## 6. Regenerating the committed derivatives

```bash
python3 R/00-ingest/01-extract_auctions.py data-raw/Auctions data-raw/auctions.csv
```

Verify before committing: the printed current-leg exact-reserve clearing count
must be 9 in-sample, with zero `PARSE-FAIL` and zero `FILENAME-MISMATCH` rows.

```r
source("R/00-ingest/05-read_cftc.R")
raw <- build_cca_wide("data-raw/cftc_raw")   # raw yearly files (gitignored)
validate_cca_wide(raw)                        # 2013-05-07 print + 676-week count
write_cca_extract(raw)                        # -> data-raw/cftc/cca_cot.csv
```

`cleanCap` is regenerated by `06-build_cleancap.R`; the hand-entered figures live
in that script (dump them from a live object with `dump_cleancap_source()`).

Everything in `data-derived/` is rebuilt by `run_all.R` and is gitignored, except
`cleanCap.rds`, which is force-committed so a clone can run the model stages
without the CARB PDFs on hand.
