# A Structural Model of WCI Allowance Pricing: replication package

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21608615.svg)](https://doi.org/10.5281/zenodo.21608615)

Code and data to reproduce the figures, tables and reported numbers of the paper.

The model prices California and Québec (WCI) carbon allowances as a bankable input. It starts
from the banking model of Rubin (1996) and adds, in turn, a risk that the program is curtailed,
the price ceiling with its two reserve tiers, and the auction reserve. The model has one free
parameter, the return required on an allowance, which is read off the price on each date.

**Versions.** Tag `v1.0` (Zenodo version 1) is the earlier paper, which solved a PDE in the
bank and time. From tag `v2.0` the package reproduces the layered banking model. The two
share the ingest stage and nothing else.

---

## Quickstart

```bash
git clone <repo> && cd wci-structural-pricing
R -e 'renv::restore()'
Rscript run_all.R               # full pipeline; figures land in paper/figures/
Rscript run_all.R --quick       # every 25th date, gates reported but not enforced
Rscript run_all.R --stage=4     # model, figures and forecast, reusing data-derived/
```

Open the project through `wci-structural-pricing.Rproj`, or start R in the repository root,
so that `.Rprofile` activates renv. The scripts use paths relative to the root.

---

## Layout

```
R/00-ingest/       raw sources -> data-derived/*.rds
  01-extract_auctions.py   auction PDFs -> auctions.csv        (Python, run offline)
  01-build_bank.R          CARB CIR -> bank, used for the 379.1 Mt check
  02-fetch_icap.R          ICAP -> `clean`, daily secondary price 2021-2026
  04-read_mrr.R            MRR workbooks -> California covered emissions
  06-build_cleancap.R      CARB caps, allocations, emissions, bank -> `cleanCap`
R/01-calibrate/    01-calibrate_gamma.R   abatement slope gamma
R/02-model/        solve_reserve.R        the solver, solve_path()
R/03-analysis/     pipeline.R             every number and table, three main figures
                   forecast_nov2026.R     forecast file for the 18 November 2026 auction
R/04-figures/      figstyle.R  figures_extra.R
data-raw/          committed public source files
data-derived/      generated, except the committed .rds named below
paper/             writeup.tex, references.bib, figures/
run_all.R          orchestrator with gates
DATA.md            sources, terms, and the choices a reader has to know about
```

`pipeline.R` prints a block headed `PAPER NUMBERS` and keeps every result in the list `out`,
saved as `data-derived/results.rds`. Each number in the paper is in that block.

---

## Gates

`run_all.R` checks the following and stops on a mismatch. Values are from the last full run.

| Gate | Value | Stage |
|---|---|---|
| `B(Q4-2024)`, vintage-restricted | 379.10 Mt | 1 |
| `clean`, daily observations | 1,275 | 1 |
| auctions in sample, and settled at the reserve | 44, 9 | 1 |
| California budget 2026 | 254.0 Mt | 2 |
| solver against the closed form, `S0` and `tau` | 82.2226, 22.222 | 3 |
| wedge, median, gamma 6.1 and 2.8 | 0.1057, 0.0847 | 4 |
| wedge, last date, gamma 6.1 | 0.1400 | 4 |
| exhaustion on the last date, gamma 6.1 and 2.8 | 2038, 2039 | 4 |
| forecast for 18 November 2026, gamma 6.1 | $33.96 | 4 |

---

## Data

Everything in `data-raw/` is public. See `DATA.md` for each file.

**Not redistributed.** The abatement-cost calibration reads the replication package of
Borenstein, Bushnell, Wolak and Zaragoza-Watkins (2019) from openICPSR, whose terms prohibit
redistribution. Without it the pipeline runs on the pinned values `gamma = 6.1` and `2.8`.

---

## Not yet automated

1. **`A`, `floor_schedule`, `ceiling_schedule`.** Small hand-entered tables (California budgets
   2021-2045, the published reserve and ceiling by year). They are committed as
   `data-derived/*.rds`, like `cleanCap.rds`. A script `07-build_schedules.R` with the figures
   and their sources should replace them.
2. **Québec totals.** The covered emissions of Québec, 2015-2025, are entered in `pipeline.R`
   from the list of covered establishments committed in `data-raw/quebec/`. The script that
   sums the list is not in the package yet.
3. **The band `[4.6, 7.5]` on gamma and the low value 2.8.** Built interactively from the
   Borenstein draws and the no-complementary-policies workbook. The point estimate 6.09 is
   reproduced by `01-calibrate_gamma.R`.
4. **The HAC standard error of the vintage curve.** `figures_extra.R` prints a plain standard
   error. The selection of auctions and the lag rule need pinning.

---

## Environment

R 4.5, packages as pinned in `renv.lock`. The model, the tables and the figures use base R only.
Python 3.8+ with `pdfplumber` for the auction parser only, whose CSV output is committed.

---

## Citation

Please cite the paper rather than this repository.
