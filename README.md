# A Structural Model of WCI Allowance Pricing: replication package



\[!\[DOI](https://zenodo.org/badge/1312967971.svg)](https://doi.org/10.5281/zenodo.21608615)



Code and data to reproduce every figure, table and reported number in the paper.

The model prices California–Québec (WCI) carbon allowances as a degenerate-parabolic
PDE in the private bank `B` and time `t`, with a Duffie–Singleton program-risk
intensity `λℓ` entering the surviving-market coefficient as `(r + λℓ)`. Calibration
closes a fixed point between the price path and the abatement it induces.

`Rscript run\\\_all.R` runs the full pipeline end to end and every correctness gate
passes; the last recorded full run reproduced all twelve headline quantities.

\---

## Quickstart

```bash
git clone <repo> \\\&\\\& cd wci-structural-pricing
R -e 'renv::restore()'          # restores the exact package versions in renv.lock
Rscript run\\\_all.R               # full pipeline, \\\~30 min; figures land in paper/figures/
Rscript run\\\_all.R --quick       # \\\~3 min, coarse grids; gates relaxed, figures rough
```

Open the project through `wci-structural-pricing.Rproj` (or start R in the repo
root) so `.Rprofile` activates renv. Stages checkpoint to `data-derived/`;
`Rscript run\\\_all.R --stage=3` resumes from the model stage without re-running
ingest.

\---

## Layout

```
R/00-ingest/       raw sources -> data-derived/\\\*.rds
  00-download\\\_cir.R        (optional) refresh CARB CIR files from source
  01-extract\\\_auctions.py   auction PDFs -> auctions.csv          (Python, run offline)
  01-build\\\_bank.R          CARB CIR -> quarterly private bank B\\\_t
  02-fetch\\\_icap.R          ICAP -> `clean`, daily secondary 2021-2026
  03-splice\\\_prices.R       -> `prices\\\_full`, the 3-segment spliced series
  04-read\\\_mrr.R            MRR workbooks -> covered emissions e\\\_t
  05-read\\\_cftc.R           CFTC COT -> `cca\\\_wide` positioning panel
  06-build\\\_cleancap.R      CARB caps/allocations/emissions -> `cleanCap`
R/01-calibrate/    01-calibrate\\\_gamma.R    abatement slope gamma
R/02-model/        build\\\_forcing.R  solver.R  fixed\\\_point.R
R/03-tests/        lambda\\\_ell.R  convergence.R  sensitivity.R  positioning\\\_test.R
R/04-figures/      figstyle.R  figure\\\_surface.R  figure\\\_projection.R  figure\\\_premium.R
data-raw/          committed public source files
data-derived/      generated; safe to delete and rebuild
paper/             writeup.tex, references.bib, figures/
run\\\_all.R          orchestrator with correctness gates
DATA.md            provenance, licensing, and the open conventions
```

`figstyle.R` is **not** optional. The figure scripts reference `PAL`, `SCEN\\\_COL`
and `fig\\\_open()` unqualified and fail without it; `run\\\_all.R` sources it once at
stage 5.

\---

## Correctness gates

`run\\\_all.R` asserts the paper's headline numbers as it goes and aborts on
mismatch, so a broken clone fails loudly rather than silently producing different
figures. All values below are from the last full run.

|Gate|Value|Stage|
|-|-|-|
|`B(Q4-2024)` vintage-restricted|379.10 Mt|1|
|`clean` daily observations|1,275|1|
|`cca\\\_wide` weekly observations|676|1|
|`lambda\\\_ell\\\_hat` (median, interior)|0.102|2|
|`S^int(400,t0)` at NB=400 (Table 9)|21.60 USD/t|4|
|median `S\\\*` over 2021-2026|31.9 USD/t|4|
|spatial convergence order|1.02|4|
|`tau\\\*` onset, fitted λℓ|2036|3|
|Spearman ρ, premium vs net length|0.525|4|
|Spearman ρ, ex-touches|0.697|4|

`gamma` is gated only when the Borenstein package is present (see Data); the
pipeline otherwise runs on the pinned `gamma = 6.1`. Tolerances are loose relative
to reported precision — the solver is a finite-difference scheme and small
floating-point differences across platforms are expected. A gate that trips means
something structural changed, not rounding.

**λℓ is pinned at 0.10 for the model stages.** The fit returns 0.102 (IQR
\[0.091, 0.116]); the paper reports 0.10 throughout, and every model quantity is
computed at the rounded value. λℓ is a bounded Q-measure object, not a precisely
measured one, so three significant figures would overstate the fit. Stage 2 gates
that the fit rounds to 0.10; stages 3+ use 0.10.

\---

## Data

Everything in `data-raw/` is public and redistributable: CARB/WCI auction reports
and Compliance Instrument Reports, ICAP Allowance Price Explorer, California
Carbon Dashboard futures, CFTC Commitments of Traders, CARB MRR. See `DATA.md`
for per-file provenance, the CFTC filtered-extract policy, and the open
conventions (γ pooling, futures de-duplication).

**Not redistributed.** The abatement-cost calibration reads the Borenstein,
Bushnell, Wolak \& Zaragoza-Watkins (2019) replication package from openICPSR;
its terms prohibit redistribution. `DATA.md` gives the DOI and names the files
needed. Without it the pipeline runs on the pinned `gamma = 6.1` and prints a
notice; only the gamma calibration itself is unreproducible.

\---

## Not yet automated

Two paper outputs are not reproduced by this pipeline; both are documented rather
than silently missing.

1. **Figure 1 and the `\\\[4.6, 7.5]` draw band on gamma** (`01-build\\\_bau.R`). The
BAU emissions fan and the per-draw gamma band were built interactively from
the Borenstein simulation draws and have not been scripted. The point estimate
`gamma = 6.09` is reproduced by `01-calibrate\\\_gamma.R`; the band is not.
2. **Prediction 4.3's vintage-slope regression** (`test\\\_vintage.R`). Note the
printed HAC standard error (0.28) and an earlier reconstruction (0.27) differ
in the third decimal; the interior-subsample definition or the HAC lag rule
needs pinning before this is frozen.

Neither blocks `run\\\_all.R`.

\---

## Environment

```r
install.packages("renv")
renv::restore()
```

R 4.5 (the lockfile records the exact version; earlier releases are untested).
Package dependencies: `dplyr`, `tidyr`, `tibble`, `purrr`, `readr`, `readxl`,
`stringr`, `curl`, plus their recursive dependencies as pinned in `renv.lock`.
Figures are base graphics — no ggplot2.

Python 3.8+ with `pdfplumber` for the auction parser only (run offline; its CSV
output is committed).

\---

## Citation

Preprint, arXiv (forthcoming). Please cite the paper rather than this repository.

