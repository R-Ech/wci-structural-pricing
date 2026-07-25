# A Structural Model of WCI Allowance Pricing — replication package

Code and data to reproduce every figure, table and reported number in the paper.

The model prices California–Québec (WCI) carbon allowances as a degenerate-parabolic
PDE in the private bank `B` and time `t`, with a Duffie–Singleton program-risk
intensity `λℓ` entering the surviving-market coefficient as `(r + λℓ)`. Calibration
closes a fixed point between the price path and the abatement it induces.

`Rscript run_all.R` runs the full pipeline end to end and every correctness gate
passes; the last recorded full run reproduced all twelve headline quantities.

---

## Quickstart

```bash
git clone <repo> && cd wci-structural-pricing
R -e 'renv::restore()'          # restores the exact package versions in renv.lock
Rscript run_all.R               # full pipeline, ~30 min; figures land in paper/figures/
Rscript run_all.R --quick       # ~3 min, coarse grids; gates relaxed, figures rough
```

Open the project through `wci-structural-pricing.Rproj` (or start R in the repo
root) so `.Rprofile` activates renv. Stages checkpoint to `data-derived/`;
`Rscript run_all.R --stage=3` resumes from the model stage without re-running
ingest.

---

## Layout

```
R/00-ingest/       raw sources -> data-derived/*.rds
  00-download_cir.R        (optional) refresh CARB CIR files from source
  01-extract_auctions.py   auction PDFs -> auctions.csv          (Python, run offline)
  01-build_bank.R          CARB CIR -> quarterly private bank B_t
  02-fetch_icap.R          ICAP -> `clean`, daily secondary 2021-2026
  03-splice_prices.R       -> `prices_full`, the 3-segment spliced series
  04-read_mrr.R            MRR workbooks -> covered emissions e_t
  05-read_cftc.R           CFTC COT -> `cca_wide` positioning panel
  06-build_cleancap.R      CARB caps/allocations/emissions -> `cleanCap`
R/01-calibrate/    01-calibrate_gamma.R    abatement slope gamma
R/02-model/        build_forcing.R  solver.R  fixed_point.R
R/03-tests/        lambda_ell.R  convergence.R  sensitivity.R  positioning_test.R
R/04-figures/      figstyle.R  figure_surface.R  figure_projection.R  figure_premium.R
data-raw/          committed public source files
data-derived/      generated; safe to delete and rebuild
paper/             writeup.tex, references.bib, figures/
run_all.R          orchestrator with correctness gates
DATA.md            provenance, licensing, and the open conventions
```

`figstyle.R` is **not** optional. The figure scripts reference `PAL`, `SCEN_COL`
and `fig_open()` unqualified and fail without it; `run_all.R` sources it once at
stage 5.

---

## Correctness gates

`run_all.R` asserts the paper's headline numbers as it goes and aborts on
mismatch, so a broken clone fails loudly rather than silently producing different
figures. All values below are from the last full run.

| Gate | Value | Stage |
|---|---|---|
| `B(Q4-2024)` vintage-restricted | 379.10 Mt | 1 |
| `clean` daily observations | 1,275 | 1 |
| `cca_wide` weekly observations | 676 | 1 |
| `lambda_ell_hat` (median, interior) | 0.102 | 2 |
| `S^int(400,t0)` at NB=400 (Table 9) | 21.60 USD/t | 4 |
| median `S*` over 2021-2026 | 31.9 USD/t | 4 |
| spatial convergence order | 1.02 | 4 |
| `tau*` onset, fitted λℓ | 2036 | 3 |
| Spearman ρ, premium vs net length | 0.525 | 4 |
| Spearman ρ, ex-touches | 0.697 | 4 |

`gamma` is gated only when the Borenstein package is present (see Data); the
pipeline otherwise runs on the pinned `gamma = 6.1`. Tolerances are loose relative
to reported precision — the solver is a finite-difference scheme and small
floating-point differences across platforms are expected. A gate that trips means
something structural changed, not rounding.

**λℓ is pinned at 0.10 for the model stages.** The fit returns 0.102 (IQR
[0.091, 0.116]); the paper reports 0.10 throughout, and every model quantity is
computed at the rounded value. λℓ is a bounded Q-measure object, not a precisely
measured one, so three significant figures would overstate the fit. Stage 2 gates
that the fit rounds to 0.10; stages 3+ use 0.10.

---

## Data

Everything in `data-raw/` is public and redistributable: CARB/WCI auction reports
and Compliance Instrument Reports, ICAP Allowance Price Explorer, California
Carbon Dashboard futures, CFTC Commitments of Traders, CARB MRR. See `DATA.md`
for per-file provenance, the CFTC filtered-extract policy, and the open
conventions (γ pooling, futures de-duplication).

**Not redistributed.** The abatement-cost calibration reads the Borenstein,
Bushnell, Wolak & Zaragoza-Watkins (2019) replication package from openICPSR;
its terms prohibit redistribution. `DATA.md` gives the DOI and names the files
needed. Without it the pipeline runs on the pinned `gamma = 6.1` and prints a
notice; only the gamma calibration itself is unreproducible.

---

## Not yet automated

Two paper outputs are not reproduced by this pipeline; both are documented rather
than silently missing.

1. **Figure 1 and the `[4.6, 7.5]` draw band on gamma** (`01-build_bau.R`). The
   BAU emissions fan and the per-draw gamma band were built interactively from
   the Borenstein simulation draws and have not been scripted. The point estimate
   `gamma = 6.09` is reproduced by `01-calibrate_gamma.R`; the band is not.
2. **Prediction 4.3's vintage-slope regression** (`test_vintage.R`). Note the
   printed HAC standard error (0.28) and an earlier reconstruction (0.27) differ
   in the third decimal; the interior-subsample definition or the HAC lag rule
   needs pinning before this is frozen.

Neither blocks `run_all.R`.

---

## Open items flagged during assembly

Found while building the package; each needs a decision or a one-line text edit,
none is structural. Detail in `DATA.md §4` and the notes below.

**ρ rounds to 0.52, not 0.53.** The full run gives Spearman ρ = 0.5246 on the
44-auction panel; the draft text and Table 7 report 0.53. The ex-touches value
(0.697 → 0.70) is fine. Confirm whether the published 0.53 came from a different
panel (one extra auction, or the 45-row pre-freeze frame) or is a rounding error
to correct.

**Figure 5's vacant band is [1.7%, 3.1%), not [1.7%, 3.5%].** The stage-5 output
confirms it directly: 16 floor auctions (9 at exactly the reserve), 0 in the gap,
29 interior. The caption's 3.5% is wrong, and §4.5's description of May 2026
(3.11%) as inside the band is false — it classifies interior. State the band as
1.7%–3.1% and describe May 2026 as at its upper edge.

**`g_y` fits to −2.91%, paper uses −2.87%.** `04-read_mrr.R` fits log-linear
covered emissions over 2015–2024 and returns −0.02906; the pinned baseline is
−0.02869. Reconcile the window (e.g. 2013–2024, or ex-2020) or state both. The
pinned value is used throughout the model.

**`gamma` pooling.** 6.09 as published vs 4.93 undoubled; see `DATA.md §4.1`. The
default reproduces the paper.

---

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

---

## Citation

Preprint, arXiv (forthcoming). Please cite the paper rather than this repository.
