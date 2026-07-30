# A Structural Model of WCI Allowance Pricing: replication package

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21608615.svg)](https://doi.org/10.5281/zenodo.21608615)

Code and data to reproduce every figure, table and reported number in the paper.

`Rscript run\\\_all.R` runs the full pipeline end to end.

\---

## Quickstart

```bash
git clone <repo> \\\&\\\& cd wci-structural-pricing
R -e 'renv::restore()'          # restores the exact package versions in renv.lock
Rscript run\\\_all.R               # full pipeline, \\\~30 min; figures land in paper/figures/
Rscript run\\\_all.R --quick       # \\\~3 min, coarse grids; gates relaxed, figures rough
```

Open the project through `wci-structural-pricing.Rproj` (or start R in the repo root) so `.Rprofile` activates renv. Stages checkpoint to `data-derived/`; `Rscript run\\\_all.R --stage=3` resumes from the model stage without re-running ingest.

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
\---

## Data

Everything in `data-raw/` is public and redistributable: CARB/WCI auction reports and Compliance Instrument Reports, ICAP Allowance Price Explorer, California Carbon Dashboard futures, CFTC Commitments of Traders, CARB MRR. See `DATA.md` for per-file provenance, the CFTC filtered-extract policy, and the open conventions (γ pooling, futures de-duplication).

**Not redistributed.** The abatement-cost calibration reads the Borenstein, Bushnell, Wolak \& Zaragoza-Watkins (2019) replication package from openICPSR. Its terms prohibit redistribution. `DATA.md` gives the DOI and names the files needed. Without it the pipeline runs on the pinned `gamma = 6.1` and prints a notice.

\---

## Environment

```r
install.packages("renv")
renv::restore()
```

R 4.5 (the lockfile records the exact version. earlier releases untested).
Package dependencies: `dplyr`, `tidyr`, `tibble`, `purrr`, `readr`, `readxl`, `stringr`, `curl`, plus their recursive dependencies as pinned in `renv.lock`.

Python 3.8+ with `pdfplumber` for the auction parser only (run offline, CSV output is committed).

\---

## Citation

Preprint, arXiv (forthcoming). Please cite the paper rather than this repository.
