# Analysis code — Prognostic validity of Disability and Pain Intensity by clinical examination at Three-Month Follow-Up in Participants with Acute Neck Pain

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22673331.svg)](https://doi.org/10.5281/zenodo.22673331)

R code for all analyses reported in the manuscript (working title above).

## Citation

Archived on Zenodo. Concept DOI (all versions): https://doi.org/10.5281/zenodo.22673331
Version v1.0.0 DOI: https://doi.org/10.5281/zenodo.22673332

## Contents

- `Petra_longneck_update_260612.R` — the full analysis pipeline: variable derivation,
  descriptive tables, missingness handling (rows with >30% missing values excluded,
  n = 156 → 152), kNN imputation (VIM), best-subset regression with BIC selection for all
  outcome models, and the bootstrap internal validation (Harrell optimism correction,
  B = 500, seed 42).
- `no_baseline_reselection.R` — helper for the models without the baseline outcome
  (re-selection of the best subset among the remaining candidates).
- `generate_manuscript_tables.R` — runs the pipeline once and exports the manuscript
  regression tables and the internal-validation table as .docx/.html (written to `./results/`).

## Data

The individual participant data (`longneckPetra.xlsx`) are **not** included in this
repository. The scripts expect the file in the working directory. Data may be obtained
from the corresponding author upon reasonable request.

## Running

```sh
Rscript --vanilla Petra_longneck_update_260612.R    # full pipeline
Rscript --vanilla generate_manuscript_tables.R      # pipeline + publication-ready tables
```

R packages are loaded via `pacman` at the top of the main script. See `sessionInfo` output
produced by the runs for exact versions.
