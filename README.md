# MIE 286 Final Project — Team 51

**Title:** The Effect of Feedback Modality on Speed-Accuracy Tradeoff in a Visual Target-Selection Task

**Team:** Markiyan Konyk, Jessica Yi, Memo Ozdincer

## Structure

```
MIE286_submission/
├── main.pdf          <- Final report (compiled)
├── main.tex          <- LaTeX source
├── figures/          <- All figures (PDF + PNG)
├── data/             <- Raw participant CSVs (N=72, analysis uses N=41 60-trial subset)
├── scripts/          <- R analysis pipeline
│   ├── 00_run_all.R       <- Master script (run this)
│   ├── 01_load_clean.R    <- Load CSVs, clean outliers, aggregate
│   ├── 02_descriptives.R  <- Summary tables
│   ├── 03_assumptions.R   <- Shapiro-Wilk, QQ plots
│   ├── 04_inferential.R   <- H1 (paired t), H2 (Wilcoxon), H3 (regression)
│   ├── 05_plots.R         <- Main figures
│   ├── 06_secondary.R     <- Tilt, order check, covariates
│   ├── 07_new_plots.R     <- Additional visualizations
│   └── 08_creative_plots.R <- Exploratory analyses
└── README.md
```

## Reproducing the Analysis

```r
# From the submission root directory:
setwd("path/to/MIE286_submission")
source("scripts/00_run_all.R")
```

Requires: R 4.5+, tidyverse, hexbin
# MIE286_submission
