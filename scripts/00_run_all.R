# ── 00_run_all.R ─────────────────────────────────────────────────
# Master script: sources everything in order.
# Run from the analysis/ directory: Rscript scripts/00_run_all.R

# Set working directory to analysis/ regardless of how script is called
setwd(tryCatch(
  { d <- dirname(rstudioapi::getSourceEditorContext()$path); file.path(d, "..") },
  error = function(e) {
    # When run via Rscript, use the script's own location
    args <- commandArgs(trailingOnly = FALSE)
    f <- sub("--file=", "", args[grep("--file=", args)])
    if (length(f)) dirname(dirname(f)) else getwd()
  }
))

cat("Working directory:", getwd(), "\n\n")

source("scripts/01_load_clean.R")
source("scripts/02_descriptives.R")
source("scripts/03_assumptions.R")
source("scripts/04_inferential.R")
source("scripts/05_plots.R")
source("scripts/06_secondary.R")

cat("\n=== ALL DONE ===\n")
