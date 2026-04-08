# ── 02_descriptives.R ────────────────────────────────────────────
# Generate summary tables for the report.
# Input:  output/clean_trials.rds, output/participant_means.rds
# Output: console tables (copy into LaTeX)

library(tidyverse)

pm <- readRDS("output/participant_means.rds")
ct <- readRDS("output/clean_trials.rds")

# ── TABLE 1: DVs by condition ────────────────────────────────────
cat("\n=== TABLE 1: DVs by Condition ===\n")
tab1 <- tibble(
  Measure = c("Reaction Time (ms)", "Radial Error (px)", "Hit Rate (%)"),
  Visual  = c(
    sprintf("%.0f ± %.0f", mean(pm$mean_rt_visual), sd(pm$mean_rt_visual)),
    sprintf("%.1f ± %.1f", mean(pm$mean_error_visual), sd(pm$mean_error_visual)),
    sprintf("%.1f ± %.1f", mean(pm$hit_rate_visual), sd(pm$hit_rate_visual))
  ),
  Auditory = c(
    sprintf("%.0f ± %.0f", mean(pm$mean_rt_auditory), sd(pm$mean_rt_auditory)),
    sprintf("%.1f ± %.1f", mean(pm$mean_error_auditory), sd(pm$mean_error_auditory)),
    sprintf("%.1f ± %.1f", mean(pm$hit_rate_auditory), sd(pm$hit_rate_auditory))
  )
)
print(tab1, n = Inf)

# ── TABLE 2: Covariates — Device Type ───────────────────────────
cat("\n=== TABLE 2a: By Device Type ===\n")
tab2a <- pm %>%
  group_by(device) %>%
  summarise(
    N = n(),
    Mean_RT = sprintf("%.0f", mean((mean_rt_visual + mean_rt_auditory) / 2)),
    Hit_Rate = sprintf("%.1f", mean((hit_rate_visual + hit_rate_auditory) / 2)),
    .groups = "drop"
  )
print(tab2a, n = Inf)

# ── TABLE 2: Covariates — Gaming Frequency ──────────────────────
cat("\n=== TABLE 2b: By Gaming Frequency ===\n")
tab2b <- pm %>%
  group_by(gaming) %>%
  summarise(
    N = n(),
    Mean_RT = sprintf("%.0f", mean((mean_rt_visual + mean_rt_auditory) / 2)),
    Hit_Rate = sprintf("%.1f", mean((hit_rate_visual + hit_rate_auditory) / 2)),
    .groups = "drop"
  )
print(tab2b, n = Inf)

# ── SAMPLE SUMMARY ──────────────────────────────────────────────
cat("\n=== SAMPLE SUMMARY ===\n")
cat("Total participants:", nrow(pm), "\n")
cat("Total clean trials:", nrow(ct), "\n")
cat("\nCondition order:\n")
print(table(pm$order))
cat("\nTrials per participant (visual):\n")
print(summary(pm$n_trials_visual))
cat("\nTrials per participant (auditory):\n")
print(summary(pm$n_trials_auditory))

cat("\n=== DONE: 02_descriptives.R ===\n")
