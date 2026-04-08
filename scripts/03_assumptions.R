# ── 03_assumptions.R ─────────────────────────────────────────────
# Check normality assumptions for paired t-tests.
# Input:  output/participant_means.rds
# Output: ../report/figures/qq_rt.pdf, ../report/figures/qq_error.pdf

library(tidyverse)

pm <- readRDS("output/participant_means.rds")

# ── Compute within-subject differences ──────────────────────────
d_rt    <- pm$mean_rt_visual - pm$mean_rt_auditory
d_error <- pm$mean_error_visual - pm$mean_error_auditory

# ── Shapiro-Wilk tests ──────────────────────────────────────────
cat("\n=== SHAPIRO-WILK: RT Differences ===\n")
sw_rt <- shapiro.test(d_rt)
print(sw_rt)

cat("\n=== SHAPIRO-WILK: Error Differences ===\n")
sw_err <- shapiro.test(d_error)
print(sw_err)

# ── Decision ────────────────────────────────────────────────────
cat("\n=== DECISIONS ===\n")
if (sw_rt$p.value >= 0.05) {
  cat("RT differences: Normal (p =", round(sw_rt$p.value, 3), ") → paired t-test OK\n")
} else {
  cat("RT differences: Non-normal (p =", round(sw_rt$p.value, 3), ") → use Wilcoxon\n")
}

if (sw_err$p.value >= 0.05) {
  cat("Error differences: Normal (p =", round(sw_err$p.value, 3), ") → paired t-test OK\n")
} else {
  cat("Error differences: Non-normal (p =", round(sw_err$p.value, 3), ") → use Wilcoxon\n")
}

# ── QQ Plots ────────────────────────────────────────────────────
fig_dir <- "../report/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

pdf(file.path(fig_dir, "qq_rt.pdf"), width = 5, height = 4)
par(mar = c(4, 4, 2, 1))
qqnorm(d_rt, main = "Q-Q Plot: RT Differences (Visual − Auditory)",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles (ms)")
qqline(d_rt, col = "steelblue", lwd = 2)
dev.off()

pdf(file.path(fig_dir, "qq_error.pdf"), width = 5, height = 4)
par(mar = c(4, 4, 2, 1))
qqnorm(d_error, main = "Q-Q Plot: Error Differences (Visual − Auditory)",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles (px)")
qqline(d_error, col = "steelblue", lwd = 2)
dev.off()

cat("\nSaved: qq_rt.pdf, qq_error.pdf\n")
cat("\n=== DONE: 03_assumptions.R ===\n")
