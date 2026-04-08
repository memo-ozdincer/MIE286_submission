# ── 04_inferential.R ─────────────────────────────────────────────
# Run the three main hypothesis tests.
# Input:  output/participant_means.rds, output/clean_trials.rds
# Output: console (copy stats into report)

library(tidyverse)

pm <- readRDS("output/participant_means.rds")
ct <- readRDS("output/clean_trials.rds")

# ── H1: Reaction Time — Paired t-test (two-tailed) ──────────────
cat("\n=== H1: REACTION TIME ===\n")
cat("H0: mu_visual_rt = mu_auditory_rt\n")
cat("H1: mu_visual_rt ≠ mu_auditory_rt (two-tailed)\n\n")

d_rt <- pm$mean_rt_visual - pm$mean_rt_auditory
h1_test <- t.test(pm$mean_rt_visual, pm$mean_rt_auditory,
                  paired = TRUE, alternative = "two.sided")
print(h1_test)

cohen_d_rt <- mean(d_rt) / sd(d_rt)
cat("\nCohen's d =", round(cohen_d_rt, 3), "\n")
cat("Mean difference =", round(mean(d_rt), 1), "ms\n")
cat("SD of differences =", round(sd(d_rt), 1), "ms\n")

# ── H2: Radial Error — Wilcoxon signed-rank (one-tailed) ────────
# Shapiro-Wilk rejected normality for error differences → nonparametric
cat("\n=== H2: RADIAL ERROR (Wilcoxon — normality violated) ===\n")
cat("H0: median(visual_error - auditory_error) <= 0\n")
cat("H1: median(visual_error - auditory_error) > 0 (one-tailed)\n\n")

d_error <- pm$mean_error_visual - pm$mean_error_auditory
h2_test <- wilcox.test(pm$mean_error_visual, pm$mean_error_auditory,
                       paired = TRUE, alternative = "greater")
print(h2_test)

# Also report paired t-test for comparison (sensitivity check)
h2_ttest <- t.test(pm$mean_error_visual, pm$mean_error_auditory,
                   paired = TRUE, alternative = "greater")
cat("\nSensitivity check — paired t-test:\n")
print(h2_ttest)

cohen_d_err <- mean(d_error) / sd(d_error)
cat("\nCohen's d =", round(cohen_d_err, 3), "\n")
cat("Mean difference =", round(mean(d_error), 2), "px\n")
cat("SD of differences =", round(sd(d_error), 2), "px\n")
cat("Median difference =", round(median(d_error), 2), "px\n")

# ── H3: SAT Structure ───────────────────────────────────────────
cat("\n=== H3: SPEED-ACCURACY TRADEOFF STRUCTURE ===\n")

# PRIMARY: Multiple regression with interaction (Week 12-13)
cat("\n--- Multiple Regression: error ~ rt * condition ---\n")
ct$condition <- factor(ct$condition, levels = c("auditory", "visual"))
model_int <- lm(error ~ rt * condition, data = ct)
cat("\nModel summary:\n")
print(summary(model_int))
cat("\nANOVA table:\n")
print(anova(model_int))

# SUPPORTING: Pearson correlation per condition (Week 2-3)
cat("\n--- Pearson Correlations per Condition ---\n")
vis_trials <- ct %>% filter(condition == "visual")
aud_trials <- ct %>% filter(condition == "auditory")

r_vis <- cor.test(vis_trials$rt, vis_trials$error)
r_aud <- cor.test(aud_trials$rt, aud_trials$error)

cat("\nVisual:   r =", round(r_vis$estimate, 3),
    ", p =", format.pval(r_vis$p.value, digits = 3),
    ", 95% CI [", round(r_vis$conf.int[1], 3), ",", round(r_vis$conf.int[2], 3), "]\n")
cat("Auditory: r =", round(r_aud$estimate, 3),
    ", p =", format.pval(r_aud$p.value, digits = 3),
    ", 95% CI [", round(r_aud$conf.int[1], 3), ",", round(r_aud$conf.int[2], 3), "]\n")

# SUPPORTING: Simple linear regression per condition (Week 11)
cat("\n--- Simple Regression per Condition ---\n")
lm_vis <- lm(error ~ rt, data = vis_trials)
lm_aud <- lm(error ~ rt, data = aud_trials)

cat("\nVisual:   slope =", round(coef(lm_vis)[2], 4),
    ", R² =", round(summary(lm_vis)$r.squared, 4), "\n")
cat("Auditory: slope =", round(coef(lm_aud)[2], 4),
    ", R² =", round(summary(lm_aud)$r.squared, 4), "\n")

cat("\n=== DONE: 04_inferential.R ===\n")
