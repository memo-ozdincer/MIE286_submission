# ── 05_plots.R ───────────────────────────────────────────────────
# Generate all figures for the report. PDF to ../report/figures/.
# Input:  output/clean_trials.rds, output/participant_means.rds

library(tidyverse)

pm <- readRDS("output/participant_means.rds")
ct <- readRDS("output/clean_trials.rds")

fig_dir <- "../report/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

pal <- c("Visual" = "#2166AC", "Auditory" = "#D6604D")

# ═══════════════════════════════════════════════════════════════════
# BODY FIGURES
# ═══════════════════════════════════════════════════════════════════

# ── Figure 1: Paired Slope Plot (Spaghetti) ─────────────────────
pm_slope <- pm %>%
  select(uid, mean_rt_visual, mean_rt_auditory) %>%
  pivot_longer(cols = c(mean_rt_visual, mean_rt_auditory),
               names_to = "condition", values_to = "rt") %>%
  mutate(condition = ifelse(grepl("visual", condition), "Visual", "Auditory"),
         condition = factor(condition, levels = c("Visual", "Auditory")))

p1 <- ggplot(pm_slope, aes(x = condition, y = rt, group = uid)) +
  geom_line(alpha = 0.3, color = "grey50") +
  geom_point(aes(color = condition), size = 2, alpha = 0.7) +
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               linewidth = 1.5, color = "black", linetype = "dashed") +
  stat_summary(aes(group = 1), fun = mean, geom = "point",
               size = 4, color = "black", shape = 18) +
  scale_color_manual(values = pal) +
  labs(x = "Feedback Condition", y = "Mean Reaction Time (ms)",
       caption = "Grey lines connect individual participants. Diamond = grand mean.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

ggsave(file.path(fig_dir, "rt_spaghetti.pdf"), p1, width = 6, height = 5)
cat("Saved: rt_spaghetti.pdf\n")

# ── Figure 2: RT Density Overlay ────────────────────────────────
ct_dens <- ct %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory"))

p2 <- ggplot(ct_dens, aes(x = rt, fill = Condition, color = Condition)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  labs(x = "Reaction Time (ms)", y = "Density",
       fill = "Condition", color = "Condition") +
  coord_cartesian(xlim = c(150, 2000)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.85, 0.85))

ggsave(file.path(fig_dir, "rt_density.pdf"), p2, width = 7, height = 4)
cat("Saved: rt_density.pdf\n")

# ── Figure 3: SAT Scatter (kept, key figure) ────────────────────
ct_plot <- ct %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory"))

r_vis <- cor(ct_plot$rt[ct_plot$Condition == "Visual"],
             ct_plot$error[ct_plot$Condition == "Visual"])
r_aud <- cor(ct_plot$rt[ct_plot$Condition == "Auditory"],
             ct_plot$error[ct_plot$Condition == "Auditory"])

p3 <- ggplot(ct_plot, aes(x = rt, y = error, color = Condition)) +
  geom_point(alpha = 0.12, size = 0.7) +
  scale_color_manual(values = pal) +
  coord_cartesian(xlim = c(150, 2000), ylim = c(0, 100)) +
  labs(x = "Reaction Time (ms)", y = "Radial Error (px)",
       color = "Condition") +
  annotate("text", x = Inf, y = Inf,
           label = sprintf("r[vis] == %.3f", r_vis),
           parse = TRUE, hjust = 1.1, vjust = 1.5, size = 3.5, color = pal["Visual"]) +
  annotate("text", x = Inf, y = Inf,
           label = sprintf("r[aud] == %.3f", r_aud),
           parse = TRUE, hjust = 1.1, vjust = 3.0, size = 3.5, color = pal["Auditory"]) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.15, 0.85))

ggsave(file.path(fig_dir, "sat_scatter.pdf"), p3, width = 7, height = 5)
cat("Saved: sat_scatter.pdf\n")

# ── Figure 4: Learning Curve (RT by Trial Number) ───────────────
learning <- ct %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory")) %>%
  group_by(Condition, trial_num) %>%
  summarise(mean_rt = mean(rt), se_rt = sd(rt) / sqrt(n()),
            .groups = "drop")

p4 <- ggplot(learning, aes(x = trial_num, y = mean_rt, color = Condition)) +
  geom_line(linewidth = 0.6, alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1.2) +
  scale_color_manual(values = pal) +
  labs(x = "Trial Number Within Block", y = "Mean Reaction Time (ms)",
       color = "Condition") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.85, 0.85))

ggsave(file.path(fig_dir, "learning_curve.pdf"), p4, width = 7, height = 4)
cat("Saved: learning_curve.pdf\n")

# ── Figure 5: Click Scatter / Hit Map ───────────────────────────
ct_clicks <- ct %>%
  mutate(
    dx = click_x - target_x,
    dy = click_y - target_y,
    Condition = ifelse(condition == "visual", "Visual", "Auditory"),
    outcome = ifelse(hit == 1, "Hit", "Miss")
  )

p5 <- ggplot(ct_clicks, aes(x = dx, y = dy, color = Condition)) +
  geom_point(alpha = 0.08, size = 0.5) +
  annotate("path",
           x = 35 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 35 * sin(seq(0, 2 * pi, length.out = 100)),
           linewidth = 0.8, linetype = "dashed", color = "grey30") +
  geom_point(aes(x = 0, y = 0), color = "black", size = 2, shape = 3) +
  scale_color_manual(values = pal) +
  coord_fixed(xlim = c(-80, 80), ylim = c(-80, 80)) +
  labs(x = "Horizontal Offset from Target (px)",
       y = "Vertical Offset from Target (px)",
       color = "Condition",
       caption = "Dashed circle = 35 px hit zone. Cross = target center.") +
  facet_wrap(~Condition) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(file.path(fig_dir, "click_scatter.pdf"), p5, width = 8, height = 4.5)
cat("Saved: click_scatter.pdf\n")

# ═══════════════════════════════════════════════════════════════════
# APPENDIX FIGURES
# ═══════════════════════════════════════════════════════════════════

# ── RT Boxplot (moved to appendix) ──────────────────────────────
pm_long_rt <- pm %>%
  select(uid, mean_rt_visual, mean_rt_auditory) %>%
  pivot_longer(cols = c(mean_rt_visual, mean_rt_auditory),
               names_to = "condition", values_to = "rt") %>%
  mutate(condition = ifelse(grepl("visual", condition), "Visual", "Auditory"))

pa1 <- ggplot(pm_long_rt, aes(x = condition, y = rt, fill = condition)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.12, size = 1.5, alpha = 0.6) +
  scale_fill_manual(values = pal) +
  labs(x = "Feedback Condition", y = "Mean Reaction Time (ms)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

ggsave(file.path(fig_dir, "rt_boxplot.pdf"), pa1, width = 6, height = 4)
cat("Saved: rt_boxplot.pdf (appendix)\n")

# ── Error Boxplot (moved to appendix) ───────────────────────────
pm_long_err <- pm %>%
  select(uid, mean_error_visual, mean_error_auditory) %>%
  pivot_longer(cols = c(mean_error_visual, mean_error_auditory),
               names_to = "condition", values_to = "error") %>%
  mutate(condition = ifelse(grepl("visual", condition), "Visual", "Auditory"))

pa2 <- ggplot(pm_long_err, aes(x = condition, y = error, fill = condition)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.12, size = 1.5, alpha = 0.6) +
  scale_fill_manual(values = pal) +
  labs(x = "Feedback Condition", y = "Mean Radial Error (px)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

ggsave(file.path(fig_dir, "error_boxplot.pdf"), pa2, width = 6, height = 4)
cat("Saved: error_boxplot.pdf (appendix)\n")

# ── Covariate Bar (moved to appendix) ───────────────────────────
cov_data <- pm %>%
  mutate(overall_rt = (mean_rt_visual + mean_rt_auditory) / 2) %>%
  group_by(device) %>%
  summarise(
    mean_rt = mean(overall_rt), se_rt = sd(overall_rt) / sqrt(n()),
    n = n(), .groups = "drop"
  )

pa3 <- ggplot(cov_data, aes(x = reorder(device, mean_rt), y = mean_rt)) +
  geom_col(fill = "#4393C3", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt), width = 0.2) +
  geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5, size = 3.5) +
  labs(x = "Device Type", y = "Mean Reaction Time (ms)") +
  coord_cartesian(ylim = c(0, max(cov_data$mean_rt + cov_data$se_rt) * 1.15)) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave(file.path(fig_dir, "covariate_bar.pdf"), pa3, width = 6, height = 4)
cat("Saved: covariate_bar.pdf (appendix)\n")

# ── Residual Diagnostics for SAT Regression ─────────────────────
ct$condition <- factor(ct$condition, levels = c("auditory", "visual"))
model_int <- lm(error ~ rt * condition, data = ct)

pdf(file.path(fig_dir, "residuals.pdf"), width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(fitted(model_int), resid(model_int),
     pch = ".", col = rgb(0, 0, 0, 0.1),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "steelblue", lwd = 2)
qqnorm(resid(model_int), pch = ".", col = rgb(0, 0, 0, 0.1),
       main = "Q-Q of Residuals")
qqline(resid(model_int), col = "steelblue", lwd = 2)
dev.off()
cat("Saved: residuals.pdf (appendix)\n")

# ── Block 1 vs Block 2 Comparison ───────────────────────────────
block_means <- ct %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory")) %>%
  group_by(uid, block, Condition) %>%
  summarise(mean_rt = mean(rt), .groups = "drop")

pa5 <- ggplot(block_means, aes(x = factor(block), y = mean_rt, fill = Condition)) +
  geom_boxplot(width = 0.5, alpha = 0.7, position = position_dodge(0.6)) +
  scale_fill_manual(values = pal) +
  labs(x = "Block", y = "Mean Reaction Time (ms)", fill = "Condition") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave(file.path(fig_dir, "block_comparison.pdf"), pa5, width = 6, height = 4)
cat("Saved: block_comparison.pdf (appendix)\n")

# ── Gaming Frequency × Condition Interaction ─────────────────────
gaming_data <- pm %>%
  select(uid, gaming, mean_rt_visual, mean_rt_auditory) %>%
  pivot_longer(cols = c(mean_rt_visual, mean_rt_auditory),
               names_to = "condition", values_to = "rt") %>%
  mutate(Condition = ifelse(grepl("visual", condition), "Visual", "Auditory"),
         gaming = factor(gaming, levels = c("Never", "Rarely", "Weekly", "Daily")))

pa6 <- ggplot(gaming_data, aes(x = gaming, y = rt, fill = Condition)) +
  geom_boxplot(width = 0.6, alpha = 0.7, position = position_dodge(0.7)) +
  scale_fill_manual(values = pal) +
  labs(x = "Gaming Frequency", y = "Mean Reaction Time (ms)", fill = "Condition") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave(file.path(fig_dir, "gaming_interaction.pdf"), pa6, width = 7, height = 4)
cat("Saved: gaming_interaction.pdf (appendix)\n")

cat("\n=== DONE: 05_plots.R ===\n")
