# ── 07_new_plots.R ───────────────────────────────────────────────
# Additional analyses and visualizations.
# Input:  output/clean_trials.rds, output/participant_means.rds

library(tidyverse)

ct <- readRDS("output/clean_trials.rds")
pm <- readRDS("output/participant_means.rds")

fig_dir <- "../report/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

pal <- c("Visual" = "#2166AC", "Auditory" = "#D6604D")

ct_lab <- ct %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory"))

# ═══════════════════════════════════════════════════════════════
# 1. 2D Density / Heatmap SAT (replaces misleading scatter)
# Shows the Boltzmann-like boundary: no high-RT + high-error zone
# ═══════════════════════════════════════════════════════════════

p_dens <- ggplot(ct_lab %>% filter(rt < 2000, error < 100),
                 aes(x = rt, y = error)) +
  geom_hex(bins = 40, alpha = 0.85) +
  scale_fill_viridis_c(option = "magma", name = "Trials") +
  facet_wrap(~Condition) +
  labs(x = "Reaction Time (ms)", y = "Radial Error (px)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 12))

ggsave(file.path(fig_dir, "sat_density.pdf"), p_dens, width = 9, height = 4.5)
cat("Saved: sat_density.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 2. Tilt Effect Visualization — miss probability over trials
#    after a miss vs after a hit
# ═══════════════════════════════════════════════════════════════

tilt_data <- ct %>%
  arrange(uid, condition, block, trial_num) %>%
  group_by(uid, condition, block) %>%
  mutate(
    prev_hit = lag(hit),
    prev2_hit = lag(hit, 2),
    trials_since_miss = {
      x <- integer(n())
      counter <- 999L
      for (i in seq_len(n())) {
        if (i == 1 || hit[i-1] == 0) counter <- 0L
        else counter <- counter + 1L
        x[i] <- counter
      }
      x
    }
  ) %>%
  filter(!is.na(prev_hit)) %>%
  ungroup()

# Miss rate by trials since last miss (recovery curve)
recovery <- tilt_data %>%
  filter(trials_since_miss <= 10) %>%
  group_by(trials_since_miss) %>%
  summarise(
    miss_rate = mean(1 - hit) * 100,
    n = n(),
    se = sqrt(miss_rate/100 * (1 - miss_rate/100) / n) * 100,
    .groups = "drop"
  )

p_tilt <- ggplot(recovery, aes(x = trials_since_miss, y = miss_rate)) +
  geom_col(fill = "#D6604D", alpha = 0.7, width = 0.7) +
  geom_errorbar(aes(ymin = pmax(0, miss_rate - se),
                    ymax = miss_rate + se),
                width = 0.3, color = "grey30") +
  geom_hline(yintercept = mean(1 - ct$hit) * 100,
             linetype = "dashed", color = "grey50") +
  annotate("text", x = 8, y = mean(1 - ct$hit) * 100 + 1.5,
           label = "Overall miss rate", size = 3, color = "grey40") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Trials Since Last Miss",
       y = "Miss Rate (%)",
       caption = "0 = the trial immediately after a miss. Dashed line = overall miss rate.") +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, "tilt_recovery.pdf"), p_tilt, width = 7, height = 4)
cat("Saved: tilt_recovery.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 3. Per-participant RT difference histogram
#    Shows the distribution of the within-subject effect
# ═══════════════════════════════════════════════════════════════

pm_diff <- pm %>%
  mutate(d_rt = mean_rt_visual - mean_rt_auditory)

p_diff <- ggplot(pm_diff, aes(x = d_rt)) +
  geom_histogram(binwidth = 20, fill = "#2166AC", alpha = 0.6,
                 color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = mean(pm_diff$d_rt), color = "#D6604D",
             linewidth = 1) +
  annotate("text", x = mean(pm_diff$d_rt) - 3, y = Inf,
           label = sprintf("Mean = %.1f ms", mean(pm_diff$d_rt)),
           hjust = 1, vjust = 2, size = 3.5, color = "#D6604D") +
  labs(x = "RT Difference: Visual - Auditory (ms)",
       y = "Number of Participants",
       caption = "Negative = faster under visual. Red line = mean difference.") +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, "rt_diff_hist.pdf"), p_diff, width = 6, height = 4)
cat("Saved: rt_diff_hist.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 4. Hit rate over trial number (accuracy fatigue curve)
# ═══════════════════════════════════════════════════════════════

acc_curve <- ct_lab %>%
  group_by(Condition, trial_num) %>%
  summarise(hit_rate = mean(hit) * 100, n = n(), .groups = "drop")

p_acc <- ggplot(acc_curve, aes(x = trial_num, y = hit_rate, color = Condition)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 1.2) +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(80, 100)) +
  labs(x = "Trial Number Within Block", y = "Hit Rate (%)",
       color = "Condition") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.85, 0.2))

ggsave(file.path(fig_dir, "accuracy_curve.pdf"), p_acc, width = 7, height = 4)
cat("Saved: accuracy_curve.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 5. Streak length distribution by condition
# ═══════════════════════════════════════════════════════════════

streaks <- ct %>%
  arrange(uid, condition, block, trial_num) %>%
  group_by(uid, condition, block) %>%
  mutate(
    streak_id = cumsum(c(1, diff(hit) != 0))
  ) %>%
  filter(hit == 1) %>%
  group_by(uid, condition, block, streak_id) %>%
  summarise(streak_len = n(), .groups = "drop") %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory"))

p_streak <- ggplot(streaks, aes(x = streak_len, fill = Condition)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = seq(0, 60, 5)) +
  coord_cartesian(xlim = c(1, 40)) +
  labs(x = "Hit Streak Length", y = "Count",
       fill = "Condition") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.8, 0.8))

ggsave(file.path(fig_dir, "streak_dist.pdf"), p_streak, width = 7, height = 4)
cat("Saved: streak_dist.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 6. Target position heatmap — RT by canvas region
# ═══════════════════════════════════════════════════════════════

p_heatmap <- ggplot(ct_lab %>% filter(rt < 2000),
                    aes(x = target_x, y = target_y, z = rt)) +
  stat_summary_hex(fun = mean, bins = 15) +
  scale_fill_viridis_c(option = "plasma", name = "Mean RT\n(ms)") +
  scale_y_reverse() +
  coord_fixed(ratio = 1) +
  labs(x = "Target X (px)", y = "Target Y (px)") +
  facet_wrap(~Condition) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(fig_dir, "position_heatmap.pdf"), p_heatmap, width = 9, height = 4)
cat("Saved: position_heatmap.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 7. Quadrant analysis — RT vs Error boundary
#    Counts in each quadrant to show the Boltzmann shape
# ═══════════════════════════════════════════════════════════════

rt_med <- median(ct$rt)
err_med <- median(ct$error)

quadrants <- ct_lab %>%
  filter(rt < 2000, error < 100) %>%
  mutate(
    quadrant = case_when(
      rt < rt_med & error < err_med ~ "Fast + Accurate",
      rt < rt_med & error >= err_med ~ "Fast + Inaccurate",
      rt >= rt_med & error < err_med ~ "Slow + Accurate",
      rt >= rt_med & error >= err_med ~ "Slow + Inaccurate"
    )
  )

quad_summary <- quadrants %>%
  group_by(Condition, quadrant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Condition) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cat("\n=== QUADRANT ANALYSIS ===\n")
cat("Median RT:", round(rt_med), "ms, Median Error:", round(err_med, 1), "px\n\n")
print(quad_summary %>% arrange(Condition, quadrant), n = Inf)

p_quad <- ggplot(quad_summary, aes(x = quadrant, y = pct, fill = Condition)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = pal) +
  labs(x = "", y = "Percentage of Trials (%)",
       fill = "Condition") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(fig_dir, "quadrant_bar.pdf"), p_quad, width = 7, height = 4)
cat("Saved: quadrant_bar.pdf\n")


cat("\n=== DONE: 07_new_plots.R ===\n")
