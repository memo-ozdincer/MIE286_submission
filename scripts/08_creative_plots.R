# ── 08_creative_plots.R ──────────────────────────────────────────
# Creative analyses exploring patterns in the data.

library(tidyverse)

ct <- readRDS("output/clean_trials.rds")
pm <- readRDS("output/participant_means.rds")

fig_dir <- "../report/figures"
pal <- c("Visual" = "#2166AC", "Auditory" = "#D6604D")

ct_lab <- ct %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory"))


# ═══════════════════════════════════════════════════════════════
# 1. Post-miss RT trajectory: how does RT change after a miss?
#    Shows whether people slow down, and whether the effect
#    differs by condition.
# ═══════════════════════════════════════════════════════════════

post_miss <- ct %>%
  arrange(uid, condition, block, trial_num) %>%
  group_by(uid, condition, block) %>%
  mutate(
    prev_hit = lag(hit),
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
  filter(!is.na(prev_hit), trials_since_miss <= 8) %>%
  ungroup() %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory"))

rt_after_miss <- post_miss %>%
  group_by(Condition, trials_since_miss) %>%
  summarise(mean_rt = mean(rt), se = sd(rt)/sqrt(n()), n = n(), .groups = "drop")

p1 <- ggplot(rt_after_miss, aes(x = trials_since_miss, y = mean_rt,
                                 color = Condition, fill = Condition)) +
  geom_ribbon(aes(ymin = mean_rt - se, ymax = mean_rt + se), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = 0:8) +
  labs(x = "Trials Since Last Miss",
       y = "Mean Reaction Time (ms)",
       color = "Condition", fill = "Condition",
       caption = "0 = trial immediately after a miss. Shaded = SE.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.85, 0.85))

ggsave(file.path(fig_dir, "post_miss_rt.pdf"), p1, width = 7, height = 4.5)
cat("Saved: post_miss_rt.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 2. Gold-chasing: % of hits that are "fast" (<600ms) over
#    trial number, visual vs auditory.
#    Shows whether visual participants learn to chase speed.
# ═══════════════════════════════════════════════════════════════

fast_rate <- ct_lab %>%
  filter(hit == 1) %>%
  mutate(is_fast = as.integer(rt < 600)) %>%
  group_by(Condition, trial_num) %>%
  summarise(fast_pct = mean(is_fast) * 100, n = n(), .groups = "drop")

p2 <- ggplot(fast_rate, aes(x = trial_num, y = fast_pct, color = Condition)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 1.2) +
  scale_color_manual(values = pal) +
  labs(x = "Trial Number Within Block",
       y = "% of Hits That Are 'Fast' (<600ms)",
       color = "Condition",
       caption = "In the visual condition, 'fast' triggers a gold border glow.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.85, 0.2))

ggsave(file.path(fig_dir, "gold_chasing.pdf"), p2, width = 7, height = 4.5)
cat("Saved: gold_chasing.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 3. Fitts' Law: distance from previous target → RT
# ═══════════════════════════════════════════════════════════════

fitts <- ct %>%
  arrange(uid, condition, block, trial_num) %>%
  group_by(uid, condition, block) %>%
  mutate(
    prev_tx = lag(target_x),
    prev_ty = lag(target_y),
    target_dist = sqrt((target_x - prev_tx)^2 + (target_y - prev_ty)^2)
  ) %>%
  filter(!is.na(target_dist), rt < 2000) %>%
  ungroup() %>%
  mutate(Condition = ifelse(condition == "visual", "Visual", "Auditory"))

p3 <- ggplot(fitts, aes(x = target_dist, y = rt, color = Condition)) +
  geom_point(alpha = 0.05, size = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
  scale_color_manual(values = pal) +
  labs(x = "Distance from Previous Target (px)",
       y = "Reaction Time (ms)",
       color = "Condition",
       caption = "Positive slopes confirm a Fitts' Law-like relationship in our data.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.15, 0.85))

ggsave(file.path(fig_dir, "fitts_law.pdf"), p3, width = 7, height = 5)
cat("Saved: fitts_law.pdf\n")

# Fitts' Law stats
cat("\n=== FITTS' LAW ===\n")
for (cond in c("Visual", "Auditory")) {
  sub <- fitts %>% filter(Condition == cond)
  m <- lm(rt ~ target_dist, data = sub)
  cat(sprintf("%s: slope = %.2f ms/px, R² = %.4f, p = %s\n",
              cond, coef(m)[2], summary(m)$r.squared,
              format.pval(summary(m)$coefficients[2,4], digits = 3)))
}


# ═══════════════════════════════════════════════════════════════
# 4. Per-participant paired scatter: visual RT vs auditory RT
#    Each person is one point. Diagonal = no difference.
# ═══════════════════════════════════════════════════════════════

p4 <- ggplot(pm, aes(x = mean_rt_auditory, y = mean_rt_visual)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(color = order), size = 2.5, alpha = 0.7) +
  scale_color_manual(values = c("VA" = "#7C3AED", "AV" = "#059669"),
                     labels = c("AV" = "Aud first", "VA" = "Vis first")) +
  labs(x = "Auditory Mean RT (ms)", y = "Visual Mean RT (ms)",
       color = "Condition\nOrder",
       caption = "Points below diagonal = faster under visual. Points above = faster under auditory.") +
  coord_fixed() +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.15, 0.85))

ggsave(file.path(fig_dir, "paired_scatter.pdf"), p4, width = 6, height = 6)
cat("Saved: paired_scatter.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 5. Error direction: where do misses go?
#    Polar histogram of miss angle from target center.
# ═══════════════════════════════════════════════════════════════

misses <- ct_lab %>%
  filter(hit == 0) %>%
  mutate(
    dx = click_x - target_x,
    dy = click_y - target_y,
    angle = atan2(dy, dx) * 180 / pi
  )

p5 <- ggplot(misses, aes(x = angle, fill = Condition)) +
  geom_histogram(binwidth = 30, alpha = 0.6, position = "identity",
                 color = "white", linewidth = 0.3) +
  scale_fill_manual(values = pal) +
  coord_polar(start = -pi) +
  scale_x_continuous(breaks = seq(-150, 180, 30),
                     labels = c("", "", "", "Left", "", "", "Down", "", "", "Right", "", "Up")) +
  labs(x = "", y = "Count", fill = "Condition",
       caption = "Angular distribution of miss direction from target center.") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_blank())

ggsave(file.path(fig_dir, "miss_direction.pdf"), p5, width = 6, height = 5)
cat("Saved: miss_direction.pdf\n")


# ═══════════════════════════════════════════════════════════════
# 6. RT change after miss vs after hit, by condition
#    Do visual participants slow down more after a miss?
# ═══════════════════════════════════════════════════════════════

rt_context <- ct %>%
  arrange(uid, condition, block, trial_num) %>%
  group_by(uid, condition, block) %>%
  mutate(
    prev_hit = lag(hit),
    prev_rt  = lag(rt),
    rt_change = rt - prev_rt
  ) %>%
  filter(!is.na(prev_hit)) %>%
  ungroup() %>%
  mutate(
    Condition = ifelse(condition == "visual", "Visual", "Auditory"),
    Previous = ifelse(prev_hit == 1, "After Hit", "After Miss")
  )

rt_context_summary <- rt_context %>%
  group_by(Condition, Previous) %>%
  summarise(
    mean_rt = mean(rt),
    mean_change = mean(rt_change),
    se = sd(rt) / sqrt(n()),
    .groups = "drop"
  )

cat("\n=== RT AFTER HIT vs AFTER MISS, BY CONDITION ===\n")
print(rt_context_summary, n = Inf)

p6 <- ggplot(rt_context_summary,
             aes(x = Previous, y = mean_rt, fill = Condition)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se),
                position = position_dodge(0.6), width = 0.2) +
  scale_fill_manual(values = pal) +
  labs(x = "", y = "Mean RT (ms)", fill = "Condition",
       caption = "How much do participants slow down after a miss? Grouped by condition.") +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, "rt_after_miss_bar.pdf"), p6, width = 6, height = 4.5)
cat("Saved: rt_after_miss_bar.pdf\n")


cat("\n=== DONE: 08_creative_plots.R ===\n")
