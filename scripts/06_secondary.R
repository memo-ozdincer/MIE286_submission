# ── 06_secondary.R ───────────────────────────────────────────────
# Secondary analyses: tilt effect, condition order, RT variance.
# Input:  output/clean_trials.rds, output/participant_means.rds
# Output: console (copy stats into report)

library(tidyverse)

ct <- readRDS("output/clean_trials.rds")
pm <- readRDS("output/participant_means.rds")

# ── Tilt Effect (miss autocorrelation) ──────────────────────────
cat("\n=== TILT EFFECT ===\n")

tilt <- ct %>%
  arrange(uid, condition, block, trial_num) %>%
  group_by(uid, condition, block) %>%
  mutate(
    prev_hit = lag(hit),
    is_miss  = 1L - hit
  ) %>%
  filter(!is.na(prev_hit)) %>%
  ungroup()

miss_after_miss <- sum(tilt$is_miss == 1 & tilt$prev_hit == 0)
total_after_miss <- sum(tilt$prev_hit == 0)
miss_after_hit <- sum(tilt$is_miss == 1 & tilt$prev_hit == 1)
total_after_hit <- sum(tilt$prev_hit == 1)

cat("P(miss | prev miss) =", miss_after_miss, "/", total_after_miss,
    "=", round(miss_after_miss / total_after_miss * 100, 1), "%\n")
cat("P(miss | prev hit)  =", miss_after_hit, "/", total_after_hit,
    "=", round(miss_after_hit / total_after_hit * 100, 1), "%\n")

# Chi-square test
tilt_mat <- matrix(c(miss_after_miss, total_after_miss - miss_after_miss,
                     miss_after_hit, total_after_hit - miss_after_hit),
                   nrow = 2, byrow = TRUE)
colnames(tilt_mat) <- c("Miss", "Hit")
rownames(tilt_mat) <- c("After Miss", "After Hit")
cat("\nContingency table:\n")
print(tilt_mat)

tilt_test <- chisq.test(tilt_mat)
cat("\nChi-squared test:\n")
print(tilt_test)

# ── Condition Order Check ────────────────────────────────────────
cat("\n=== CONDITION ORDER CHECK ===\n")

va <- pm %>% filter(order == "VA")
av <- pm %>% filter(order == "AV")
cat("VA group: n =", nrow(va), "\n")
cat("AV group: n =", nrow(av), "\n")

# Overall mean RT per participant
pm$overall_rt <- (pm$mean_rt_visual + pm$mean_rt_auditory) / 2

order_test <- t.test(overall_rt ~ order, data = pm)
cat("\nIndependent t-test: Overall RT by condition order\n")
print(order_test)

cat("\nMean RT — VA:", round(mean(va$mean_rt_visual + va$mean_rt_auditory) / 2, 1),
    "ms, AV:", round(mean(av$mean_rt_visual + av$mean_rt_auditory) / 2, 1), "ms\n")

# ── RT Variance Comparison ──────────────────────────────────────
cat("\n=== RT VARIANCE COMPARISON ===\n")

var_vis <- var(pm$mean_rt_visual)
var_aud <- var(pm$mean_rt_auditory)
cat("Visual RT variance:", round(var_vis, 1), "(SD =", round(sqrt(var_vis), 1), ")\n")
cat("Auditory RT variance:", round(var_aud, 1), "(SD =", round(sqrt(var_aud), 1), ")\n")

var_test <- var.test(pm$mean_rt_visual, pm$mean_rt_auditory)
cat("\nF-test for variance equality:\n")
print(var_test)

# ── Block 1 vs Block 2 (Practice/Fatigue Effect) ─────────────────
cat("\n=== BLOCK COMPARISON ===\n")

block_rt <- ct %>%
  group_by(uid, block) %>%
  summarise(mean_rt = mean(rt), mean_error = mean(error), .groups = "drop") %>%
  pivot_wider(id_cols = uid, names_from = block,
              values_from = c(mean_rt, mean_error))

cat("Block 1 mean RT:", round(mean(block_rt$mean_rt_1, na.rm = TRUE), 1), "ms\n")
cat("Block 2 mean RT:", round(mean(block_rt$mean_rt_2, na.rm = TRUE), 1), "ms\n")

block_test <- t.test(block_rt$mean_rt_1, block_rt$mean_rt_2, paired = TRUE)
cat("\nPaired t-test: Block 1 vs Block 2 RT\n")
print(block_test)

d_block <- (mean(block_rt$mean_rt_1, na.rm = TRUE) - mean(block_rt$mean_rt_2, na.rm = TRUE)) /
           sd(block_rt$mean_rt_1 - block_rt$mean_rt_2, na.rm = TRUE)
cat("Cohen's d =", round(d_block, 3), "\n")

# ── Click Spatial Bias ───────────────────────────────────────────
cat("\n=== CLICK SPATIAL BIAS ===\n")
ct_miss <- ct %>% filter(hit == 0)
cat("Total misses:", nrow(ct_miss), "\n")
cat("Mean horizontal offset (click - target):", round(mean(ct_miss$click_x - ct_miss$target_x), 1), "px\n")
cat("Mean vertical offset (click - target):", round(mean(ct_miss$click_y - ct_miss$target_y), 1), "px\n")
cat("Mean miss distance:", round(mean(ct_miss$error), 1), "px\n")

# ── Per-Participant SAT Slopes ───────────────────────────────────
cat("\n=== PER-PARTICIPANT SAT SLOPES ===\n")

pp_slopes <- ct %>%
  group_by(uid, condition) %>%
  summarise(slope = coef(lm(error ~ rt))[2], .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = slope,
              names_prefix = "slope_")

cat("Visual SAT slope:   M =", round(mean(pp_slopes$slope_visual, na.rm=T), 5),
    ", SD =", round(sd(pp_slopes$slope_visual, na.rm=T), 5), "\n")
cat("Auditory SAT slope: M =", round(mean(pp_slopes$slope_auditory, na.rm=T), 5),
    ", SD =", round(sd(pp_slopes$slope_auditory, na.rm=T), 5), "\n")

slope_test <- t.test(pp_slopes$slope_visual, pp_slopes$slope_auditory, paired = TRUE)
cat("\nPaired t-test on per-participant SAT slopes:\n")
print(slope_test)

# ── Leaderboard Effect (Pre vs Post) ────────────────────────────
cat("\n=== LEADERBOARD EFFECT ===\n")
# First ~21 files chronologically were pre-leaderboard
file_times <- ct %>%
  group_by(uid) %>%
  summarise(source = first(source_file), .groups = "drop") %>%
  mutate(timestamp = as.numeric(str_extract(source, "\\d{13}"))) %>%
  arrange(timestamp) %>%
  mutate(rank = row_number())

# Mark first 21 as pre-leaderboard
pre_lb_uids <- file_times$uid[1:min(21, nrow(file_times))]
ct$leaderboard <- ifelse(ct$uid %in% pre_lb_uids, "Pre-leaderboard", "Post-leaderboard")

lb_summary <- ct %>%
  group_by(leaderboard) %>%
  summarise(
    n_participants = n_distinct(uid),
    mean_rt = mean(rt), mean_error = mean(error),
    hit_rate = mean(hit) * 100, .groups = "drop"
  )
cat("\n")
print(as.data.frame(lb_summary))

lb_means <- ct %>%
  group_by(uid, leaderboard) %>%
  summarise(mean_rt = mean(rt), .groups = "drop")

lb_test <- t.test(mean_rt ~ leaderboard, data = lb_means)
cat("\nIndependent t-test: RT by leaderboard presence\n")
print(lb_test)

# ── Edge vs Center Targets ──────────────────────────────────────
cat("\n=== EDGE VS CENTER TARGETS ===\n")
ct$edge <- ifelse(ct$target_x < 100 | ct$target_x > 700 |
                  ct$target_y < 100 | ct$target_y > 400, "Edge", "Center")

edge_summary <- ct %>%
  group_by(edge) %>%
  summarise(
    n = n(), mean_rt = round(mean(rt), 1),
    mean_error = round(mean(error), 1),
    hit_rate = round(mean(hit) * 100, 1), .groups = "drop"
  )
print(as.data.frame(edge_summary))

cat("\n=== DONE: 06_secondary.R ===\n")
