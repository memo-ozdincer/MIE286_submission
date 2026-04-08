# ── 01_load_clean.R ──────────────────────────────────────────────
# Load all participant CSVs, clean, and aggregate per-participant means.
# Output: output/clean_trials.rds, output/participant_means.rds

library(tidyverse)

# ── Load all CSVs ────────────────────────────────────────────────
csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
cat("Found", length(csv_files), "CSV files\n")

raw <- csv_files %>%
  map_dfr(~ {
    df <- read_csv(.x, col_types = cols(.default = "c"))
    df$source_file <- basename(.x)
    df
  })

cat("Total raw rows:", nrow(raw), "\n")

# ── Parse and rename columns ─────────────────────────────────────
trials <- raw %>%
  mutate(
    rt        = as.numeric(reaction_time_ms),
    error     = as.numeric(radial_error),
    hit       = as.integer(is_hit),
    block     = as.integer(block),
    trial_num = as.integer(trial),
    condition = as.character(condition),
    pid       = as.character(participant_id),
    order     = as.character(condition_order),
    device    = as.character(device_type),
    gaming    = as.character(gaming_freq),
    target_x  = as.numeric(target_x),
    target_y  = as.numeric(target_y),
    click_x   = as.numeric(click_x),
    click_y   = as.numeric(click_y)
  ) %>%
  # Create unique participant ID from source file (handles repeat names)
  mutate(uid = source_file)

cat("Unique participants (by file):", n_distinct(trials$uid), "\n")

# ── Exclude RT outliers ──────────────────────────────────────────
n_before <- nrow(trials)
trials_clean <- trials %>%
  filter(rt >= 150, rt <= 8000)

n_excluded <- n_before - nrow(trials_clean)
cat("Excluded", n_excluded, "outlier trials (",
    round(n_excluded / n_before * 100, 2), "%)\n")
cat("Clean trials:", nrow(trials_clean), "\n")

# ── Save trial-level data ────────────────────────────────────────
dir.create("output", showWarnings = FALSE)
saveRDS(trials_clean, "output/clean_trials.rds")

# ── Aggregate per-participant means ──────────────────────────────
participant_by_condition <- trials_clean %>%
  group_by(uid, pid, order, device, gaming, condition) %>%
  summarise(
    mean_rt    = mean(rt),
    sd_rt      = sd(rt),
    mean_error = mean(error),
    sd_error   = sd(error),
    hit_rate   = mean(hit) * 100,
    n_trials   = n(),
    .groups    = "drop"
  )

# Wide format: one row per participant
participant_means <- participant_by_condition %>%
  pivot_wider(
    id_cols     = c(uid, pid, order, device, gaming),
    names_from  = condition,
    values_from = c(mean_rt, sd_rt, mean_error, sd_error, hit_rate, n_trials)
  )

# Check we have both conditions for every participant
complete <- participant_means %>%
  filter(!is.na(mean_rt_visual), !is.na(mean_rt_auditory))

cat("Participants with both conditions:", nrow(complete), "/", nrow(participant_means), "\n")

saveRDS(complete, "output/participant_means.rds")
cat("Saved: output/clean_trials.rds, output/participant_means.rds\n")
