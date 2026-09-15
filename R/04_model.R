# 04_model.R
# Train the three-stage projection model
#
# Stage 1: Minutes model (lme4 mixed effects) — used as prior, overridden manually
# Stage 2: Shots model (glmmTMB Negative Binomial) — volume
# Stage 3: Goals model (glmmTMB Poisson) — quality conditional on shots
# Stage 3b: Assists / xA model (glmmTMB Zero-Inflated Poisson)
# Stage 3c: Shots on Target (binomial conditional on shots)

library(tidyverse)
library(lme4)
library(glmmTMB)

# ── Load processed data ────────────────────────────────────────────────────────

load_model_data <- function() {
  df <- readRDS("data/processed/model_dataset.rds")

  # Only use rows where player actually played meaningful minutes
  df <- df %>%
    filter(minutes_played >= 10) %>%
    mutate(
      is_home = as.integer(is_home),
      log_minutes = log(minutes_played / 90),

      # Winsorize extreme rates to prevent outlier blowup
      roll_xg_per_shot      = pmin(roll_xg_per_shot, 0.5, na.rm = TRUE),
      roll_xpass_diff       = pmin(pmax(coalesce(roll_xpass_diff,       0), -50), 50),
      roll_receiving_g_plus = pmin(pmax(coalesce(roll_receiving_g_plus, 0),  -3),  3)
    )

  df
}

# ── Stage 1: Minutes Model ────────────────────────────────────────────────────
# Used as the baseline — your manual override replaces this for daily projections
# But useful to understand rotation patterns

train_minutes_model <- function(df) {
  message("Training minutes model...")

  m <- lmer(
    minutes_played ~
      is_home +
      days_rest +
      roll_minutes +           # recent minutes trend
      minutes_sd_L10 +         # consistency
      games_played_L10 +       # participation rate
      (1 | player_id) +        # player random effect
      (1 | team_id),           # team random effect (rotation style)
    data    = df,
    REML    = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )

  message("Minutes model trained (singular fit warnings are normal for sparse random effects)")
  m
}

# ── Stage 2: Shots Model (Negative Binomial) ──────────────────────────────────
# NB2 handles the overdispersion in shot counts better than Poisson

train_shots_model <- function(df) {
  message("Training shots model (Negative Binomial)...")

  m <- glmmTMB(
    shots ~
      offset(log_minutes) +
      is_home +
      days_rest +
      roll_shots_per90 +
      roll_xg_per_shot +
      opp_shots_conceded_L10 +
      opp_xg_conceded_L10 +
      roll_receiving_g_plus +          # positioning in dangerous areas → more attempts
      (1 | player_id),
    family = nbinom2,
    data   = df %>% filter(!is.na(opp_shots_conceded_L10))
  )

  message("Shots model AIC: ", round(AIC(m), 1))
  m
}

# ── Stage 3: Goals Model (Poisson) ────────────────────────────────────────────
# Rate per shot — separates volume from quality

train_goals_model <- function(df) {
  message("Training goals model (Poisson)...")

  df_shots <- df %>% filter(shots > 0)  # conditional on attempting a shot

  m <- glmmTMB(
    goals ~
      offset(log(shots + 0.5)) +
      roll_xg_per_shot +         # player's shot quality
      opp_xg_conceded_L10 +      # opponent GK/defensive quality
      is_home +
      (1 | player_id),
    family = poisson,
    data   = df_shots
  )

  message("Goals model AIC: ", round(AIC(m), 1))
  m
}

# ── Stage 3b: Shots on Target (Binomial given shots) ──────────────────────────

train_shots_ot_model <- function(df) {
  message("Training shots on target model (Binomial)...")

  df_shots <- df %>%
    filter(shots > 0) %>%
    mutate(
      shots_off_target = shots - shots_on_target,
      shots_off_target = pmax(shots_off_target, 0)
    )

  m <- glmmTMB(
    cbind(shots_on_target, shots_off_target) ~
      roll_shots_ot_per90 / roll_shots_per90 +   # historical on-target rate
      opp_shots_ot_conceded_L10 +
      is_home +
      (1 | player_id),
    family = binomial,
    data   = df_shots
  )

  message("Shots OT model AIC: ", round(AIC(m), 1))
  m
}

# ── Stage 3c: Assists Model (Zero-Inflated Poisson) ───────────────────────────
# ZIP because most players get 0 assists most games

train_assists_model <- function(df) {
  message("Training assists model (Zero-Inflated Poisson)...")

  m <- glmmTMB(
    assists ~
      offset(log_minutes) +
      roll_xassists_per90 +
      roll_key_passes_per90 +
      opp_xg_conceded_L10 +
      is_home +
      roll_xpass_diff +                # completing harder passes than expected → creative edge
      (1 | player_id),
    ziformula = ~ roll_xassists_per90,
    family    = poisson,
    data      = df
  )

  message("Assists model AIC: ", round(AIC(m), 1))
  m
}

# ── Train and save all models ──────────────────────────────────────────────────

train_all_models <- function() {

  df <- load_model_data()

  models <- list(
    minutes   = train_minutes_model(df),
    shots     = train_shots_model(df),
    goals     = train_goals_model(df),
    shots_ot  = train_shots_ot_model(df),
    assists   = train_assists_model(df)
  )

  saveRDS(models, "data/processed/models.rds")
  message("All models saved to data/processed/models.rds")

  models
}

# To run (from the project root):
# source("R/00_setup.R")
# models <- train_all_models()
