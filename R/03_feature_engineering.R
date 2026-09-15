# 03_feature_engineering.R
# Build the modeling dataset from raw ASA pulls
# Key outputs: per-game player rows with all features needed for the model

library(tidyverse)
library(zoo)
library(lubridate)

# ── Load raw data ──────────────────────────────────────────────────────────────

load_raw <- function() {
  ga_path <- "data/raw/asa_goals_added.rds"
  list(
    asa_xgoals      = readRDS("data/raw/asa_xgoals.rds"),
    asa_xpass       = readRDS("data/raw/asa_xpass.rds"),
    asa_goals_added = if (file.exists(ga_path)) readRDS(ga_path) else NULL,
    asa_team_xg     = readRDS("data/raw/asa_team_xgoals.rds"),
    asa_games       = readRDS("data/raw/asa_games.rds")
  )
}

# ── Decay-weighted rolling average ────────────────────────────────────────────
# More recent games weighted heavier — critical for capturing form

decay_roll <- function(x, n = 10, decay = 0.85) {
  weights <- decay ^ (seq(n - 1, 0))
  rollapply(
    x,
    width   = n,
    FUN     = function(vals) weighted.mean(vals, weights[seq_along(vals)], na.rm = TRUE),
    fill    = NA,
    align   = "right",
    partial = TRUE
  )
}

# ── 1. Build player-game rows from ASA xgoals ──────────────────────────────────

build_player_game_rows <- function(asa_xg, asa_xpass, asa_goals_added = NULL) {

  # ASA xgoals has: player_id, player_name, team_id, season, game_id,
  # date, minutes_played, shots, shots_on_target, goals, xgoals, xplace,
  # key_passes, xassists, assists, position_general

  # xgoals already contains key_passes, xassists, primary_assists
  base <- asa_xg %>%
    select(
      player_id, player_name, team_id, season_name,
      game_id, date, minutes_played,
      shots, shots_on_target, goals,
      xgoals, xplace,
      key_passes, xassists,
      primary_assists,
      general_position
    ) %>%
    rename(
      position_general = general_position,
      assists          = primary_assists
    ) %>%
    mutate(date = as.Date(date))

  # Join xpass for passing completion metrics (pass volume / accuracy)
  pass_cols <- asa_xpass %>%
    select(player_id, game_id,
           pass_completion_percentage,
           xpass_completion_percentage,
           attempted_passes) %>%
    distinct(player_id, game_id, .keep_all = TRUE)

  base <- base %>%
    left_join(pass_cols, by = c("player_id", "game_id")) %>%
    mutate(xpass_diff = pass_completion_percentage - xpass_completion_percentage)

  # Goals Added — join passing + receiving components only
  if (!is.null(asa_goals_added) && nrow(asa_goals_added) > 0) {
    message("  Goals Added columns available: ",
            paste(grep("passing|receiving", names(asa_goals_added), value = TRUE), collapse = ", "))
    ga_cols <- asa_goals_added %>%
      select(player_id, game_id,
             any_of(c("passing_goals_added_for",   "passing_goals_added_raw",
                      "receiving_goals_added_for",  "receiving_goals_added_raw"))) %>%
      distinct(player_id, game_id, .keep_all = TRUE)

    # Normalise column names regardless of ASA naming variant
    if ("passing_goals_added_raw"  %in% names(ga_cols))
      ga_cols <- rename(ga_cols, passing_goals_added_for  = passing_goals_added_raw)
    if ("receiving_goals_added_raw" %in% names(ga_cols))
      ga_cols <- rename(ga_cols, receiving_goals_added_for = receiving_goals_added_raw)

    base <- base %>% left_join(ga_cols, by = c("player_id", "game_id"))
  } else {
    base <- base %>%
      mutate(passing_goals_added_for  = NA_real_,
             receiving_goals_added_for = NA_real_)
  }

  base
}

# ── 2. Build opponent defensive profile (position-specific) ───────────────────
# This is the key differentiator — not just team-level xGA, but by position of attacker

build_opp_defense <- function(asa_xg, asa_games) {

  # For each team-game, calculate xG conceded to each position group
  # asa_xg already has home_team_id and away_team_id from the pull join
  opp_profile <- asa_xg %>%
    rename(position_general = general_position) %>%
    mutate(
      date        = as.Date(date),
      opp_team_id = case_when(
        team_id == home_team_id ~ away_team_id,
        team_id == away_team_id ~ home_team_id,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(opp_team_id)) %>%
    group_by(opp_team_id, game_id, date, position_general) %>%
    summarise(
      xg_conceded          = sum(xgoals, na.rm = TRUE),
      shots_conceded       = sum(shots, na.rm = TRUE),
      shots_ot_conceded    = sum(shots_on_target, na.rm = TRUE),
      goals_conceded       = sum(goals, na.rm = TRUE),
      .groups = "drop"
    )

  # Rolling L10 defensive average per team per position
  opp_rolling <- opp_profile %>%
    arrange(opp_team_id, position_general, date) %>%
    group_by(opp_team_id, position_general) %>%
    mutate(
      opp_xg_conceded_L10      = decay_roll(xg_conceded, n = 10),
      opp_shots_conceded_L10   = decay_roll(shots_conceded, n = 10),
      opp_shots_ot_conceded_L10 = decay_roll(shots_ot_conceded, n = 10),
      opp_goals_conceded_L10   = decay_roll(goals_conceded, n = 10)
    ) %>%
    ungroup()

  opp_rolling
}

# ── 3. Build rolling player features ──────────────────────────────────────────

build_player_rolling <- function(player_games) {

  # Guard: ensure goals_added / xpass columns exist even if upstream join found nothing
  if (!"passing_goals_added_for"  %in% names(player_games))
    player_games$passing_goals_added_for  <- NA_real_
  if (!"receiving_goals_added_for" %in% names(player_games))
    player_games$receiving_goals_added_for <- NA_real_
  if (!"xpass_diff" %in% names(player_games))
    player_games$xpass_diff <- NA_real_

  player_games %>%
    arrange(player_id, date) %>%
    group_by(player_id) %>%
    mutate(
      # Per-90 rates (scale by minutes)
      shots_per90         = shots / (minutes_played / 90),
      shots_ot_per90      = shots_on_target / (minutes_played / 90),
      goals_per90         = goals / (minutes_played / 90),
      xg_per90            = xgoals / (minutes_played / 90),
      xg_per_shot         = ifelse(shots > 0, xgoals / shots, NA),   # shot quality
      key_passes_per90    = key_passes / (minutes_played / 90),
      xassists_per90      = xassists / (minutes_played / 90),

      # Rolling L10 decay-weighted averages
      roll_shots_per90      = decay_roll(shots_per90, n = 10),
      roll_shots_ot_per90   = decay_roll(shots_ot_per90, n = 10),
      roll_xg_per90         = decay_roll(xg_per90, n = 10),
      roll_xg_per_shot      = decay_roll(xg_per_shot, n = 10),
      roll_xassists_per90   = decay_roll(xassists_per90, n = 10),
      roll_key_passes_per90 = decay_roll(key_passes_per90, n = 10),
      roll_minutes          = decay_roll(minutes_played, n = 10),

      # Minutes consistency (std dev — high variance = wider projections)
      minutes_sd_L10 = rollapply(minutes_played, 10, sd, fill = NA, align = "right", partial = TRUE),

      # Games played in last 10 (participation rate)
      games_played_L10 = rollapply(minutes_played > 0, 10, sum, fill = NA, align = "right", partial = TRUE),

      # ── Dev features (xPass diff + Goals Added components) ──────────────────
      # xPass diff: positive = completing harder passes than expected (creative, precise)
      roll_xpass_diff      = decay_roll(coalesce(xpass_diff, 0), n = 10),

      # Passing g+/90: creative passing value beyond just chances created
      # Cap at ±3 to prevent Inf from short appearances blowing up rolling averages
      passing_g_plus_per90 = pmin(pmax(
        passing_goals_added_for / (minutes_played / 90), -3, na.rm = TRUE), 3, na.rm = TRUE),
      roll_passing_g_plus  = decay_roll(coalesce(passing_g_plus_per90, 0), n = 10),

      # Receiving g+/90: how well a player gets on the ball in dangerous areas
      receiving_g_plus_per90 = pmin(pmax(
        receiving_goals_added_for / (minutes_played / 90), -3, na.rm = TRUE), 3, na.rm = TRUE),
      roll_receiving_g_plus  = decay_roll(coalesce(receiving_g_plus_per90, 0), n = 10)
    ) %>%
    ungroup()
}

# ── 4. Add game context features ──────────────────────────────────────────────

add_game_context <- function(player_games, asa_games) {

  game_ctx <- asa_games %>%
    select(game_id, home_team_id, away_team_id,
           home_score, away_score, expanded_minutes)

  player_games %>%
    left_join(game_ctx, by = "game_id") %>%
    mutate(
      is_home = team_id == home_team_id,

      # Days rest per player
      days_rest = as.numeric(date - lag(date)),

      team_score = ifelse(is_home, home_score, away_score),
      opp_score  = ifelse(is_home, away_score, home_score)
    )
}

# ── 5. Final join — player-game rows with all features ────────────────────────

build_model_dataset <- function() {

  raw <- load_raw()

  message("Building player-game rows...")
  player_games <- build_player_game_rows(raw$asa_xgoals, raw$asa_xpass, raw$asa_goals_added)

  message("Building opponent defensive profiles...")
  opp_defense <- build_opp_defense(raw$asa_xgoals, raw$asa_games)

  message("Computing rolling player features...")
  player_rolling <- build_player_rolling(player_games)

  message("Adding game context...")
  player_ctx <- add_game_context(player_rolling, raw$asa_games)

  # opp_team_id already computed in build_opp_defense; derive it on player_ctx too
  message("Joining opponent features...")
  player_ctx <- player_ctx %>%
    mutate(
      opp_team_id = case_when(
        team_id == home_team_id ~ away_team_id,
        team_id == away_team_id ~ home_team_id,
        TRUE ~ NA_character_
      )
    )

  full_dataset <- player_ctx %>%
    left_join(
      opp_defense %>%
        select(opp_team_id, game_id, position_general,
               opp_xg_conceded_L10, opp_shots_conceded_L10,
               opp_shots_ot_conceded_L10, opp_goals_conceded_L10),
      by = c("opp_team_id", "game_id", "position_general")
    ) %>%
    filter(!is.na(roll_shots_per90))  # drop rows with no rolling history

  saveRDS(full_dataset, "data/processed/model_dataset.rds")
  message("Model dataset saved: ", nrow(full_dataset), " rows")

  full_dataset
}

# Helper: add opp_team_id to game roster
asa_games_with_opp <- function(asa_games) {
  bind_rows(
    asa_games %>% transmute(game_id, team_id = home_team_id, opp_team_id = away_team_id),
    asa_games %>% transmute(game_id, team_id = away_team_id, opp_team_id = home_team_id)
  )
}

# To run (from the project root):
# source("R/00_setup.R")
# model_data <- build_model_dataset()
