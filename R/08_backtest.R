# 08_backtest.R
# Two-part backtest system:
#
#   Part 1 — Calibration (run immediately on model_dataset)
#     Refit models on 2023-2024, predict on 2025 out-of-sample
#     Reports: MAE/RMSE, Brier scores, calibration curves per stat
#
#   Part 2 — Projection accuracy tracking (accumulates week-over-week)
#     log_projections()          → saves each week's sheet to data/backtest/projections_log.csv
#     log_outcomes()              → pulls actual results from ASA after games finish
#     track_projection_accuracy() → hit-rate / calibration report on logged data
#
#   Part 3 — Dev feature comparison (A/B test candidate features before promoting)

library(tidyverse)
library(glmmTMB)
library(lme4)
library(httr)
library(jsonlite)

BACKTEST_DIR  <- "data/backtest"
PROJ_LOG_PATH <- file.path(BACKTEST_DIR, "projections_log.csv")
OUT_LOG_PATH  <- file.path(BACKTEST_DIR, "outcomes_log.csv")

ASA_BASE <- "https://app.americansocceranalysis.com/api/v1/mls"

# ── Helper: refit all 4 models on a training subset ───────────────────────────

refit_models <- function(df) {

  df <- df %>%
    filter(minutes_played >= 10) %>%
    mutate(
      is_home          = as.integer(is_home),
      log_minutes      = log(minutes_played / 90),
      roll_xg_per_shot = pmin(roll_xg_per_shot, 0.5, na.rm = TRUE)
    )

  message("  Fitting shots model...")
  shots_m <- glmmTMB(
    shots ~
      offset(log_minutes) +
      is_home + days_rest +
      roll_shots_per90 + roll_xg_per_shot +
      opp_shots_conceded_L10 + opp_xg_conceded_L10 +
      (1 | player_id),
    family = nbinom2,
    data   = df %>% filter(!is.na(opp_shots_conceded_L10))
  )

  message("  Fitting shots-on-target model...")
  shots_ot_m <- glmmTMB(
    cbind(shots_on_target, pmax(shots - shots_on_target, 0)) ~
      roll_shots_ot_per90 / roll_shots_per90 +
      opp_shots_ot_conceded_L10 + is_home +
      (1 | player_id),
    family = binomial,
    data   = df %>% filter(shots > 0)
  )

  message("  Fitting goals model...")
  goals_m <- glmmTMB(
    goals ~
      offset(log(shots + 0.5)) +
      roll_xg_per_shot + opp_xg_conceded_L10 + is_home +
      (1 | player_id),
    family = poisson,
    data   = df %>% filter(shots > 0)
  )

  message("  Fitting assists model...")
  assists_m <- glmmTMB(
    assists ~
      offset(log_minutes) +
      roll_xassists_per90 + roll_key_passes_per90 +
      opp_xg_conceded_L10 + is_home +
      (1 | player_id),
    ziformula = ~ roll_xassists_per90,
    family    = poisson,
    data      = df
  )

  list(shots = shots_m, shots_ot = shots_ot_m,
       goals = goals_m, assists = assists_m)
}

# ── Helper: calibration bin table ─────────────────────────────────────────────
# Shows whether P=40% rows actually happen 40% of the time

calibration_table <- function(actual, p_model, n_bins = 10) {
  tibble(actual = actual, p_model = p_model) %>%
    filter(!is.na(p_model), !is.na(actual)) %>%
    mutate(
      bin = cut(p_model,
                breaks         = seq(0, 1, length.out = n_bins + 1),
                include.lowest = TRUE,
                labels         = FALSE)
    ) %>%
    group_by(bin) %>%
    summarise(
      n           = n(),
      mean_pred   = round(mean(p_model),  3),
      actual_rate = round(mean(actual),   3),
      diff        = round(actual_rate - mean_pred, 3),
      .groups     = "drop"
    ) %>%
    mutate(calibrated = ifelse(abs(diff) <= 0.05, "OK", "OFF"))
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1: CALIBRATION BACKTEST
# ═══════════════════════════════════════════════════════════════════════════════

run_calibration_backtest <- function(train_cutoff = as.Date("2025-01-01"),
                                     test_cutoff  = as.Date("2026-01-01"),
                                     min_minutes  = 10) {

  message("Loading model dataset...")
  df <- readRDS("data/processed/model_dataset.rds") %>%
    filter(minutes_played >= min_minutes)

  # Strict time-based split — no future data leaks into training
  train_df <- df %>% filter(date <  train_cutoff)
  test_df  <- df %>%
    filter(date >= train_cutoff, date < test_cutoff) %>%
    mutate(
      is_home          = as.integer(is_home),
      log_minutes      = log(minutes_played / 90),
      roll_xg_per_shot = pmin(roll_xg_per_shot, 0.5, na.rm = TRUE)
    )

  message("Train rows: ", nrow(train_df),
          " (up to ", format(train_cutoff - 1), ")")
  message("Test rows:  ", nrow(test_df),
          " (", format(train_cutoff), " → ", format(test_cutoff - 1), ")")

  if (nrow(test_df) == 0) stop("No rows for test season '", test_season, "'")

  message("\nRefitting models on train set...")
  models <- refit_models(train_df)

  message("\nGenerating out-of-sample predictions...")

  # Shots
  test_df$pred_shots <- predict(models$shots, newdata = test_df,
                                type = "response", allow.new.levels = TRUE)

  # Shots on target (conditional on shots)
  nd_sot <- test_df %>%
    mutate(shots            = pred_shots,
           shots_off_target = pmax(pred_shots * 0.5, 0))
  test_df$pred_shots_ot <- test_df$pred_shots *
    predict(models$shots_ot, newdata = nd_sot,
            type = "response", allow.new.levels = TRUE)

  # Goals (conditional on shots)
  nd_goals <- test_df %>% mutate(shots = pred_shots)
  test_df$pred_goals <- predict(models$goals, newdata = nd_goals,
                                type = "response", allow.new.levels = TRUE)

  # Assists
  test_df$pred_assists <- predict(models$assists, newdata = test_df,
                                  type = "response", allow.new.levels = TRUE)

  # ── Raw accuracy ──────────────────────────────────────────────────────────────
  raw_acc <- function(actual, predicted, label) {
    tibble(
      metric = label,
      mae    = round(mean(abs(actual - predicted), na.rm = TRUE), 3),
      rmse   = round(sqrt(mean((actual - predicted)^2, na.rm = TRUE)), 3),
      mean_actual = round(mean(actual, na.rm = TRUE), 3),
      mean_pred   = round(mean(predicted, na.rm = TRUE), 3)
    )
  }

  acc <- bind_rows(
    raw_acc(test_df$goals,           test_df$pred_goals,    "goals"),
    raw_acc(test_df$assists,         test_df$pred_assists,  "assists"),
    raw_acc(test_df$shots_on_target, test_df$pred_shots_ot, "shots_ot"),
    raw_acc(test_df$shots,           test_df$pred_shots,    "shots")
  )

  message("\n── Raw accuracy (out-of-sample: ", format(train_cutoff), " → ",
          format(test_cutoff - 1), ") ──────────────────")
  print(acc)

  # ── Brier scores (probabilistic calibration) ─────────────────────────────────
  # Brier = mean squared error of the probability forecast
  # Lower is better; baseline = naive rate * (1 - naive rate)

  brier_row <- function(actual_count, pred_lambda, threshold, label) {
    p_actual <- as.integer(actual_count >= threshold)
    p_model  <- 1 - ppois(threshold - 1L, pmax(pred_lambda, 0))
    bs       <- mean((p_model - p_actual)^2, na.rm = TRUE)
    naive    <- mean(p_actual, na.rm = TRUE)
    tibble(
      outcome    = label,
      brier      = round(bs, 4),
      baseline   = round(naive * (1 - naive), 4),  # random-guess Brier
      skill      = round(1 - bs / (naive * (1 - naive)), 3),  # Brier skill score
      event_rate = round(naive, 3)
    )
  }

  briers <- bind_rows(
    brier_row(test_df$goals,           test_df$pred_goals,    1, "scored a goal (>=1)"),
    brier_row(test_df$assists,         test_df$pred_assists,  1, "recorded an assist (>=1)"),
    brier_row(test_df$shots_on_target, test_df$pred_shots_ot, 2, "2+ shots on target"),
    brier_row(test_df$shots,           test_df$pred_shots,    3, "3+ shots")
  )

  message("\n── Brier scores (skill > 0 = beats naive guess) ────────────────────")
  print(briers)

  # ── Calibration curves ────────────────────────────────────────────────────────
  message("\n── Calibration curve: goals ──────────────────────────────────────────")
  print(calibration_table(
    actual  = as.integer(test_df$goals >= 1),
    p_model = 1 - exp(-pmax(test_df$pred_goals, 0))
  ))

  message("\n── Calibration curve: assists ────────────────────────────────────────")
  print(calibration_table(
    actual  = as.integer(test_df$assists >= 1),
    p_model = 1 - exp(-pmax(test_df$pred_assists, 0))
  ))

  message("\n── Calibration curve: 2+ shots on target ────────────────────────────")
  print(calibration_table(
    actual  = as.integer(test_df$shots_on_target >= 2),
    p_model = 1 - ppois(1L, pmax(test_df$pred_shots_ot, 0))
  ))

  message("\n── Calibration curve: 3+ shots ───────────────────────────────────────")
  print(calibration_table(
    actual  = as.integer(test_df$shots >= 3),
    p_model = 1 - ppois(2L, pmax(test_df$pred_shots, 0))
  ))

  invisible(list(accuracy = acc, brier = briers, test_df = test_df))
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 2: WEEKLY LOGGER + PROJECTION ACCURACY TRACKING
# ═══════════════════════════════════════════════════════════════════════════════

# ── 2a. Log this week's projections ───────────────────────────────────────────
# Call after run_daily_projections() to build your track record

log_projections <- function(proj_df, path = PROJ_LOG_PATH) {

  dir.create(BACKTEST_DIR, showWarnings = FALSE, recursive = TRUE)

  new_rows <- proj_df %>%
    mutate(game_date = as.Date(game_time), logged_at = Sys.time())

  if (file.exists(path)) {
    existing <- read_csv(path, show_col_types = FALSE)
    already  <- new_rows %>%
      semi_join(existing, by = c("player", "team", "game_date"))
    if (nrow(already) > 0)
      message("  Skipping ", nrow(already), " already-logged rows")
    new_rows <- new_rows %>%
      anti_join(existing, by = c("player", "team", "game_date"))
    if (nrow(new_rows) == 0) {
      message("All projections already logged.")
      return(invisible(NULL))
    }
    new_rows <- bind_rows(existing, new_rows)
  }

  write_csv(new_rows, path)
  message("Logged ", nrow(proj_df), " projections → ", path)
  invisible(new_rows)
}

# ── 2b. Log actual outcomes after games finish ────────────────────────────────
# Pulls from ASA API for a given game date

log_outcomes <- function(game_date = Sys.Date() - 1,
                          season    = format(Sys.Date(), "%Y"),
                          path      = OUT_LOG_PATH) {

  dir.create(BACKTEST_DIR, showWarnings = FALSE, recursive = TRUE)
  game_date <- as.Date(game_date)

  message("Pulling ASA player stats for ", game_date, "...")

  # Pull season xgoals (split_by_game gives one row per player per game)
  resp <- GET(paste0(ASA_BASE, "/players/xgoals"),
              query = list(season_name  = season,
                           split_by_game = "true",
                           limit         = 2000))

  if (status_code(resp) != 200)
    stop("ASA API error: ", status_code(resp))

  xg <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE) %>%
    rename_with(~ gsub("\\.", "_", .x))

  # Pull games to get dates
  gresp <- GET(paste0(ASA_BASE, "/games"),
               query = list(season_name = season, limit = 1000))
  games <- fromJSON(content(gresp, "text", encoding = "UTF-8"), flatten = TRUE) %>%
    rename_with(~ gsub("\\.", "_", .x)) %>%
    mutate(date = as.Date(date_time_utc)) %>%
    select(game_id, date)

  xg <- xg %>% left_join(games, by = "game_id") %>%
    filter(date == game_date)

  if (nrow(xg) == 0) {
    message("No player data available for ", game_date,
            " — check if ASA has updated yet (usually lags ~1 day)")
    return(invisible(NULL))
  }

  # Pull player names
  presp <- GET(paste0(ASA_BASE, "/players"), query = list(limit = 5000))
  players <- fromJSON(content(presp, "text", encoding = "UTF-8"), flatten = TRUE) %>%
    rename_with(~ gsub("\\.", "_", .x)) %>%
    select(player_id, player_name) %>%
    distinct(player_id, .keep_all = TRUE)

  outcomes <- xg %>%
    left_join(players, by = "player_id") %>%
    mutate(
      player    = stringi::stri_trans_general(player_name, "Latin-ASCII"),
      game_date = date
    ) %>%
    select(player, game_date,
           actual_goals    = goals,
           actual_assists  = primary_assists,
           actual_shots    = shots,
           actual_shots_ot = shots_on_target,
           actual_minutes  = minutes_played) %>%
    filter(!is.na(actual_minutes), actual_minutes > 0)

  if (file.exists(path)) {
    existing <- read_csv(path, show_col_types = FALSE)
    new_rows <- outcomes %>%
      anti_join(existing, by = c("player", "game_date"))
    if (nrow(new_rows) == 0) {
      message("Outcomes for ", game_date, " already logged.")
      return(invisible(existing))
    }
    outcomes <- bind_rows(existing, new_rows)
  }

  write_csv(outcomes, path)
  message("Logged outcomes: ", nrow(xg), " player-games on ", game_date,
          " → ", path)
  invisible(outcomes)
}

# ── 2c. Projection accuracy tracking ──────────────────────────────────────────
# Joins logged projections + outcomes and reports hit-rate / calibration —
# i.e. when the model said there was a 40% chance of an event, did it happen
# about 40% of the time? This is the ground truth for whether the model's
# probability estimates hold up in the real world, independent of any market.

track_projection_accuracy <- function(prop_filter = NULL,
                                       proj_path   = PROJ_LOG_PATH,
                                       out_path    = OUT_LOG_PATH) {

  if (!file.exists(proj_path)) stop("No projection log at: ", proj_path)
  if (!file.exists(out_path))  stop("No outcomes log at: ",  out_path)

  proj <- read_csv(proj_path, show_col_types = FALSE) %>%
    mutate(game_date = as.Date(game_time))
  outs <- read_csv(out_path,  show_col_types = FALSE) %>%
    mutate(game_date = as.Date(game_date))

  joined <- proj %>%
    inner_join(outs, by = c("player", "game_date"))

  message("Matched ", nrow(joined), " player-games with outcomes (",
          n_distinct(joined$game_date), " game dates)")

  if (nrow(joined) == 0) {
    message("No matched rows — check player name formatting between logs")
    return(invisible(NULL))
  }

  # Build long-format table: one row per player-game-stat, with the model's
  # implied probability of clearing a standard evaluation threshold
  records <- bind_rows(

    joined %>%
      transmute(player, team, game_date, prop = "goals",
                p_model   = 1 - exp(-pmax(goals, 0)),
                actual    = actual_goals, threshold = 1L),

    joined %>%
      transmute(player, team, game_date, prop = "assists",
                p_model   = 1 - exp(-pmax(assists, 0)),
                actual    = actual_assists, threshold = 1L),

    joined %>%
      transmute(player, team, game_date, prop = "shots_ot",
                p_model   = 1 - ppois(1L, pmax(shots_ot, 0)),
                actual    = actual_shots_ot, threshold = 2L),

    joined %>%
      transmute(player, team, game_date, prop = "shots",
                p_model   = 1 - ppois(2L, pmax(shots, 0)),
                actual    = actual_shots, threshold = 3L)
  )

  if (!is.null(prop_filter)) records <- records %>% filter(prop %in% prop_filter)

  records <- records %>% mutate(hit = as.integer(actual >= threshold))

  overall <- records %>%
    summarise(
      n         = n(),
      mean_pred = round(mean(p_model, na.rm = TRUE), 3),
      hit_rate  = round(mean(hit, na.rm = TRUE), 3),
      brier     = round(mean((p_model - hit)^2, na.rm = TRUE), 4)
    )

  by_prop <- records %>%
    group_by(prop) %>%
    summarise(
      n         = n(),
      mean_pred = round(mean(p_model, na.rm = TRUE), 3),
      hit_rate  = round(mean(hit, na.rm = TRUE), 3),
      brier     = round(mean((p_model - hit)^2, na.rm = TRUE), 4),
      .groups   = "drop"
    )

  message("\n── Overall projection accuracy ──────────────────────────────────────")
  print(overall)
  message("\n── By stat ───────────────────────────────────────────────────────────")
  print(by_prop)
  message("\n── Calibration curve (all stats pooled) ─────────────────────────────")
  print(calibration_table(records$hit, records$p_model))

  invisible(list(overall = overall, by_prop = by_prop, records = records))
}

# ── 2d. Backtest status summary ───────────────────────────────────────────────

backtest_summary <- function(proj_path = PROJ_LOG_PATH,
                              out_path  = OUT_LOG_PATH) {
  cat("\n── Projection log ──────────────────────────────────────────────────────\n")
  if (file.exists(proj_path)) {
    p <- read_csv(proj_path, show_col_types = FALSE) %>%
      mutate(game_date = as.Date(game_time))
    cat("  Rows:", nrow(p), "| Game dates:", n_distinct(p$game_date),
        "| Players:", n_distinct(p$player), "\n")
    cat("  Date range:", format(min(p$game_date)), "→", format(max(p$game_date)), "\n")
  } else {
    cat("  No projection log yet — run log_projections(proj) after run_daily_projections()\n")
  }

  cat("\n── Outcomes log ────────────────────────────────────────────────────────\n")
  if (file.exists(out_path)) {
    o <- read_csv(out_path, show_col_types = FALSE)
    cat("  Rows:", nrow(o), "| Game dates:", n_distinct(o$game_date),
        "| Players:", n_distinct(o$player), "\n")
  } else {
    cat("  No outcomes log yet — run log_outcomes(game_date) after games finish\n")
  }

  # If both exist, show match rate
  if (file.exists(proj_path) && file.exists(out_path)) {
    p <- read_csv(proj_path, show_col_types = FALSE) %>%
      mutate(game_date = as.Date(game_time))
    o <- read_csv(out_path, show_col_types = FALSE) %>%
      mutate(game_date = as.Date(game_date))
    matched <- inner_join(p, o, by = c("player", "game_date"))
    cat("\n  Matched for accuracy tracking:", nrow(matched),
        "rows (", round(nrow(matched) / nrow(p) * 100, 1), "% of projections)\n")
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 3: DEV FEATURE COMPARISON
# ═══════════════════════════════════════════════════════════════════════════════
# Trains baseline vs enhanced model on the same split and compares Brier scores.
# New features: roll_xpass_diff, roll_receiving_g_plus, roll_passing_g_plus
# Promotes to production only if Brier is same or better on ALL four stats.

compare_model_features <- function(train_cutoff = as.Date("2025-01-01"),
                                    test_cutoff  = as.Date("2026-01-01"),
                                    min_minutes  = 10) {

  message("Loading model dataset...")
  df <- readRDS("data/processed/model_dataset.rds") %>%
    filter(minutes_played >= min_minutes) %>%
    mutate(
      is_home          = as.integer(is_home),
      log_minutes      = log(minutes_played / 90),
      roll_xg_per_shot = pmin(roll_xg_per_shot, 0.5, na.rm = TRUE)
    )

  train_df <- df %>% filter(date <  train_cutoff)
  test_df  <- df %>% filter(date >= train_cutoff, date < test_cutoff)

  message("Train: ", nrow(train_df), " rows (up to ", format(train_cutoff - 1), ")")
  message("Test:  ", nrow(test_df),  " rows (", format(train_cutoff), " → ", format(test_cutoff - 1), ")")

  # ── Check whether dev features are available in dataset ────────────────────
  dev_cols <- c("roll_xpass_diff", "roll_receiving_g_plus", "roll_passing_g_plus")
  missing  <- dev_cols[!dev_cols %in% names(df)]
  if (length(missing) > 0) {
    stop("Dev features not in model_dataset: ", paste(missing, collapse = ", "),
         "\nRun: source('R/01_pull_asa.R'); pull_all_asa() then rebuild model_dataset.")
  }

  # ── Helper: compute Brier scores from model list ───────────────────────────
  brier_suite <- function(models, td) {
    td$pred_shots   <- predict(models$shots,    newdata = td, type = "response", allow.new.levels = TRUE)
    nd_sot <- td %>% mutate(shots = pred_shots, shots_off_target = pmax(pred_shots * 0.5, 0))
    td$pred_shots_ot <- td$pred_shots *
      predict(models$shots_ot, newdata = nd_sot, type = "response", allow.new.levels = TRUE)
    nd_g <- td %>% mutate(shots = pred_shots)
    td$pred_goals   <- predict(models$goals,   newdata = nd_g, type = "response", allow.new.levels = TRUE)
    td$pred_assists <- predict(models$assists, newdata = td,   type = "response", allow.new.levels = TRUE)

    brier_row <- function(actual_count, pred_lambda, threshold, label) {
      p_actual <- as.integer(actual_count >= threshold)
      p_model  <- 1 - ppois(threshold - 1L, pmax(pred_lambda, 0))
      bs       <- mean((p_model - p_actual)^2, na.rm = TRUE)
      naive    <- mean(p_actual, na.rm = TRUE)
      tibble(
        outcome    = label,
        brier      = round(bs, 4),
        skill      = round(1 - bs / (naive * (1 - naive)), 3),
        event_rate = round(naive, 3)
      )
    }

    bind_rows(
      brier_row(td$goals,           td$pred_goals,    1, "goals (>=1)"),
      brier_row(td$assists,         td$pred_assists,  1, "assists (>=1)"),
      brier_row(td$shots_on_target, td$pred_shots_ot, 2, "2+ shots on target"),
      brier_row(td$shots,           td$pred_shots,    3, "3+ shots")
    )
  }

  # ── Baseline: current production features ─────────────────────────────────
  message("\n── Fitting BASELINE models ───────────────────────────────────────────")
  baseline_models <- refit_models(train_df)
  baseline_brier  <- brier_suite(baseline_models, test_df)

  # ── Enhanced: + xpass_diff, receiving_g+, passing_g+ ──────────────────────
  message("\n── Fitting ENHANCED models (+ xpass_diff + goals_added) ──────────────")

  # Cap new features to finite range — per-90 on short appearances can produce Inf
  clean_dev_features <- function(d) {
    d %>% mutate(
      roll_xpass_diff       = pmin(pmax(coalesce(roll_xpass_diff,       0), -50), 50),
      roll_receiving_g_plus = pmin(pmax(coalesce(roll_receiving_g_plus, 0),  -3),  3),
      roll_passing_g_plus   = pmin(pmax(coalesce(roll_passing_g_plus,   0),  -3),  3)
    )
  }

  train_enh <- clean_dev_features(train_df)
  test_enh  <- clean_dev_features(test_df)

  # Shots: + roll_receiving_g_plus (better positioning in dangerous areas)
  message("  Fitting enhanced shots model...")
  shots_enh <- glmmTMB(
    shots ~
      offset(log_minutes) +
      is_home + days_rest +
      roll_shots_per90 + roll_xg_per_shot +
      opp_shots_conceded_L10 + opp_xg_conceded_L10 +
      roll_receiving_g_plus +
      (1 | player_id),
    family = nbinom2,
    data   = train_enh %>% filter(!is.na(opp_shots_conceded_L10))
  )

  # Shots on target: unchanged (receiving g+ less relevant to accuracy)
  message("  Fitting enhanced shots-on-target model...")
  shots_ot_enh <- glmmTMB(
    cbind(shots_on_target, pmax(shots - shots_on_target, 0)) ~
      roll_shots_ot_per90 / roll_shots_per90 +
      opp_shots_ot_conceded_L10 + is_home +
      (1 | player_id),
    family = binomial,
    data   = train_enh %>% filter(shots > 0)
  )

  # Goals: receiving_g+ removed — confounds shot quality (roll_xg_per_shot already captures that)
  message("  Fitting enhanced goals model...")
  goals_enh <- glmmTMB(
    goals ~
      offset(log(shots + 0.5)) +
      roll_xg_per_shot + opp_xg_conceded_L10 + is_home +
      (1 | player_id),
    family = poisson,
    data   = train_enh %>% filter(shots > 0)
  )

  # Assists: roll_xpass_diff only — passing_g+ caused convergence issues, xpass_diff is simpler signal
  message("  Fitting enhanced assists model...")
  assists_enh <- glmmTMB(
    assists ~
      offset(log_minutes) +
      roll_xassists_per90 + roll_key_passes_per90 +
      opp_xg_conceded_L10 + is_home +
      roll_xpass_diff +
      (1 | player_id),
    ziformula = ~ roll_xassists_per90,
    family    = poisson,
    data      = train_enh
  )

  enhanced_models <- list(shots    = shots_enh,
                          shots_ot = shots_ot_enh,
                          goals    = goals_enh,
                          assists  = assists_enh)
  enhanced_brier <- brier_suite(enhanced_models, test_enh)

  # ── Side-by-side comparison ────────────────────────────────────────────────
  comparison <- baseline_brier %>%
    rename(base_brier = brier, base_skill = skill) %>%
    left_join(
      enhanced_brier %>% rename(enh_brier = brier, enh_skill = skill),
      by = c("outcome", "event_rate")
    ) %>%
    mutate(
      brier_delta = round(enh_brier - base_brier, 4),
      skill_delta = round(enh_skill - base_skill, 3),
      verdict     = case_when(
        brier_delta < -0.0005 ~ "BETTER",
        brier_delta >  0.0005 ~ "WORSE",
        TRUE                  ~ "NO CHANGE"
      )
    )

  message("\n══════════════════════════════════════════════════════════════════════")
  message("  FEATURE COMPARISON: Baseline vs Enhanced (+ xPass + Goals Added)")
  message("  Train: up to ", format(train_cutoff - 1),
          " | Test: ", format(train_cutoff), " → ", format(test_cutoff - 1))
  message("══════════════════════════════════════════════════════════════════════\n")
  print(comparison %>% select(outcome, event_rate,
                               base_skill, enh_skill, skill_delta,
                               brier_delta, verdict))

  promote <- all(comparison$verdict != "WORSE")
  message("\n  Promote to production? ",
          if (promote) "YES — all stats same or better" else
            paste0("NO — ", sum(comparison$verdict == "WORSE"),
                   " stat(s) got worse"))

  if (promote) {
    message("\n  Next steps:")
    message("  1. Update R/04_model.R: add new terms to shots/goals/assists formulas")
    message("  2. Rebuild model: source('R/04_model.R'); models <- fit_all_models(model_data)")
    message("  3. Update R/09_documentation.R to reflect new features")
  }

  invisible(list(
    comparison       = comparison,
    baseline_models  = baseline_models,
    enhanced_models  = enhanced_models,
    promote          = promote
  ))
}

# ── Usage ─────────────────────────────────────────────────────────────────────
#
# source("R/00_setup.R")
# source("R/08_backtest.R")
#
# ── PART 1: Run calibration backtest immediately ──────────────────────────────
# cal <- run_calibration_backtest()
#
# ── PART 2: Start accumulating weekly data ────────────────────────────────────
#
# Step 1 — after running projections each week:
#   source("R/05_projections.R")
#   proj <- run_daily_projections()
#   log_projections(proj)
#
# Step 2 — after games finish (usually next day):
#   log_outcomes(game_date = as.Date("2026-03-15"))
#
# Step 3 — once you have a few weeks:
#   track_projection_accuracy()
#
# Step 4 — check status anytime:
#   backtest_summary()
#
# ── PART 3: Dev feature comparison ────────────────────────────────────────────
#
# Step 1 — pull fresh data with goals_added:
#   source("R/01_pull_asa.R")
#   pull_all_asa(seasons = c("2023","2024","2025","2026"))
#
# Step 2 — rebuild feature dataset:
#   source("R/03_feature_engineering.R")
#   model_data <- build_model_dataset()
#
# Step 3 — run dev comparison:
#   source("R/08_backtest.R")
#   result <- compare_model_features()
#
# Step 4 — if result$promote is TRUE, update 04_model.R with new formula terms
