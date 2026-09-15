# 02_pull_fbref.R
# Pull MLS player stats from FBref via worldfootballR
# Uses load_fb_advanced_match_stats (cached GitHub data, no scraping/Chrome needed)
#
# Only "summary" and "passing" are cached for MLS (USA / M / 1st tier)
# "summary" covers: goals, assists, shots, xG, minutes, position, age
# "passing" covers: completions, attempts, progressive passes, key passes, xA
#
# Note: cached data lags ~season behind; 2026 will be empty until cache updates

library(worldfootballR)
library(tidyverse)

# ── Helper: retry wrapper ──────────────────────────────────────────────────────

fbref_pull <- function(fn, ..., retries = 3, wait = 30) {
  for (i in seq_len(retries)) {
    tryCatch({
      result <- fn(...)
      return(result)
    }, error = function(e) {
      message("Attempt ", i, " failed: ", conditionMessage(e))
      if (i < retries) {
        message("Waiting ", wait, "s before retry...")
        Sys.sleep(wait)
      }
    })
  }
  stop("All retries exhausted")
}

# ── 1. Summary stats (goals, assists, shots, xG, minutes, position, age) ──────

pull_fbref_summary <- function(season_end_year = 2025) {
  message("Pulling FBref summary stats for ", season_end_year, "...")

  df <- load_fb_advanced_match_stats(
    country         = "USA",
    gender          = "M",
    tier            = "1st",
    stat_type       = "summary",
    team_or_player  = "player",
    season_end_year = season_end_year
  )

  if (nrow(df) == 0) {
    message("  No data available for ", season_end_year, " (cache not yet updated)")
    return(NULL)
  }

  df %>%
    mutate(season = season_end_year) %>%
    rename_with(~ gsub(" ", "_", .x)) %>%
    rename_with(tolower)
}

# ── 2. Passing stats (completions, progressive passes, key passes, xA) ────────

pull_fbref_passing <- function(season_end_year = 2025) {
  message("Pulling FBref passing stats for ", season_end_year, "...")

  df <- load_fb_advanced_match_stats(
    country         = "USA",
    gender          = "M",
    tier            = "1st",
    stat_type       = "passing",
    team_or_player  = "player",
    season_end_year = season_end_year
  )

  if (nrow(df) == 0) {
    message("  No data available for ", season_end_year, " (cache not yet updated)")
    return(NULL)
  }

  df %>%
    mutate(season = season_end_year) %>%
    rename_with(~ gsub(" ", "_", .x)) %>%
    rename_with(tolower)
}

# ── 3. Match-level logs (game-by-game for rolling features) ───────────────────

pull_fbref_match_logs <- function(player_urls, stat_type = "shooting") {
  message("Pulling match logs for ", length(player_urls), " players (", stat_type, ")...")

  logs <- fb_player_match_logs(
    player_url      = player_urls,
    stat_type       = stat_type,
    season_end_year = 2025
  )

  logs %>%
    rename_with(~ gsub(" ", "_", .x)) %>%
    rename_with(tolower)
}

# ── Run all season-level pulls ─────────────────────────────────────────────────

pull_all_fbref <- function(seasons = c(2023, 2024, 2025, 2026), between_wait = 5) {

  pull_with_gap <- function(fn, seasons, wait) {
    results <- list()
    for (yr in seasons) {
      results[[as.character(yr)]] <- fn(yr)
      if (yr != tail(seasons, 1)) Sys.sleep(wait)
    }
    bind_rows(results)
  }

  summary_stats <- pull_with_gap(pull_fbref_summary, seasons, between_wait)
  passing       <- pull_with_gap(pull_fbref_passing, seasons, between_wait)

  saveRDS(summary_stats, "data/raw/fbref_summary.rds")
  saveRDS(passing,       "data/raw/fbref_passing.rds")

  message("All FBref data saved to data/raw/")

  list(
    summary = summary_stats,
    passing = passing
  )
}

# To run (from the project root):
# source("R/00_setup.R")
# fbref_data <- pull_all_fbref()
