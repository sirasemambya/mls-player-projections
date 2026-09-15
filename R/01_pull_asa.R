# 01_pull_asa.R
# Pull player-level xG, xA, goals added, and minutes from American Soccer Analysis API

library(httr)
library(jsonlite)
library(tidyverse)

ASA_BASE <- "https://app.americansocceranalysis.com/api/v1/mls"

# ── Paginated GET — ASA limits to 1000 rows per request ───────────────────────

asa_get_all <- function(endpoint, params = list(), page_size = 1000) {
  all_rows <- list()
  offset   <- 0

  repeat {
    p <- c(params, list(limit = page_size, offset = offset))
    url  <- paste0(ASA_BASE, endpoint)
    resp <- GET(url, query = p)

    if (status_code(resp) != 200) {
      stop(paste("ASA API error:", status_code(resp), "on", endpoint))
    }

    batch <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)

    if (is.null(batch) || nrow(batch) == 0) break

    all_rows[[length(all_rows) + 1]] <- batch
    message("  fetched ", offset + nrow(batch), " rows...")

    if (nrow(batch) < page_size) break
    offset <- offset + page_size
  }

  bind_rows(all_rows)
}

# ── 1. Players lookup (id → name, position) ───────────────────────────────────

pull_players <- function() {
  message("Pulling players lookup...")
  df <- asa_get_all("/players")
  df %>% rename_with(~ gsub("\\.", "_", .x)) %>% mutate(pulled_at = Sys.time())
}

# ── 2. Player xGoals per game ─────────────────────────────────────────────────

pull_xgoals <- function(season = NULL) {
  message("Pulling xGoals", if (!is.null(season)) paste0(" (", season, ")") else "", "...")
  params <- list(split_by_game = "true")
  if (!is.null(season)) params$season_name <- season

  df <- asa_get_all("/players/xgoals", params)
  df %>% rename_with(~ gsub("\\.", "_", .x)) %>% mutate(pulled_at = Sys.time())
}

# ── 3. Player xPass per game ──────────────────────────────────────────────────

pull_xpass <- function(season = NULL) {
  message("Pulling xPass", if (!is.null(season)) paste0(" (", season, ")") else "", "...")
  params <- list(split_by_game = "true")
  if (!is.null(season)) params$season_name <- season

  df <- asa_get_all("/players/xpass", params)
  df %>% rename_with(~ gsub("\\.", "_", .x)) %>% mutate(pulled_at = Sys.time())
}

# ── 4. Team xGoals per game (opponent defensive profile) ──────────────────────

pull_team_xgoals <- function(season = NULL) {
  message("Pulling team xGoals", if (!is.null(season)) paste0(" (", season, ")") else "", "...")
  params <- list(split_by_game = "true")
  if (!is.null(season)) params$season_name <- season

  df <- asa_get_all("/teams/xgoals", params)
  df %>% rename_with(~ gsub("\\.", "_", .x)) %>% mutate(pulled_at = Sys.time())
}

# ── 5. Games (dates, home/away, season) ───────────────────────────────────────

pull_games <- function(season = NULL) {
  message("Pulling games", if (!is.null(season)) paste0(" (", season, ")") else "", "...")
  params <- list()
  if (!is.null(season)) params$season_name <- season

  df <- asa_get_all("/games", params)
  df %>%
    rename_with(~ gsub("\\.", "_", .x)) %>%
    mutate(
      date       = as.Date(date_time_utc),
      pulled_at  = Sys.time()
    )
}

# ── 6. Goals Added per game ───────────────────────────────────────────────────
# Components used: passing (assists model), receiving (shots/goals model)
# shooting/dribbling/interrupting not used — correlated with existing features

pull_goals_added <- function(season = NULL) {
  message("Pulling Goals Added", if (!is.null(season)) paste0(" (", season, ")") else "", "...")
  params <- list(split_by_game = "true")
  if (!is.null(season)) params$season_name <- season

  df <- asa_get_all("/players/goals-added", params)

  if (nrow(df) == 0) {
    message("  No Goals Added data returned for this season")
    return(NULL)
  }

  df <- df %>% rename_with(~ gsub("\\.", "_", .x)) %>% mutate(pulled_at = Sys.time())

  # ASA returns action-type components inside a nested `data` list-column.
  # Unnest and pivot so each action type becomes its own column prefix.
  if ("data" %in% names(df) && is.list(df$data)) {

    unnested <- df %>%
      select(player_id, game_id, data) %>%
      tidyr::unnest(data) %>%
      rename_with(~ gsub("\\.", "_", .x))

    # Show what came back so we can diagnose naming issues
    message("  Goals Added nested columns: ", paste(names(unnested), collapse = ", "))

    # Normalise: ASA may use action_type or type
    if ("action_type" %in% names(unnested)) {
      unnested <- unnested %>% mutate(action_type = tolower(action_type))
    } else if ("type" %in% names(unnested)) {
      unnested <- unnested %>% rename(action_type = type) %>%
        mutate(action_type = tolower(action_type))
    }

    # Normalise value column name to goals_added_for
    ga_col <- intersect(c("goals_added_for", "goals_added_raw", "num_actions_for"), names(unnested))[1]
    if (!is.na(ga_col)) {
      unnested <- unnested %>% rename(goals_added_for = !!ga_col)
    }

    df <- unnested %>%
      filter(action_type %in% c("passing", "receiving")) %>%
      select(player_id, game_id, action_type, goals_added_for) %>%
      tidyr::pivot_wider(
        id_cols     = c(player_id, game_id),
        names_from  = action_type,
        values_from = goals_added_for,
        names_glue  = "{action_type}_goals_added_for"
      )

    message("  Goals Added columns after pivot: ",
            paste(names(df), collapse = ", "))
  }

  df
}

# ── Run all pulls and save ─────────────────────────────────────────────────────

pull_all_asa <- function(seasons = c("2023", "2024", "2025", "2026")) {

  players     <- pull_players()

  xgoals      <- map_dfr(seasons, pull_xgoals)
  xpass       <- map_dfr(seasons, pull_xpass)
  goals_added <- map_dfr(seasons, ~ pull_goals_added(.x) %||% tibble())
  team_xg     <- map_dfr(seasons, pull_team_xgoals)
  games       <- map_dfr(seasons, pull_games)

  # Attach player names + broad position to xgoals
  player_lookup <- players %>%
    select(player_id, player_name, birth_date, nationality,
           primary_general_position, primary_broad_position) %>%
    distinct(player_id, .keep_all = TRUE)

  xgoals      <- xgoals      %>% left_join(player_lookup, by = "player_id")
  xpass       <- xpass       %>% left_join(player_lookup, by = "player_id")
  goals_added <- goals_added %>% left_join(player_lookup, by = "player_id")

  # Attach date + season to player rows via game join
  game_dates <- games %>% select(game_id, date, season_name, home_team_id, away_team_id)

  xgoals      <- xgoals      %>% left_join(game_dates, by = "game_id")
  xpass       <- xpass       %>% left_join(game_dates, by = "game_id")
  goals_added <- goals_added %>% left_join(game_dates, by = "game_id")

  saveRDS(players,     "data/raw/asa_players.rds")
  saveRDS(xgoals,      "data/raw/asa_xgoals.rds")
  saveRDS(xpass,       "data/raw/asa_xpass.rds")
  saveRDS(goals_added, "data/raw/asa_goals_added.rds")
  saveRDS(team_xg,     "data/raw/asa_team_xgoals.rds")
  saveRDS(games,       "data/raw/asa_games.rds")

  message("\nAll ASA data saved to data/raw/")
  message("  xgoals:      ", nrow(xgoals),      " rows")
  message("  xpass:       ", nrow(xpass),        " rows")
  message("  goals_added: ", nrow(goals_added),  " rows")
  message("  team_xg:     ", nrow(team_xg),      " rows")
  message("  games:       ", nrow(games),         " rows")

  list(players = players, xgoals = xgoals, xpass = xpass,
       goals_added = goals_added, team_xg = team_xg, games = games)
}

# To run (from the project root):
# source("R/00_setup.R")
# asa_data <- pull_all_asa(seasons = c("2023", "2024", "2025"))
