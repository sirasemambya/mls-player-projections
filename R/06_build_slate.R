# 06_build_slate.R
# Auto-builds the slate CSV for any upcoming game week from ESPN schedule.
#
# Workflow:
#   1. Run build_slate()
#   2. CSV opens automatically — type the MINS column only
#   3. Run run_daily_projections() from 05_projections.R
#
# Mins guide:
#   90 = locked starter (max)  60 = rotation starter, likely subbed
#   80 = starter, sub risk     40 = super-sub
#   20 = bench minutes         delete row = DNP
#
# Team minutes must sum to 990 (11 × 90). build_slate() scales defaults
# automatically. After manual edits, run validate_slate_minutes() to recheck.

library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(openxlsx)

ASA_BASE       <- "https://app.americansocceranalysis.com/api/v1/mls"
ESPN_BASE      <- "https://site.api.espn.com/apis/site/v2/sports/soccer/usa.1/scoreboard"
CONCACAF_BASE  <- "https://site.api.espn.com/apis/site/v2/sports/soccer/concacaf.champions/scoreboard"

# ── ESPN name → ASA name crosswalk (only outliers) ────────────────────────────

ESPN_TO_ASA <- c(
  "LAFC"             = "Los Angeles FC",
  "Red Bull New York" = "New York Red Bulls",
  "Portland Timbers"  = "Portland Timbers FC",
  "St. Louis CITY SC" = "St. Louis City SC",
  "Vancouver Whitecaps" = "Vancouver Whitecaps FC"
)

normalize_name <- function(name) {
  ifelse(name %in% names(ESPN_TO_ASA), ESPN_TO_ASA[name], name)
}

# ── Pull upcoming MLS games from ESPN ─────────────────────────────────────────

get_espn_schedule <- function(from_date = Sys.Date(),
                              to_date   = Sys.Date() + 7) {

  date_str <- paste0(
    format(from_date, "%Y%m%d"), "-", format(to_date, "%Y%m%d")
  )

  resp <- GET(ESPN_BASE, query = list(dates = date_str))

  if (status_code(resp) != 200) stop("ESPN API error: ", status_code(resp))

  d      <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
  events <- d$events

  if (is.null(events) || nrow(events) == 0) {
    message("No MLS games found between ", from_date, " and ", to_date)
    return(NULL)
  }

  games <- map_dfr(seq_len(nrow(events)), function(i) {
    comps <- events$competitions[[i]]$competitors[[1]]
    home  <- comps %>% filter(homeAway == "home") %>%
               select(espn_name = team.displayName)
    away  <- comps %>% filter(homeAway == "away") %>%
               select(espn_name = team.displayName)

    tibble(
      game_time       = lubridate::ymd_hm(sub("Z$", "", events$date[i]), tz = "UTC"),
      game_date       = as.Date(game_time),
      home_espn_name  = home$espn_name,
      away_espn_name  = away$espn_name,
      status          = events$status.type.description[i]
    )
  })

  games %>%
    filter(status == "Scheduled") %>%
    mutate(
      home_asa_name = normalize_name(home_espn_name),
      away_asa_name = normalize_name(away_espn_name)
    )
}

# ── CONCACAF midweek check ────────────────────────────────────────────────────
# Flags teams with a CONCACAF game within the next 5 days of their MLS match.
# concacaf_days = days until their next CONCACAF game (NA = none in window)

get_concacaf_games <- function(from_date = Sys.Date(), days_ahead = 5) {
  to_date  <- from_date + days_ahead
  date_str <- paste0(format(from_date, "%Y%m%d"), "-", format(to_date, "%Y%m%d"))
  resp <- tryCatch(
    GET(CONCACAF_BASE, query = list(dates = date_str)),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) {
    message("  CONCACAF schedule unavailable — skipping check")
    return(tibble())
  }
  d      <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE), error = function(e) NULL)
  events <- d$events
  if (is.null(events) || nrow(events) == 0) return(tibble())

  map_dfr(seq_len(nrow(events)), function(i) {
    comps     <- events$competitions[[i]]$competitors[[1]]
    game_date <- as.Date(substr(events$date[i], 1, 10))
    tibble(team_espn = comps$team.displayName, concacaf_date = game_date)
  })
}

# ── ASA team lookup ───────────────────────────────────────────────────────────

get_teams <- function() {
  resp <- GET(paste0(ASA_BASE, "/teams"), query = list(limit = 50))
  fromJSON(content(resp, "text", encoding = "UTF-8")) %>%
    select(team_id, team_name, team_abbreviation)
}

# ── Pull active players for a team ────────────────────────────────────────────

get_team_players <- function(team_id, model_data, teams, n_players = 18) {

  team_name <- teams %>% filter(team_id == !!team_id) %>% pull(team_name)

  model_data %>%
    filter(team_id == !!team_id) %>%
    arrange(player_id, date) %>%
    group_by(player_id) %>%
    summarise(
      player_name      = last(player_name),
      position_general = last(position_general),
      last_game_date   = max(date),
      last_game_mins   = last(minutes_played),   # default mins = last match
      avg_minutes_L5   = mean(tail(minutes_played, 5), na.rm = TRUE),
      games_L10        = n(),
      .groups          = "drop"
    ) %>%
    filter(last_game_date >= max(last_game_date) - 30) %>%
    arrange(desc(avg_minutes_L5)) %>%
    slice_head(n = n_players) %>%
    mutate(team_id = !!team_id, team_name = team_name)
}

# ── Master slate builder ───────────────────────────────────────────────────────

build_slate <- function(from_date   = Sys.Date(),
                        to_date     = Sys.Date() + 7,
                        output_path = "data/slate_today.csv",
                        open_after  = TRUE,
                        concacaf_window = 5) {

  message("Fetching this week's MLS schedule from ESPN...")
  schedule <- get_espn_schedule(from_date, to_date)
  if (is.null(schedule)) return(invisible(NULL))

  message("Found ", nrow(schedule), " upcoming games:")
  schedule %>%
    mutate(matchup = paste0(home_espn_name, " vs ", away_espn_name)) %>%
    select(game_date, matchup) %>%
    { message(paste(capture.output(print(., n = Inf)), collapse = "\n")) }

  message("\nLoading model data and team lookup...")
  model_data <- readRDS("data/processed/model_dataset.rds")
  teams      <- get_teams()

  # Join ASA team IDs onto schedule
  team_lookup <- teams %>% select(team_id, team_name)

  schedule <- schedule %>%
    left_join(team_lookup %>% rename(home_team_id = team_id),
              by = c("home_asa_name" = "team_name")) %>%
    left_join(team_lookup %>% rename(away_team_id = team_id),
              by = c("away_asa_name" = "team_name"))

  # Warn on unmatched teams
  unmatched <- schedule %>%
    filter(is.na(home_team_id) | is.na(away_team_id)) %>%
    select(home_espn_name, away_espn_name, home_team_id, away_team_id)

  if (nrow(unmatched) > 0) {
    message("\nWARNING — could not match these teams to ASA IDs:")
    print(unmatched)
  }

  schedule <- schedule %>% filter(!is.na(home_team_id), !is.na(away_team_id))

  message("\nBuilding player rows...")
  all_rows <- list()

  for (i in seq_len(nrow(schedule))) {
    g <- schedule[i, ]

    home_p <- get_team_players(g$home_team_id, model_data, teams) %>%
      mutate(opp_team_id = g$away_team_id, opp_name = g$away_asa_name,
             is_home = TRUE,  game_date = g$game_date, game_time = g$game_time)

    away_p <- get_team_players(g$away_team_id, model_data, teams) %>%
      mutate(opp_team_id = g$home_team_id, opp_name = g$home_asa_name,
             is_home = FALSE, game_date = g$game_date, game_time = g$game_time)

    all_rows[[i]] <- bind_rows(home_p, away_p)
  }

  slate <- bind_rows(all_rows) %>%
    transmute(
      player_id,
      player_name,
      team        = team_name,
      opp         = opp_name,
      game_time,
      game_date,
      position    = position_general,
      mins        = pmin(as.integer(round(last_game_mins)), 90L),
      is_home,
      last_game_date,
      avg_mins_L5 = round(avg_minutes_L5, 1)
    ) %>%
    arrange(game_time, team, desc(avg_mins_L5))

  # ── CONCACAF midweek check ──────────────────────────────────────────────────
  message("\nChecking CONCACAF schedule (next ", concacaf_window, " days)...")
  concacaf_raw <- get_concacaf_games(from_date = min(slate$game_date),
                                     days_ahead = concacaf_window)
  if (nrow(concacaf_raw) > 0) {
    concacaf_lookup <- concacaf_raw %>%
      mutate(asa_name = normalize_name(team_espn)) %>%
      group_by(asa_name) %>%
      summarise(concacaf_date = min(concacaf_date), .groups = "drop")

    slate <- slate %>%
      left_join(concacaf_lookup, by = c("team" = "asa_name")) %>%
      mutate(
        concacaf_days = as.integer(concacaf_date - game_date),
        concacaf_days = if_else(!is.na(concacaf_days) & concacaf_days >= 0 &
                                  concacaf_days <= concacaf_window,
                                concacaf_days, NA_integer_)
      ) %>%
      select(-concacaf_date)

    flagged <- slate %>% filter(!is.na(concacaf_days)) %>%
      distinct(team, concacaf_days) %>% arrange(concacaf_days)
    if (nrow(flagged) > 0) {
      message("  ⚠ CONCACAF within ", concacaf_window, " days:")
      for (i in seq_len(nrow(flagged)))
        message("    ", flagged$team[i], " — ", flagged$concacaf_days[i], " day(s) after this match")
    } else {
      message("  No CONCACAF conflicts found")
    }
  } else {
    slate <- slate %>% mutate(concacaf_days = NA_integer_)
    message("  No CONCACAF games found in window")
  }

  # ── Write CSV (used by all downstream functions) ─────────────────────────────
  write_csv(slate, output_path)

  # ── Write Excel viewer (frozen name col, ET time, 12-hour format) ────────────
  xlsx_path <- sub("\\.csv$", ".xlsx", output_path)

  slate_xl <- slate %>%
    mutate(
      game_time = format(with_tz(game_time, "America/New_York"), "%m/%d %I:%M %p")
    )

  wb  <- createWorkbook()
  addWorksheet(wb, "Slate")
  writeData(wb, "Slate", slate_xl)
  freezePane(wb, "Slate", firstRow = TRUE, firstActiveCol = 3)  # freeze player_id + player_name
  setColWidths(wb, "Slate", cols = 1:ncol(slate_xl), widths = "auto")
  saveWorkbook(wb, xlsx_path, overwrite = TRUE)

  message("\n── Slate ready: ", output_path, " ──")
  message("Total players: ", nrow(slate))
  message("Games:         ", nrow(schedule))
  message("\nNext:")
  message("  1. Open slate_today.csv")
  message("  2. 'mins' is pre-filled with each player's last game minutes")
  message("  3. Only change mins if you have intel (injury / rotation / suspension)")
  message("  4. Delete rows for confirmed DNPs")
  message("  5. source('R/05_projections.R'); run_daily_projections()")

  if (open_after) system(paste("open", shQuote(xlsx_path)))

  invisible(slate)
}

# ── Minutes constraint validator ──────────────────────────────────────────────
#
# In a soccer game, 11 players are on the field for 90 minutes.
# Total available team minutes = 11 × 90 = 990.
# Every player-minute must be accounted for — the slate mins for each team
# must sum exactly to 990, the same way NBA lineups must exhaust 240 minutes.
#
# Usage (after manually editing slate_today.csv):
#   source("R/06_build_slate.R")
#   validate_slate_minutes("data/slate_today.csv")

TEAM_MINUTES <- 11 * 90  # 990

validate_slate_minutes <- function(path = "data/slate_today.csv") {
  slate <- read_csv(path, show_col_types = FALSE)

  team_totals <- slate %>%
    group_by(game_date, team) %>%
    summarise(total_mins = sum(mins, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      diff   = total_mins - TEAM_MINUTES,
      status = case_when(
        diff == 0 ~ "OK",
        diff >  0 ~ paste0("OVER by ", diff),
        diff <  0 ~ paste0("SHORT by ", abs(diff))
      )
    )

  ok    <- team_totals %>% filter(status == "OK")
  bad   <- team_totals %>% filter(status != "OK")

  if (nrow(bad) == 0) {
    message("All teams balance to ", TEAM_MINUTES, " minutes. Good to go.")
  } else {
    message("\nMINUTES CONSTRAINT VIOLATIONS (target = ", TEAM_MINUTES, " per team):\n")
    bad %>%
      arrange(game_date, team) %>%
      { message(paste(capture.output(print(., n = Inf)), collapse = "\n")) }
    message("\nTeams OK: ", nrow(ok), " / ", nrow(team_totals))
    message("Fix the flagged teams before running projections.\n")
  }

  invisible(team_totals)
}

# ── Apply confirmed lineup to slate ───────────────────────────────────────────
# Call this after lineups are confirmed, before run_daily_projections().
#
# Usage:
#   source("R/06_build_slate.R")
#   apply_lineup(
#     team     = "LA Galaxy",
#     starters = c("Riqui Puig", "Joseph Paintsil", "Gabriel Pec"),   # partial names OK
#     subs     = c("Dejan Joveljic"),    # expected to come on ~60 min
#     dnp      = c("John Nelson")        # confirmed out — row deleted
#   )
#
# Minutes assigned:
#   Starters not in subs list → 90 (starter, no sub risk)
#   Starters in subs list     → 75 (subbed off ~75)
#   subs only (super-sub)     → 30
#   dnp                       → removed from slate

apply_lineup <- function(team,
                         starters    = character(0),
                         subs        = character(0),
                         dnp         = character(0),
                         slate_path  = "data/slate_today.csv") {

  slate <- read_csv(slate_path, show_col_types = FALSE)

  match_player <- function(name, pool) {
    # Case-insensitive partial match so "Puig" matches "Riqui Puig"
    idx <- which(stringi::stri_detect_fixed(
      stringi::stri_trans_tolower(pool),
      stringi::stri_trans_tolower(name)
    ))
    if (length(idx) == 0) {
      warning("No match found for: ", name)
    }
    idx
  }

  team_rows <- which(stringi::stri_detect_fixed(
    stringi::stri_trans_tolower(slate$team),
    stringi::stri_trans_tolower(team)
  ))

  team_names <- slate$player_name[team_rows]

  # DNP — remove rows
  dnp_idx <- unlist(lapply(dnp, match_player, pool = team_names))
  if (length(dnp_idx) > 0) {
    message("Removing DNPs: ", paste(team_names[dnp_idx], collapse = ", "))
    slate <- slate[-team_rows[dnp_idx], ]
    # Refresh after removal
    team_rows <- which(stringi::stri_detect_fixed(
      stringi::stri_trans_tolower(slate$team),
      stringi::stri_trans_tolower(team)
    ))
    team_names <- slate$player_name[team_rows]
  }

  # Starters subbed off ~75
  subbed_off_idx <- unlist(lapply(
    starters[starters %in% subs | sapply(starters, function(s)
      any(stringi::stri_detect_fixed(stringi::stri_trans_tolower(subs),
                                     stringi::stri_trans_tolower(s))))],
    match_player, pool = team_names
  ))

  # Full 90 starters
  full_starter_idx <- unlist(lapply(
    starters[!starters %in% subs & !sapply(starters, function(s)
      any(stringi::stri_detect_fixed(stringi::stri_trans_tolower(subs),
                                     stringi::stri_trans_tolower(s))))],
    match_player, pool = team_names
  ))

  # Super subs (in subs but not starters)
  super_sub_names <- subs[!subs %in% starters & !sapply(subs, function(s)
    any(stringi::stri_detect_fixed(stringi::stri_trans_tolower(starters),
                                   stringi::stri_trans_tolower(s))))]
  super_sub_idx <- unlist(lapply(super_sub_names, match_player, pool = team_names))

  # Apply minutes
  if (length(full_starter_idx)  > 0) slate$mins[team_rows[full_starter_idx]]  <- 90L
  if (length(subbed_off_idx)    > 0) slate$mins[team_rows[subbed_off_idx]]    <- 75L
  if (length(super_sub_idx)     > 0) slate$mins[team_rows[super_sub_idx]]     <- 30L

  write_csv(slate, slate_path)
  message("Lineup applied for ", team, ". Validating minutes...")
  validate_slate_minutes(slate_path)

  invisible(slate)
}

# ── Override position for a player without touching minutes ───────────────────
#
# Usage:
#   set_position("Inter Miami CF", c("Silvetti" = "W"))

set_position <- function(team, positions, slate_path = "data/slate_today.csv") {
  slate <- read_csv(slate_path, show_col_types = FALSE)

  match_player <- function(name, pool) {
    which(stringi::stri_detect_fixed(
      stringi::stri_trans_tolower(pool),
      stringi::stri_trans_tolower(name)
    ))
  }

  team_rows  <- which(stringi::stri_detect_fixed(
    stringi::stri_trans_tolower(slate$team),
    stringi::stri_trans_tolower(team)
  ))
  team_names <- slate$player_name[team_rows]

  for (nm in names(positions)) {
    idx <- match_player(nm, team_names)
    if (length(idx) > 0) {
      slate$position[team_rows[idx[1]]] <- positions[[nm]]
      message("  ", team_names[idx[1]], " → ", positions[[nm]])
    } else {
      warning("Player not found: ", nm)
    }
  }

  write_csv(slate, slate_path)
  message("Position override saved.")
  invisible(slate)
}

# ── Set exact expected minutes per player (slider-style) ──────────────────────
#
# Pass a named vector where names are partial player names (case-insensitive)
# and values are expected minutes. The total across all players on that team
# must still equal 990.
#
# Heuristics:
#   Likely full 90:         90   (solid starter, no sub risk)
#   Starter, sub risk:      75   (starter expected to be relieved ~75)
#   Starter, early sub:     65   (high rotation / tactical sub ~65)
#   Primary bench option:   25   (confirmed sub, comes on ~65)
#   Secondary sub:          15   (comes on ~75)
#   Late sub:               10   (comes on ~80)
#   Garbage time / rare:     5
#   Won't feature:           0   (delete row from slate instead)
#
# Total minutes for 11 starters + subs must sum to 990 (11 × 90).
# If you reduce a starter by 15, add 15 somewhere on the bench.
#
# Usage:
#   source("R/06_build_slate.R")
#   set_lineup_minutes("New York Red Bulls", c(
#     "Che"             = 90, "Donkor"    = 90, "Dos Santos"     = 90,
#     "Forsberg"        = 75, "Hall"      = 70, "Horvath"        = 90,
#     "Marshall-Rutty"  = 80, "Mehmeti"   = 75, "Ruvalcaba"      = 90,
#     "Sofo"            = 80, "Voloder"   = 90,
#     "Choupo-Moting"   = 25, "Berggren"  = 15, "Mina"           = 10,
#     "McCarthy"        =  5
#   ))

set_lineup_minutes <- function(team,
                               minutes,
                               positions  = NULL,   # named vec player = pos, for new signings only
                               slate_path = "data/slate_today.csv") {

  slate      <- read_csv(slate_path, show_col_types = FALSE)
  model_data <- readRDS("data/processed/model_dataset.rds")

  match_player <- function(name, pool) {
    which(stringi::stri_detect_fixed(
      stringi::stri_trans_tolower(pool),
      stringi::stri_trans_tolower(name)
    ))
  }

  refresh_team <- function() {
    rows <- which(stringi::stri_detect_fixed(
      stringi::stri_trans_tolower(slate$team),
      stringi::stri_trans_tolower(team)
    ))
    list(rows = rows, names = slate$player_name[rows])
  }

  tr <- refresh_team()
  if (length(tr$rows) == 0) stop("Team not found in slate: ", team)

  # Game context from first existing team row (opp, game_time, is_home, etc.)
  ctx <- slate[tr$rows[1], ]

  # Zero out ALL players on this team — only named ones will get minutes
  slate$mins[tr$rows] <- 0L

  for (nm in names(minutes)) {
    tr <- refresh_team()
    idx <- match_player(nm, tr$names)

    if (length(idx) > 0) {
      # Found in slate — just set minutes
      slate$mins[tr$rows[idx[1]]] <- as.integer(minutes[[nm]])

    } else {
      # Not in slate — search model_dataset by whole-word match
      md_row <- model_data %>%
        filter(stringi::stri_detect_regex(
          stringi::stri_trans_tolower(player_name),
          paste0("(?i)\\b", stringi::stri_trans_tolower(nm), "\\b")
        )) %>%
        arrange(desc(date)) %>%
        slice_head(n = 1)

      if (nrow(md_row) > 0) {
        message("  Adding '", nm, "' from model data: ", md_row$player_name)
        new_row <- tibble(
          player_id      = md_row$player_id,
          player_name    = md_row$player_name,
          team           = ctx$team,
          opp            = ctx$opp,
          game_time      = ctx$game_time,
          game_date      = ctx$game_date,
          position       = md_row$position_general,
          mins           = as.integer(minutes[[nm]]),
          is_home        = ctx$is_home,
          last_game_date = md_row$date,
          avg_mins_L5    = as.numeric(md_row$minutes_played)
        )
      } else {
        # Truly new signing — use positions hint or default to "M"
        pos <- if (!is.null(positions) && nm %in% names(positions)) positions[[nm]] else "M"
        message("  New player '", nm, "' (no model data) — position: ", pos)
        new_row <- tibble(
          player_id      = paste0("new_", gsub("[^a-z]", "_", tolower(nm))),
          player_name    = nm,
          team           = ctx$team,
          opp            = ctx$opp,
          game_time      = ctx$game_time,
          game_date      = ctx$game_date,
          position       = pos,
          mins           = as.integer(minutes[[nm]]),
          is_home        = ctx$is_home,
          last_game_date = Sys.Date(),
          avg_mins_L5    = as.numeric(minutes[[nm]])
        )
      }

      slate <- bind_rows(slate, new_row)
    }
  }

  # Apply position overrides to existing players (not just new signings)
  if (!is.null(positions)) {
    tr <- refresh_team()
    for (nm in names(positions)) {
      idx <- match_player(nm, tr$names)
      if (length(idx) > 0)
        slate$position[tr$rows[idx[1]]] <- positions[[nm]]
    }
  }

  write_csv(slate, slate_path)

  tr <- refresh_team()
  message("\nMinutes for ", team, ":")
  print(slate[tr$rows, ] %>% select(player_name, position, mins) %>% arrange(desc(mins)), n = Inf)
  message("Total: ", sum(slate$mins[tr$rows], na.rm = TRUE), " / 990")
  validate_slate_minutes(slate_path)

  invisible(slate)
}

# ── Auto-estimate lineup minutes from historical data ─────────────────────────
#
# Paste in the confirmed starters and bench, and auto_lineup() will:
#   1. Look up each player's last n_games minutes in model_dataset
#   2. Use their average as the starter estimate (capped at 90)
#   3. Scale sub minutes proportionally to fill the remaining 990 budget
#   4. Print the ready-to-run set_lineup_minutes() call
#   5. Optionally apply it directly (apply = TRUE)
#
# Usage:
#   source("R/06_build_slate.R")
#   auto_lineup(
#     team     = "Chicago Fire FC",
#     starters = c("Brady", "Barroso", "Elliott", "Mbokazi", "Dean",
#                  "Saletros", "D'Avilla", "Lod", "Zinckernagel", "Bamba", "Cuypers"),
#     subs     = c("Cohen", "Radojevic", "Borso", "Waterman", "Oregel",
#                  "Pineda", "Haile-Selassie", "Dithejane", "Shokalook")
#   )

auto_lineup <- function(team,
                        starters,
                        subs,
                        n_games    = 3,
                        apply      = TRUE,
                        positions  = NULL,
                        slate_path = "data/slate_today.csv") {

  model_data <- readRDS("data/processed/model_dataset.rds")

  # Deduplicate: model_dataset sometimes has duplicate rows per player-game
  player_games <- model_data %>%
    distinct(player_id, player_name, date, minutes_played)

  # Look up a player's last n recent games (optionally only games they played)
  lookup <- function(name_partial, only_played = FALSE) {
    rows <- player_games %>%
      filter(stringi::stri_detect_regex(
        stringi::stri_trans_tolower(player_name),
        paste0("(?i)\\b", stringi::stri_trans_tolower(name_partial), "\\b")
      )) %>%
      arrange(desc(date))
    if (only_played) rows <- filter(rows, minutes_played > 0)
    rows <- slice_head(rows, n = n_games)
    list(
      found   = nrow(rows) > 0,
      name    = if (nrow(rows) > 0) rows$player_name[1] else name_partial,
      history = pmin(rows$minutes_played, 90),
      avg     = if (nrow(rows) > 0) mean(pmin(rows$minutes_played, 90)) else NA_real_
    )
  }

  # ── Starters ────────────────────────────────────────────────────────────────
  s_data <- lapply(starters, lookup)
  s_mins <- sapply(s_data, function(x) if (is.na(x$avg)) 85 else x$avg)
  names(s_mins) <- starters

  # If starters already exceed 990, scale them down
  if (sum(s_mins) > 990) s_mins <- s_mins * 990 / sum(s_mins)

  available <- 990 - sum(s_mins)

  # ── Subs ─────────────────────────────────────────────────────────────────────
  # Use sub's avg minutes *when they played* (not including DNPs)
  b_data <- lapply(subs, lookup, only_played = TRUE)
  b_raw  <- sapply(b_data, function(x) if (is.na(x$avg)) 15 else x$avg)
  names(b_raw) <- subs

  # Scale sub totals to fill exactly the available budget
  if (sum(b_raw) > 0 && available > 0) {
    b_mins <- b_raw * available / sum(b_raw)
  } else {
    b_mins <- rep(0, length(subs))
    names(b_mins) <- subs
  }

  # Round everything and fix any rounding drift so total = 990 exactly
  all_mins  <- round(c(s_mins, b_mins))
  drift     <- 990L - sum(all_mins)
  if (drift != 0) {
    # Apply drift to the player whose rounded value changed the most
    idx <- if (drift > 0) which.max(c(s_mins, b_mins) - all_mins) else
                          which.min(c(s_mins, b_mins) - all_mins)
    all_mins[idx] <- all_mins[idx] + drift
  }
  all_mins <- pmax(all_mins, 1L)   # every listed player gets at least 1 min

  # ── Report ───────────────────────────────────────────────────────────────────
  message("\n── ", team, " lineup minutes ──────────────────────────────────────")
  message(sprintf("  %-28s  %s  →  est", "Player", paste0("Last ", n_games, " games")))
  message("  STARTERS:")
  for (i in seq_along(starters)) {
    hist_str <- if (length(s_data[[i]]$history) > 0)
      paste(s_data[[i]]$history, collapse = " / ") else "no data"
    message(sprintf("  %-28s  %-18s  %d min",
                    starters[i], hist_str, all_mins[i]))
  }
  message("  SUBS:")
  for (i in seq_along(subs)) {
    hist_str <- if (length(b_data[[i]]$history) > 0)
      paste(b_data[[i]]$history, collapse = " / ") else "no data"
    message(sprintf("  %-28s  %-18s  %d min",
                    subs[i], hist_str, all_mins[length(starters) + i]))
  }
  message(sprintf("  Total: %d / 990", sum(all_mins)))

  # ── Print the ready-to-run call ───────────────────────────────────────────
  all_names <- c(starters, subs)
  message("\n── Copy-paste to apply: ────────────────────────────────────────────")
  lines <- paste0('  "', format(all_names, width = max(nchar(all_names))),
                  '" = ', formatC(all_mins, width = 2), ",")
  lines[length(lines)] <- sub(",$", "", lines[length(lines)])  # remove last comma
  cat('set_lineup_minutes("', team, '", c(\n', sep = "")
  cat(paste(lines, collapse = "\n"), "\n")
  if (!is.null(positions)) {
    pos_str <- paste0('"', names(positions), '" = "', positions, '"', collapse = ", ")
    cat('), positions = c(', pos_str, '))\n', sep = "")
  } else {
    cat('))\n')
  }

  if (apply) {
    message("\nApplying...")
    do.call(set_lineup_minutes, list(
      team       = team,
      minutes    = setNames(as.list(all_mins), all_names),
      positions  = positions,
      slate_path = slate_path
    ))
  }

  invisible(setNames(all_mins, all_names))
}

# To run (from the project root):
# source("R/00_setup.R")
# source("R/06_build_slate.R")
# build_slate()
# apply_lineup("LA Galaxy", starters = c("Puig", "Paintsil"), dnp = c("Nelson"))
# validate_slate_minutes()
# source("R/05_projections.R"); run_daily_projections()
