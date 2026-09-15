# 05_projections.R
# Daily projection runner
#
# Workflow:
#   1. build_slate()  → default mins = last game's minutes
#   2. Override mins only when you have intel (injury / rotation news)
#   3. run_daily_projections()

library(tidyverse)
library(glmmTMB)
library(lme4)
library(gt)
library(lubridate)
library(httr)
library(jsonlite)
library(openxlsx)

ASA_BASE <- "https://app.americansocceranalysis.com/api/v1/mls"

load_models     <- function() readRDS("data/processed/models.rds")
load_model_data <- function() readRDS("data/processed/model_dataset.rds")

# ── Team season stats (for game environment section) ──────────────────────────

get_team_season_stats <- function(season = format(Sys.Date(), "%Y")) {
  message("Pulling team season stats (", season, ")...")

  resp <- GET(paste0(ASA_BASE, "/teams/xgoals"),
              query = list(season_name = season, limit = 50))

  if (status_code(resp) != 200) {
    message("  Could not pull team season stats: ", status_code(resp))
    return(NULL)
  }

  df <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE) %>%
    rename_with(~ gsub("\\.", "_", .x))

  tryCatch({
    # Pull team_id → team_name lookup
    tresp <- GET(paste0(ASA_BASE, "/teams"), query = list(limit = 50))
    teams_lu <- fromJSON(content(tresp, "text", encoding = "UTF-8"), flatten = TRUE) %>%
      rename_with(~ gsub("\\.", "_", .x)) %>%
      select(team_id, team_name) %>%
      distinct(team_id, .keep_all = TRUE) %>%
      mutate(team_name = stringi::stri_trans_general(team_name, "Latin-ASCII"))

    stats <- df %>%
      left_join(teams_lu, by = "team_id") %>%
      filter(!is.na(team_name)) %>%
      transmute(
        team_name,
        goals_avg    = round(goals_for    / count_games, 2),
        ga_avg       = round(goals_against / count_games, 2),
        team_xg_diff = round((goals_for    - xgoals_for)    / count_games, 2),
        # negative = conceding MORE goals than xG allowed (weak defense / bad GK)
        opp_xg_diff  = round((xgoals_against - goals_against) / count_games, 2)
      ) %>%
      mutate(opp_def_rank = rank(ga_avg, ties.method = "min"))  # 1 = best defense

    message("  ", nrow(stats), " teams loaded")
    stats

  }, error = function(e) {
    message("  Team stats parse error: ", e$message)
    NULL
  })
}

# ── 1. Load slate ─────────────────────────────────────────────────────────────
# mins column: pre-filled with last game's minutes by build_slate()
# Override any row before running projections if you have lineup intel

load_slate <- function(path = "data/slate_today.csv") {

  resp  <- GET(paste0(ASA_BASE, "/teams"), query = list(limit = 50))
  teams <- fromJSON(content(resp, "text", encoding = "UTF-8")) %>%
    select(team_id, team_name)

  read_csv(path, show_col_types = FALSE) %>%
    mutate(minutes = pmin(as.numeric(mins), 90)) %>%   # cap at 90 (max regulation minutes)
    filter(!is.na(minutes), minutes > 0) %>%
    left_join(teams %>% rename(team_id  = team_id), by = c("team" = "team_name")) %>%
    left_join(teams %>% rename(opp_team_id = team_id), by = c("opp"  = "team_name")) %>%
    mutate(
      log_minutes = log(minutes / 90),
      days_rest   = as.numeric(Sys.Date() - last_game_date),
      is_home     = as.integer(is_home)
    ) %>%
    filter(!is.na(team_id), !is.na(opp_team_id))
}

# ── 2. Attach rolling player features ─────────────────────────────────────────

get_player_features <- function(slate, model_data) {
  feature_cols <- c("roll_shots_per90", "roll_shots_ot_per90",
                    "roll_xg_per90", "roll_xg_per_shot",
                    "roll_xassists_per90", "roll_key_passes_per90",
                    "minutes_sd_L10", "games_played_L10", "roll_minutes",
                    "roll_receiving_g_plus", "roll_xpass_diff")

  latest <- model_data %>%
    arrange(player_id, date) %>%
    group_by(player_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(player_id, position_general, all_of(feature_cols))

  result <- slate %>%
    left_join(latest, by = "player_id") %>%
    # Slate position overrides model_data if explicitly set (use for role changes)
    # Falls back to model_data position_general, then slate position
    mutate(position_general = coalesce(position, position_general))

  # Impute missing features with position-group medians from players we do have data for
  pos_medians <- result %>%
    filter(!is.na(roll_xg_per90)) %>%
    group_by(position_general) %>%
    summarise(across(all_of(feature_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
    rename_with(~ paste0(.x, "_med"), all_of(feature_cols))

  new_players <- result %>% filter(is.na(roll_xg_per90)) %>% pull(player_name)
  if (length(new_players) > 0) {
    message("  Imputing features for (no model history): ",
            paste(new_players, collapse = ", "))
  }

  result %>%
    left_join(pos_medians, by = "position_general") %>%
    mutate(across(all_of(feature_cols),
                  ~ coalesce(.x, get(paste0(cur_column(), "_med"))))) %>%
    select(-ends_with("_med"))
}

# ── 3. Attach opponent defensive profile ──────────────────────────────────────

get_opp_features <- function(slate, model_data) {
  opp_latest <- model_data %>%
    arrange(opp_team_id, position_general, date) %>%
    group_by(opp_team_id, position_general) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(opp_team_id, position_general,
           opp_xg_conceded_L10, opp_shots_conceded_L10,
           opp_shots_ot_conceded_L10, opp_goals_conceded_L10)

  slate %>% left_join(opp_latest, by = c("opp_team_id", "position_general"))
}

# ── 4. Generate raw projections ───────────────────────────────────────────────

generate_projections <- function(slate_features, models) {
  nd <- slate_features

  nd$proj_shots <- predict(models$shots, newdata = nd,
                           type = "response", allow.new.levels = TRUE)

  nd_sot <- nd %>% mutate(shots = proj_shots,
                           shots_off_target = pmax(proj_shots * 0.5, 0))
  nd$proj_shots_ot <- nd$proj_shots *
    predict(models$shots_ot, newdata = nd_sot,
            type = "response", allow.new.levels = TRUE)

  # Goals — offset(log(shots + 0.5)) is baked into the model
  # predict(type="response") already returns expected goals — do NOT multiply by shots again
  nd_goals <- nd %>% mutate(shots = proj_shots)
  nd$proj_goals <- predict(models$goals, newdata = nd_goals,
                           type = "response", allow.new.levels = TRUE)

  nd$proj_assists <- predict(models$assists, newdata = nd,
                             type = "response", allow.new.levels = TRUE)
  nd
}

# ── 5. Scale goals to match externally sourced implied team totals ────────────
# If the external market signal implies NYRB scores 1.2 goals but our model
# sums to 1.8, scale each NYRB player's goal projection down proportionally.
# Shots and assists are NOT scaled — only goals, since that's the only total
# the external signal provides.

MARKET_NAME_TO_ASA <- c(
  "Los Angeles FC"       = "Los Angeles FC",
  "LA Galaxy"            = "LA Galaxy",
  "New York Red Bulls"   = "New York Red Bulls",
  "New York City FC"     = "New York City FC",
  "Portland Timbers"     = "Portland Timbers FC",
  "St. Louis City SC"    = "St. Louis City SC",
  "Vancouver Whitecaps"  = "Vancouver Whitecaps FC",
  "Columbus Crew SC"     = "Columbus Crew",
  "Sporting KC"          = "Sporting Kansas City",
  "Inter Miami"          = "Inter Miami CF",
  "Real Salt Lake"       = "Real Salt Lake",
  "Chicago Fire"         = "Chicago Fire FC",
  "Houston Dynamo"       = "Houston Dynamo FC"
)

normalize_market_name <- function(name) {
  ifelse(name %in% names(MARKET_NAME_TO_ASA), MARKET_NAME_TO_ASA[name], name)
}

scale_to_team_totals <- function(projections, implied_goals) {

  if (is.null(implied_goals)) {
    message("No implied team totals — skipping calibration")
    return(projections)
  }

  # implied_goals has: team (ESPN name), implied_lambda
  # projections has: team (ASA name)

  implied_lookup <- implied_goals %>%
    mutate(team_asa  = normalize_market_name(team),
           game_date = as.Date(game_time)) %>%
    select(game_date, team_asa, implied_lambda)

  # Sum model's goal and assist projections per team per game
  team_model_totals <- projections %>%
    mutate(game_date = as.Date(game_time)) %>%
    group_by(game_date, team) %>%
    summarise(
      model_team_goals   = sum(proj_goals,   na.rm = TRUE),
      model_team_assists = sum(proj_assists, na.rm = TRUE),
      .groups = "drop"
    )

  # Compute separate scale factors for goals and assists
  scale_factors <- team_model_totals %>%
    left_join(implied_lookup, by = c("game_date", "team" = "team_asa")) %>%
    mutate(
      scale_goals   = if_else(!is.na(implied_lambda) & model_team_goals   > 0,
                              implied_lambda / model_team_goals,   1.0),
      scale_assists = if_else(!is.na(implied_lambda) & model_team_assists > 0,
                              implied_lambda / model_team_assists, 1.0)
    ) %>%
    select(game_date, team, scale_goals, scale_assists, implied_lambda,
           model_team_goals, model_team_assists)

  message("\nTeam total calibration (all teams — NA implied_lambda = name mismatch):")
  print(scale_factors %>%
    mutate(across(where(is.double), ~ round(.x, 3))), n = Inf)

  scaled <- projections %>%
    mutate(game_date = as.Date(game_time)) %>%
    left_join(scale_factors %>% select(game_date, team, scale_goals, scale_assists),
              by = c("game_date", "team")) %>%
    mutate(
      proj_goals_raw   = proj_goals,
      proj_assists_raw = proj_assists,
      proj_goals   = proj_goals * coalesce(scale_goals, 1.0),
      # assists are not scaled — model prediction is kept as-is
      scale_goals   = NULL,
      scale_assists = NULL,
      game_date     = NULL
    )

  # Post-scaling verification: actual (unrounded) goal sums vs implied
  message("\nPost-scaling verification (unrounded):")
  check <- scaled %>%
    mutate(game_date = as.Date(game_time)) %>%
    group_by(game_date, team) %>%
    summarise(scaled_goals = sum(proj_goals, na.rm = TRUE), .groups = "drop") %>%
    left_join(implied_lookup, by = c("game_date", "team" = "team_asa")) %>%
    filter(!is.na(implied_lambda)) %>%
    mutate(diff = round(scaled_goals - implied_lambda, 4))
  print(check, n = Inf)

  scaled
}

# ── Helper: transliterate names to ASCII ──────────────────────────────────────
# Fixes encoding artifacts: Dénis → Denis, Gonçalves → Goncalves, Montréal → Montreal

clean_name <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII")
}

# ── 6. Format output ──────────────────────────────────────────────────────────

format_projections <- function(proj_df, implied_goals = NULL, team_stats = NULL) {

  # Join implied team total per player
  if (!is.null(implied_goals) && nrow(implied_goals) > 0) {
    proj_df <- proj_df %>%
      mutate(game_date = as.Date(game_time)) %>%
      left_join(
        implied_goals %>%
          mutate(team_asa  = normalize_market_name(team),
                 game_date = as.Date(game_time)) %>%
          select(game_date, team_asa, implied_lambda),
        by = c("game_date", "team" = "team_asa")
      ) %>%
      select(-game_date)
  } else {
    proj_df <- proj_df %>% mutate(implied_lambda = NA_real_)
  }

  # Base projections
  out <- proj_df %>%
    mutate(
      proj_g_or_a = 1 - exp(-proj_goals) * exp(-proj_assists),
      player      = clean_name(player_name),
      team        = clean_name(team),
      opp         = clean_name(opp)
    ) %>%
    transmute(
      game_time  = format(with_tz(game_time, "America/New_York"), "%m/%d %I:%M %p"),
      player, team, opp,
      mins       = round(minutes, 0),
      position   = position_general,
      concacaf_days = if ("concacaf_days" %in% names(.)) concacaf_days else NA_integer_,
      team_total = round(implied_lambda, 2),
      goals      = round(proj_goals, 2),
      assists    = round(proj_assists, 2),
      g_or_a     = round(proj_g_or_a, 2),
      shots_ot   = round(proj_shots_ot, 2),
      shots      = round(proj_shots, 2),
      is_home    = as.logical(is_home)
    )

  # ── Join game environment stats ───────────────────────────────────────────────
  if (!is.null(team_stats)) {
    # Team attacking context (goals_avg, team_xg_diff)
    team_off <- team_stats %>%
      select(team_name, goals_avg, team_xg_diff)

    # Opponent defensive context (opp_def_rank, opp_xg_diff)
    team_def <- team_stats %>%
      select(team_name, opp_def_rank, opp_xg_diff)

    out <- out %>%
      left_join(team_off, by = c("team" = "team_name")) %>%
      left_join(team_def, by = c("opp"  = "team_name")) %>%
      mutate(
        tt_vs_avg = round(team_total - goals_avg, 2)
      )
  } else {
    out <- out %>% mutate(
      goals_avg    = NA_real_,
      tt_vs_avg    = NA_real_,
      opp_def_rank = NA_integer_,
      team_xg_diff = NA_real_,
      opp_xg_diff  = NA_real_
    )
  }

  # Final column order
  out %>%
    select(
      player, game_time, team, opp, mins, position, concacaf_days,
      # ── Team total + game environment ──
      team_total, goals_avg, tt_vs_avg, opp_def_rank,
      team_xg_diff, opp_xg_diff,
      # ── Projections ──
      goals, assists, shots_ot, shots, g_or_a,
      is_home
    ) %>%
    arrange(game_time, team, desc(is_home), desc(g_or_a))
}

# ── 7. Print gt table ─────────────────────────────────────────────────────────

print_projections <- function(proj_formatted) {
  proj_formatted %>%
    gt() %>%
    tab_header(
      title    = paste("MLS Player Projections —", format(Sys.Date(), "%b %d, %Y")),
      subtitle = "Goals scaled to external market-implied team totals"
    ) %>%
    cols_label(
      player     = "Player",   team       = "Team",    opp        = "Opp",
      mins       = "Min",      position   = "Pos",     team_total = "TmTot",
      goals      = "Goals",    assists    = "Ast",     g_or_a     = "G+A",
      shots_ot   = "SOT",      shots      = "Shots",
      is_home    = "Home"
    ) %>%
    fmt_number(columns = c(goals, assists, g_or_a, shots_ot, shots), decimals = 2) %>%
    data_color(columns = g_or_a, palette = "Blues") %>%
    tab_style(
      style     = cell_fill(color = "#90EE90"),
      locations = cells_body(
        columns = team_total,
        rows    = !is.na(team_total) & team_total >= 2
      )
    )
}

# ── Excel export ──────────────────────────────────────────────────────────────

export_projections_xlsx <- function(df, path) {

  wb <- createWorkbook()
  addWorksheet(wb, "Projections")

  col_idx <- function(nm) which(names(df) == nm)

  # ── Color palette for 5 stat groups ─────────────────────────────────────────
  groups <- list(
    game_env = list(cols = c("team_total","goals_avg","tt_vs_avg","opp_def_rank"),
                    hdr = "#2C5F8A", cell = "#D6E8F5"),   # steel blue
    xg_env   = list(cols = c("team_xg_diff","opp_xg_diff"),
                    hdr = "#0D7377", cell = "#D4F1F4"),   # teal
    goals    = list(cols = c("goals"),
                    hdr = "#2E75B6", cell = "#D9E1F2"),   # blue
    assists  = list(cols = c("assists"),
                    hdr = "#C55A11", cell = "#FCE4D6"),   # orange
    shots_ot = list(cols = c("shots_ot"),
                    hdr = "#538135", cell = "#E2EFDA"),   # green
    shots    = list(cols = c("shots"),
                    hdr = "#BF9000", cell = "#FFF2CC"),   # gold
    g_or_a   = list(cols = c("g_or_a"),
                    hdr = "#7030A0", cell = "#EAD1F5")    # purple
  )

  # ── Base styles ──────────────────────────────────────────────────────────────
  base_hdr   <- createStyle(fontName = "Calibri", fontSize = 11,
                             textDecoration = "bold", fontColour = "#FFFFFF",
                             halign = "center", fgFill = "#2C3E50")
  number_2dp <- createStyle(numFmt = "0.00", halign = "center")
  integer_s  <- createStyle(numFmt = "0",    halign = "center")
  center_s   <- createStyle(halign = "center")
  alt_row    <- createStyle(fgFill = "#F5F5F5")
  green_fill <- createStyle(fgFill = "#90EE90")

  # ── Write data ───────────────────────────────────────────────────────────────
  writeData(wb, "Projections", df, headerStyle = base_hdr)

  # ── Column widths ─────────────────────────────────────────────────────────────
  col_widths_map <- c(
    player        = 20,
    game_time     = 16,  # "04/11 08:45 PM"
    team          = 22,
    opp           = 22,
    mins          =  5,
    position      =  6,
    concacaf_days =  9,
    team_total    =  7,
    goals_avg     =  7,
    tt_vs_avg     =  8,
    opp_def_rank  =  8,
    team_xg_diff  =  9,
    opp_xg_diff   =  9,
    goals         =  6,
    assists       =  6,
    shots_ot      =  6,
    shots         =  6,
    g_or_a        =  6,
    is_home       =  6
  )
  for (nm in intersect(names(col_widths_map), names(df))) {
    setColWidths(wb, "Projections", cols = col_idx(nm), widths = col_widths_map[[nm]])
  }

  nrows <- nrow(df)
  ncols <- ncol(df)
  data_rows <- 2:(nrows + 1)

  # Alternating rows
  even_rows <- seq(3, nrows + 1, by = 2)
  if (length(even_rows) > 0)
    addStyle(wb, "Projections", alt_row,
             rows = even_rows, cols = 1:ncols, gridExpand = TRUE, stack = TRUE)

  # ── Group header + cell colors ───────────────────────────────────────────────
  for (g in groups) {
    present <- intersect(g$cols, names(df))
    if (length(present) == 0) next
    ci <- sapply(present, col_idx)

    grp_hdr <- createStyle(fontName = "Calibri", fontSize = 11,
                            textDecoration = "bold", fontColour = "#FFFFFF",
                            halign = "center", fgFill = g$hdr)
    grp_cell <- createStyle(fgFill = g$cell, halign = "center")

    addStyle(wb, "Projections", grp_hdr,  rows = 1,          cols = ci, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Projections", grp_cell, rows = data_rows,  cols = ci, gridExpand = TRUE, stack = TRUE)
  }

  # ── Number formats ───────────────────────────────────────────────────────────
  for (col in intersect(names(df), c("goals","assists","g_or_a","shots_ot","shots",
                                      "team_total","goals_avg","tt_vs_avg",
                                      "team_xg_diff","opp_xg_diff")))
    addStyle(wb, "Projections", number_2dp, rows = data_rows, cols = col_idx(col), stack = TRUE)

  for (col in intersect(names(df), c("opp_def_rank", "mins")))
    addStyle(wb, "Projections", integer_s, rows = data_rows, cols = col_idx(col), stack = TRUE)

  for (col in intersect(names(df), c("position","is_home")))
    addStyle(wb, "Projections", center_s, rows = data_rows, cols = col_idx(col), stack = TRUE)

  # ── Shared conditional styles ─────────────────────────────────────────────────
  pos_style <- createStyle(fgFill = "#C6EFCE", fontColour = "#276221")
  neg_style <- createStyle(fgFill = "#FFC7CE", fontColour = "#9C0006")

  cond_color <- function(col, pos_test, neg_test) {
    if (!col %in% names(df)) return(invisible(NULL))
    ci   <- col_idx(col)
    vals <- df[[col]]
    pr <- which(!is.na(vals) & pos_test(vals)) + 1
    nr <- which(!is.na(vals) & neg_test(vals)) + 1
    if (length(pr) > 0) addStyle(wb, "Projections", pos_style, rows = pr, cols = ci, stack = TRUE)
    if (length(nr) > 0) addStyle(wb, "Projections", neg_style, rows = nr, cols = ci, stack = TRUE)
  }

  # ── team_total >= 2 → green ──────────────────────────────────────────────────
  if ("team_total" %in% names(df)) {
    hi_rows <- which(!is.na(df$team_total) & df$team_total >= 2) + 1
    if (length(hi_rows) > 0)
      addStyle(wb, "Projections", green_fill,
               rows = hi_rows, cols = col_idx("team_total"), stack = TRUE)
  }

  # ── tt_vs_avg: >= +0.50 green, <= -0.50 red ──────────────────────────────────
  cond_color("tt_vs_avg",
             pos_test = function(v) v >=  0.50,
             neg_test = function(v) v <= -0.50)

  # ── opp_def_rank: top 10 (best defense) red, bottom 10 (worst defense) green ─
  if ("opp_def_rank" %in% names(df)) {
    n_teams <- max(df$opp_def_rank, na.rm = TRUE)
    cond_color("opp_def_rank",
               pos_test = function(v) v >= (n_teams - 9),   # worst defenses = green
               neg_test = function(v) v <= 10)               # best defenses  = red
  }

  # ── team_xg_diff: >= +1.0 red (over-performing xG, regression likely), <= -1.0 green (under-performing) ─
  cond_color("team_xg_diff",
             pos_test = function(v) v <= -1.0,   # under-performing xG = green (due for more goals)
             neg_test = function(v) v >=  1.0)   # over-performing xG  = red  (likely to regress)

  # ── opp_xg_diff: >= +1.0 green (defense due for regression), <= -1.0 red (lucky goals, will tighten) ─
  cond_color("opp_xg_diff",
             pos_test = function(v) v >=  1.0,   # xGA >> goals allowed = defense due to get worse = green
             neg_test = function(v) v <= -1.0)   # conceding lucky goals = will improve = red

  # ── CONCACAF days — orange warning if flagged ─────────────────────────────────
  if ("concacaf_days" %in% names(df)) {
    ci <- col_idx("concacaf_days")
    addStyle(wb, "Projections",
             createStyle(fontName = "Calibri", fontSize = 11, textDecoration = "bold",
                         fontColour = "#FFFFFF", halign = "center", fgFill = "#843C0C"),
             rows = 1, cols = ci, stack = TRUE)
    flagged_rows <- which(!is.na(df$concacaf_days)) + 1
    if (length(flagged_rows) > 0)
      addStyle(wb, "Projections",
               createStyle(fgFill = "#FCE4D6", fontColour = "#843C0C",
                           halign = "center", textDecoration = "bold"),
               rows = flagged_rows, cols = ci, stack = TRUE)
    addStyle(wb, "Projections", integer_s, rows = data_rows, cols = ci, stack = TRUE)
  }

  # ── Polish ───────────────────────────────────────────────────────────────────
  freezePane(wb, "Projections", firstRow = TRUE, firstActiveCol = 2)
  setColWidths(wb, "Projections", cols = 1:ncols, widths = "auto")

  saveWorkbook(wb, path, overwrite = TRUE)
  system(paste("open", shQuote(path)))
}

# ── Master runner ──────────────────────────────────────────────────────────────

run_daily_projections <- function(slate_path           = "data/slate_today.csv",
                                  use_market_calibration = TRUE,
                                  export_xlsx           = TRUE) {

  message("Loading models and data...")
  models     <- load_models()
  model_data <- load_model_data()

  message("Loading slate...")
  slate <- load_slate(slate_path)
  message("  ", nrow(slate), " players loaded")

  message("Attaching player features...")
  slate <- get_player_features(slate, model_data)

  message("Attaching opponent profiles...")
  slate <- get_opp_features(slate, model_data)

  message("Generating projections...")
  projections <- generate_projections(slate, models)

  implied    <- NULL
  team_stats <- NULL

  if (use_market_calibration) {
    tryCatch({
      source("R/07_market_calibration.R", local = TRUE)

      implied     <- get_implied_team_goals()
      projections <- scale_to_team_totals(projections, implied)

      team_stats  <- get_team_season_stats()

    }, error = function(e) {
      message("Market calibration step skipped: ", e$message)
      projections$proj_goals_raw   <<- projections$proj_goals
      projections$proj_assists_raw <<- projections$proj_assists
    })
  } else {
    projections$proj_goals_raw   <- projections$proj_goals
    projections$proj_assists_raw <- projections$proj_assists
  }

  # Only include games that haven't started yet and are within 6 days
  projections <- projections %>%
    filter(game_time > Sys.time(),
           as.Date(game_time) <= Sys.Date() + 6)

  output <- format_projections(projections, implied, team_stats)

  if (export_xlsx) {
    out_path <- paste0("output/projections_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".xlsx")
    export_projections_xlsx(output, out_path)
    message("\nSaved: ", out_path)
  }

  print_projections(output)
  invisible(output)
}

# Run (from the project root):
# source("R/00_setup.R")
# source("R/05_projections.R")
# run_daily_projections()
#
# Then track_projection_accuracy() in R/08_backtest.R to build a track record.
