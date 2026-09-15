# 09_documentation.R
# Generates the technical documentation for the MLS Player Projection System
#
# Installs officer + flextable if needed, then creates:
#   output/MLS_Player_Projections_Documentation.docx

library(magrittr)  # for %>% — this script can run standalone, without 00_setup.R

for (pkg in c("officer", "flextable")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  library(pkg, character.only = TRUE)
}

build_documentation <- function(out_path = "output/MLS_Player_Projections_Documentation.docx") {

  doc <- read_docx()

  # ── helpers ──────────────────────────────────────────────────────────────────
  h1  <- function(doc, txt) body_add_par(doc, txt, style = "heading 1")
  h2  <- function(doc, txt) body_add_par(doc, txt, style = "heading 2")
  p   <- function(doc, txt) body_add_par(doc, txt, style = "Normal")
  br  <- function(doc)      body_add_par(doc, "",  style = "Normal")

  tbl <- function(doc, df, col_widths = NULL) {
    ft <- flextable(df) %>%
      theme_booktabs() %>%
      fontsize(size = 9, part = "all") %>%
      bold(part = "header") %>%
      bg(bg = "#2C3E50", part = "header") %>%
      color(color = "white", part = "header") %>%
      padding(padding = 3, part = "all")
    if (!is.null(col_widths)) {
      ft <- width(ft, j = seq_along(col_widths), width = col_widths)
    } else {
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
    }
    body_add_flextable(doc, ft)
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # TITLE
  # ═══════════════════════════════════════════════════════════════════════════

  doc <- doc %>%
    h1("MLS Player Performance Projection System") %>%
    p(paste("Documentation — Version 2.0 |", format(Sys.Date(), "%B %d, %Y"))) %>%
    br()

  # ═══════════════════════════════════════════════════════════════════════════
  # SECTION 1: THE THESIS
  # ═══════════════════════════════════════════════════════════════════════════

  doc <- doc %>%
    h1("1. The Thesis") %>%
    p("Most publicly available player output projections for MLS rely on season-long averages and simple recency windows. They under-weight two things that matter a lot in soccer: how a player's underlying shot and chance-creation quality is trending, and how much of that opportunity is a function of who they're facing, position for position.") %>%
    br() %>%
    p("This system projects each player's expected shots, shots on target, goals, and assists for an upcoming match by combining shot-quality metrics (xG per shot), opponent-specific defensive vulnerabilities broken down by position group, decay-weighted recent form, playing-time calibration, and advanced passing/positioning metrics. The goal is projections that are useful for side-by-side comparison across a full slate of MLS games — for advance scouting, rotation and roster planning, and tracking whether a player's underlying output is trending up or down independent of results.") %>%
    br() %>%
    p("The process is:") %>%
    p("  1. Build player-game features from tracking data (xG, xA, shots, key passes, goals added, xPass, minutes)") %>%
    p("  2. Train four hierarchical statistical models — shots, shots on target, goals, assists") %>%
    p("  3. Generate individual player projections (lambda values) for the upcoming slate") %>%
    p("  4. Optionally anchor team-level goal totals to an external market-implied signal for that game's scoring environment") %>%
    p("  5. Track projection accuracy over time against actual results to validate and improve the model") %>%
    br() %>%
    p("The projections are not meant to predict any single game perfectly — individual match outcomes are inherently noisy. The value is in being well-calibrated across a large sample of player-games over a season, so relative rankings (who is trending up, who has a favorable matchup) are trustworthy even when any one number is off.") %>%
    br()

  # ═══════════════════════════════════════════════════════════════════════════
  # SECTION 2: DATA SOURCES
  # ═══════════════════════════════════════════════════════════════════════════

  doc <- doc %>%
    h1("2. Data Sources") %>%
    h2("2.1 American Soccer Analysis (ASA) API") %>%
    p("ASA is the primary data source. All data is pulled at the player-game level (one row per player per game), giving the model access to within-season form and game-by-game variance.") %>%
    br()

  asa_table <- data.frame(
    Endpoint = c(
      "/players/xgoals",
      "/players/xgoals",
      "/players/xgoals",
      "/players/xgoals",
      "/players/xpass",
      "/players/xpass",
      "/players/goals-added",
      "/players/goals-added",
      "/teams/xgoals",
      "/games"
    ),
    `Field` = c(
      "shots, shots_on_target, goals",
      "xgoals (expected goals)",
      "xassists, key_passes, primary_assists",
      "general_position",
      "pass_completion_pct, xpass_completion_pct",
      "attempted_passes",
      "passing_goals_added (per game)",
      "receiving_goals_added (per game)",
      "goals_for/against, xgoals_for/against",
      "date, home_team_id, away_team_id, score"
    ),
    `Used In Model` = c(
      "Yes — outcome variables for all four models",
      "Yes — xG/shot quality feature (roll_xg_per_shot)",
      "Yes — assists model features (roll_xassists_per90, roll_key_passes_per90)",
      "Yes — position-specific opponent defensive profiling",
      "Yes — xPass diff feature in assists model (roll_xpass_diff)",
      "Yes — pass volume context",
      "Yes — assists model feature (roll_passing_g_plus)",
      "Yes — shots model feature (roll_receiving_g_plus)",
      "Yes — team season stats, game environment section",
      "Yes — days_rest, home/away flag, date joins"
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, asa_table, col_widths = c(1.5, 1.9, 3.1))

  doc <- doc %>%
    br() %>%
    h2("2.2 External Market Calibration Signal (optional)") %>%
    p("Team-level goal projections can optionally be anchored to an externally sourced implied team-goals figure, pulled via The Odds API. This is used purely as a calibration input for the scoring environment of a given matchup (e.g. 'this game projects as high-scoring') — it does not drive individual player projections, and no market pricing data is surfaced anywhere in the output. If this step is skipped or unavailable, the model's own raw team-level sum is used instead.") %>%
    br()

  calib_table <- data.frame(
    `Data` = c(
      "Team goal totals (implied from published lines)"
    ),
    `How Used` = c(
      "A Poisson lambda is fit per team per game from the implied over/under probabilities, then the model's own player-level goal projections for that team are scaled so their sum matches the external total. Shots and assists are never scaled — only the team goal total."
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, calib_table, col_widths = c(2.5, 4.0))
  doc <- br(doc)

  # ═══════════════════════════════════════════════════════════════════════════
  # SECTION 3: MODEL ARCHITECTURE
  # ═══════════════════════════════════════════════════════════════════════════

  doc <- doc %>%
    h1("3. Model Architecture") %>%
    p("The projection pipeline is a four-stage hierarchical model. Each stage feeds into the next, separating volume from quality from finishing. All models are fit using glmmTMB (R package) with player random effects to capture individual tendencies beyond what the features explain.") %>%
    br() %>%
    h2("3.1 Decay-Weighted Rolling Features") %>%
    p("All rolling features are computed over the last 10 games using exponential decay with alpha = 0.85. The most recent game is weighted 1.0, the prior game 0.85, the one before 0.72, and so on. This makes the model significantly more responsive to recent form than a simple moving average. All per-90 rates are capped to prevent extreme values from short-minute appearances distorting the rolling averages.") %>%
    br() %>%
    h2("3.2 Opponent Defensive Profile (Position-Specific)") %>%
    p("Rather than using a single team-level defensive rating, the model builds a separate defensive profile for each team broken down by position group (ST, AM, W, DM, FB, CB, GK). For each matchup, a striker faces the opponent's defensive record against strikers specifically — not their overall record. Features: rolling L10 xG conceded, shots conceded, and shots on target conceded vs the specific position group. All decay-weighted.") %>%
    br() %>%
    h2("3.3 The Four Models") %>%
    br()

  model_table <- data.frame(
    Stage = c("1", "2", "3", "4"),
    Model = c("Shots", "Shots on Target Rate", "Goals", "Assists"),
    `Distribution` = c(
      "Negative Binomial (NB2)",
      "Binomial",
      "Poisson",
      "Zero-Inflated Poisson"
    ),
    `Key Features` = c(
      "log(minutes), home/away, days_rest, roll_shots/90, roll_xG/shot, opp_shots_conceded_L10, opp_xG_conceded_L10, roll_receiving_g_plus",
      "roll_SOT/shots rate (L10), opp_SOT_conceded_L10, home/away",
      "offset(log(shots+0.5)), roll_xG/shot (L10), opp_xG_conceded_L10, home/away",
      "offset(log(minutes)), roll_xA/90, roll_key_passes/90, opp_xG_conceded_L10, home/away, roll_xpass_diff; zero-inflation on xA/90"
    ),
    `Why This Distribution` = c(
      "Shots are overdispersed counts — NB2 fits variance better than Poisson",
      "Fraction of shots on target given shots taken — naturally bounded 0-1",
      "Goals per shot conditional on shot volume; Poisson for rare events",
      "Most players get 0 assists most games — ZIP separates structural zeros from Poisson count"
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, model_table, col_widths = c(0.4, 0.9, 0.9, 2.2, 2.1))

  doc <- doc %>%
    br() %>%
    h2("3.4 Advanced Features: Goals Added + xPass Differential") %>%
    p("Two additional features from ASA were added following a formal dev test (backtest Brier score comparison, 2023-2024 train / 2025 test):") %>%
    br()

  dev_feat_table <- data.frame(
    Feature = c("roll_receiving_g_plus", "roll_xpass_diff"),
    Source = c(
      "ASA /players/goals-added (receiving component)",
      "ASA /players/xpass (completion% minus xCompletion%)"
    ),
    `What It Measures` = c(
      "How consistently a player gets on the ball in dangerous areas and creates value off the ball. Decay-weighted rolling average of receiving goals added per 90 over L10 games.",
      "How much harder a player's passes are than average, and whether they complete them. Positive = completing passes harder than expected. Decay-weighted L10 rolling average."
    ),
    `Added To` = c(
      "Shots model only. Improves Brier skill +0.007. Not in goals model (confounds shot quality)",
      "Assists model. Small improvement (+0.001 Brier skill). Captures creative passers beyond raw xA."
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, dev_feat_table, col_widths = c(1.3, 1.5, 2.2, 1.5))

  doc <- doc %>%
    br() %>%
    h2("3.5 Team Total Calibration") %>%
    p("Raw model projections can be anchored to an external market-implied signal for the game's total scoring environment:") %>%
    p("  1. Pull implied over/under probabilities for a team's game total from the external data source") %>%
    p("  2. Convert those probabilities to implied Poisson lambda for that team's expected goals") %>%
    p("  3. Compute scale_factor = implied_lambda / sum(player_proj_goals)") %>%
    p("  4. Multiply every player's goal projection by scale_factor") %>%
    br() %>%
    p("Shots and assists are NOT scaled — only goals. Individual player goal rankings still come from the model; the team-level total is anchored to the external signal, which aggregates far more information (injury news, tactical intelligence) than the model alone has access to.") %>%
    br() %>%
    h2("3.6 Probability & Calibration Metrics") %>%
    p("For internal accuracy tracking (see Section 5), each projected lambda is converted to the probability of clearing a standard evaluation threshold using the relevant distribution's survival function:") %>%
    br()

  prob_table <- data.frame(
    Stat = c("Goals (>=1)", "Assists (>=1)", "Shots on target (>=2)", "Shots (>=3)"),
    Formula = c(
      "P = 1 - e^(-lambda_goals)",
      "P = 1 - e^(-lambda_assists)",
      "P = 1 - Poisson_CDF(1, lambda_shots_ot)",
      "P = 1 - Poisson_CDF(2, lambda_shots)"
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, prob_table, col_widths = c(2.5, 4.0))
  doc <- br(doc)

  # ═══════════════════════════════════════════════════════════════════════════
  # SECTION 4: COLUMN KEY
  # ═══════════════════════════════════════════════════════════════════════════

  doc <- doc %>%
    h1("4. Column Key") %>%
    h2("4.1 Identity Columns") %>%
    br()

  id_cols <- data.frame(
    Column = c("game_time", "player", "team", "opp", "mins", "position", "is_home"),
    Description = c(
      "UTC kickoff time",
      "Player name (ASCII-normalised — accents removed for matching)",
      "Player's team",
      "Opponent team",
      "Projected minutes — manually set after confirming lineup",
      "Position group: ST, AM, W, DM, FB, CB, GK",
      "TRUE if player's team is the home side"
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, id_cols, col_widths = c(1.5, 5.0))

  doc <- doc %>%
    br() %>%
    h2("4.2 Game Environment Section") %>%
    p("Steel blue header columns. Contextual team/matchup data for the current season. Use these to assess game environment quality before weighing a projection.") %>%
    br()

  env_cols <- data.frame(
    Column = c(
      "team_total",
      "goals_avg",
      "tt_vs_avg",
      "opp_def_rank",
      "team_xg_diff",
      "opp_xg_diff"
    ),
    Description = c(
      "External market-implied team goals for this game (Poisson lambda fitted from over/under signal, when available)",
      "Team's actual goals scored per game this season",
      "team_total minus goals_avg. How the external signal views this game vs season form",
      "Opponent defensive rank (1 = best/tightest defense, 29/30 = worst). Based on goals conceded per game this season",
      "Team's (goals scored - xG) per game this season. Positive = scoring more than xG (over-performing). Negative = scoring less (under-performing, due for more)",
      "Opponent's (xG allowed - goals conceded) per game. Negative = opponent conceding more goals than xG predicted (defense leaking lucky goals). Positive = defense out-performing xG"
    ),
    `Color Logic` = c(
      "Green fill when >= 2.0 (high-scoring game environment)",
      "None",
      "Green >= +0.50 (signal expects more than season form); Red <= -0.50 (signal expects less)",
      "Red = top 10 (tough defense — hard matchup); Green = bottom 10 (weak defense — good matchup)",
      "Green <= -1.0 (under-performing xG, regression due); Red >= +1.0 (over-performing, may regress down)",
      "Green <= -1.0 (opponent leaking lucky goals, good matchup to attack into); Red >= +1.0 (defense better than results show, will tighten)"
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, env_cols, col_widths = c(1.1, 2.1, 3.3))

  doc <- doc %>%
    br() %>%
    h2("4.3 Projection Columns") %>%
    p("Five color-coded projection columns, one per modeled stat plus a combined goal-or-assist figure:") %>%
    br()

  prop_groups <- data.frame(
    Column = c("goals (blue)", "assists (orange)", "shots_ot (green)", "shots (gold)", "g_or_a (purple)"),
    Description = c(
      "Projected goals for this game (lambda), after team-total calibration",
      "Projected assists for this game (lambda)",
      "Projected shots on target for this game (lambda)",
      "Projected shots for this game (lambda)",
      "Projected probability of at least one goal or assist: 1 - e^(-goals) x e^(-assists)"
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, prop_groups, col_widths = c(1.6, 5.0))

  # ═══════════════════════════════════════════════════════════════════════════
  # SECTION 5: BACKTEST FRAMEWORK
  # ═══════════════════════════════════════════════════════════════════════════

  doc <- doc %>%
    h1("5. Backtest Framework") %>%
    p("The backtest system (08_backtest.R) has three parts:") %>%
    br() %>%
    h2("5.1 Calibration Backtest") %>%
    p("Refits all four models on a training period, generates out-of-sample predictions on a test period, and reports Brier scores (probabilistic calibration) and calibration curves. Brier skill score > 0 means the model beats a naive guess; higher is better.") %>%
    br()

  brier_table <- data.frame(
    Stat = c("3+ shots", "2+ shots on target", "Assists (>=1)", "Goals (>=1)"),
    `Brier Skill Score` = c("0.385", "0.303", "0.101", "0.001"),
    `Interpretation` = c(
      "Strong — best-performing stat. Shot volume is the most predictable output.",
      "Strong — second best. Shot accuracy is stable across players.",
      "Moderate — assists are noisier but model adds real signal.",
      "Weak standalone — corrected by team-total calibration. Goal rankings within a team are more reliable than absolute rates."
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, brier_table, col_widths = c(1.5, 1.2, 3.8))

  doc <- doc %>%
    br() %>%
    h2("5.2 Dev Feature Testing") %>%
    p("Before any new feature is promoted to production, compare_model_features() trains baseline and enhanced models on the same train/test split and reports side-by-side Brier scores. A feature is only promoted if ALL four stats show same or better skill. This prevents over-fitting from features that improve one stat at the cost of another.") %>%
    br() %>%
    h2("5.3 Weekly Projection Accuracy Tracking") %>%
    p("log_projections() saves each week's output to data/backtest/projections_log.csv. log_outcomes() pulls actual ASA results after games finish. track_projection_accuracy() joins both logs and reports hit-rate and Brier-score calibration by stat and by predicted-probability bucket — i.e. when the model said there was a 40% chance of an event, did it happen about 40% of the time? This is the ground truth for whether the model's probability estimates hold up over a season.") %>%
    br()

  # ═══════════════════════════════════════════════════════════════════════════
  # SECTION 6: WORKFLOW
  # ═══════════════════════════════════════════════════════════════════════════

  doc <- doc %>%
    h1("6. Weekly Workflow") %>%
    br()

  workflow <- data.frame(
    Step = c("1", "2", "3", "4", "5", "6", "7", "8"),
    Action = c(
      "Pull fresh ASA data",
      "Rebuild feature dataset",
      "Retrain models (if new data)",
      "Build slate",
      "Set confirmed lineups",
      "Run projections",
      "Evaluate Excel sheet",
      "Log for backtest"
    ),
    Command = c(
      "source('R/01_pull_asa.R'); pull_all_asa()",
      "source('R/03_feature_engineering.R'); build_model_dataset()",
      "source('R/04_model.R'); train_all_models()",
      "source('R/06_build_slate.R'); build_slate()",
      "auto_lineup('Team Name', starters=c(...), subs=c(...))",
      "source('R/05_projections.R'); run_daily_projections()",
      "Open output/projections_YYYY-MM-DD.xlsx",
      "log_projections(proj); log_outcomes(game_date)"
    ),
    Notes = c(
      "Run weekly — refreshes xG, xA, shots, goals added, xPass for all seasons",
      "Must run after every ASA pull — rebuilds all rolling features",
      "Only needed after significant new data; models persist in data/processed/models.rds",
      "Auto-populates slate with last-game minutes as default",
      "Set after lineup news — word-boundary matching, use exact last names",
      "Optionally anchors team goal totals to external calibration signal; saves Excel to output/",
      "Check game environment columns and projection values for context",
      "Run log_outcomes() the day after games finish (ASA updates ~24h after)"
    ),
    check.names = FALSE
  )
  doc <- tbl(doc, workflow, col_widths = c(0.3, 1.2, 2.2, 2.8))

  doc <- doc %>%
    br()

  # ═══════════════════════════════════════════════════════════════════════════
  # SECTION 7: LIMITATIONS
  # ═══════════════════════════════════════════════════════════════════════════

  doc <- doc %>%
    h1("7. Limitations & Known Risks") %>%
    p("1. Lineup dependency: projections are only as good as the minutes inputs. Without confirmed lineups, the model uses last-game defaults which can be significantly wrong for rotated or injured players. The lineup window (confirmed lineup to kickoff) is the primary timing constraint.") %>%
    br() %>%
    p("2. Goals model: the raw goals model has near-zero Brier skill standalone. Its value is in ranking players within a team — who converts more of their shots. The team-total calibration step corrects the absolute level. Treat the raw goal lambda as a within-team ranking signal, not an absolute rate, unless calibration has been applied.") %>%
    br() %>%
    p("3. New players: players with no ASA game history are imputed using position-group medians. These projections are significantly less reliable. Flag imputed players in the output (shown in console during run).") %>%
    br() %>%
    p("4. Model strength ranking: Shots (0.385) > Shots on Target (0.303) > Assists (0.101) > Goals (0.001) by Brier skill. Shots and shots-on-target projections carry the highest standalone confidence.") %>%
    br() %>%
    p("5. Accuracy tracking requires volume: do not draw conclusions from fewer than 5 game weeks of logged data. A single week of results is noise, not signal.") %>%
    br() %>%
    p("6. External calibration signal availability: the team-total calibration step depends on an external data source being available and the team names matching via crosswalk. When unavailable, the model falls back to its own raw team-level goal sum with no external anchor — flagged clearly in the console output.") %>%
    br()

  # ── save ─────────────────────────────────────────────────────────────────────
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  print(doc, target = out_path)
  message("Documentation saved: ", out_path)
  system(paste("open", shQuote(out_path)))
}

# Run (from the project root):
# source("R/09_documentation.R")
# build_documentation()
