# MLS Player Performance Projection System

A statistical modeling pipeline that projects expected shots, shots on target, goals, and assists for MLS players ahead of upcoming matches. Built in R using hierarchical generalized linear mixed models (glmmTMB / lme4) trained on player-game tracking data from American Soccer Analysis and FBref.

The goal is not to predict any single game perfectly. Individual match outcomes are noisy. The goal is projections that are well calibrated across a full season of player-games, so relative comparisons (who is trending up, who has a favorable matchup this week, how a player's underlying output compares to their raw box score) are trustworthy even when any one number is off.

## What it does

For a given slate of upcoming MLS games, the pipeline:

1. Pulls player-game level tracking data (xG, xA, shots, key passes, goals added, xPass, minutes) from American Soccer Analysis, and season/match-log stats from FBref.
2. Builds decay-weighted rolling features (last 10 games, exponential decay, alpha = 0.85) so the model responds to recent form, not season-long averages.
3. Builds a position-specific opponent defensive profile for every team. A striker's opponent difficulty is measured against that team's record against strikers specifically, not their overall record.
4. Trains four models: shots (Negative Binomial), shots on target rate (Binomial, conditional on shots), goals (Poisson, conditional on shots), and assists (Zero-Inflated Poisson).
5. Generates a projection for every player on the slate, with playing time set manually from confirmed lineups.
6. Optionally anchors team-level goal totals to an external market-implied signal for that game's expected scoring environment, then scales individual goal projections proportionally so the team sum matches. This step is optional and the model runs fine without it.
7. Exports a formatted Excel workbook and can log projections against actual outcomes over time to track calibration.

## Data sources

| Source | What it provides | Access |
|---|---|---|
| [American Soccer Analysis API](https://www.americansocceranalysis.com/) | Player and team xG, xA, shots, goals added, xPass, games, at the game level. Primary data source, no key required. | Public REST API |
| [FBref](https://fbref.com/) via [worldfootballR](https://github.com/JaseZiv/worldfootballR) | Season and match-log summary/passing stats, cached community data | Public, via R package |
| [ESPN scoreboard API](https://www.espn.com/) | Upcoming MLS and CONCACAF Champions Cup schedule, used to auto-build the weekly slate | Public REST API |
| The Odds API (optional) | External market-implied team goal totals, used only to calibrate the game-level scoring environment | Requires a free API key, stored in `.Renviron`, never committed |

No player prop or betting market data is pulled, stored, or displayed anywhere in this pipeline. The one external market signal used is a team-level total, applied purely as a calibration input for the scoring environment.

## Pipeline order

Scripts are numbered and meant to run in order. Everything reads and writes relative paths from the project root.

| Script | Purpose |
|---|---|
| `R/00_setup.R` | Installs and loads all package dependencies |
| `R/01_pull_asa.R` | Pulls player and team data from American Soccer Analysis |
| `R/02_pull_fbref.R` | Pulls supplementary stats from FBref |
| `R/03_feature_engineering.R` | Builds the modeling dataset: rolling features, opponent defensive profiles, game context |
| `R/04_model.R` | Trains the four projection models and saves them |
| `R/06_build_slate.R` | Auto-builds the upcoming slate from the ESPN schedule; helper functions to set confirmed lineup minutes |
| `R/05_projections.R` | Generates projections for the current slate, applies calibration, exports Excel |
| `R/07_market_calibration.R` | Pulls the external market-implied team goal signal (called internally by `05_projections.R`) |
| `R/08_backtest.R` | Calibration backtest (Brier scores, MAE/RMSE), ongoing projection accuracy tracking, and A/B testing for candidate features |
| `R/09_documentation.R` | Generates a Word document with full technical documentation |

`RUNBOOK.R` is a copy-paste weekly checklist that walks through the full cycle: pull data, rebuild features, retrain (if needed), build the slate, set lineups, run projections, and log for accuracy tracking.

## How to run it

Clone the repo, open it as your R working directory (or set one up as an RStudio project), then:

```r
source("R/00_setup.R")
source("R/01_pull_asa.R"); pull_all_asa(seasons = c("2023", "2024", "2025", "2026"))
source("R/03_feature_engineering.R"); model_data <- build_model_dataset()
source("R/04_model.R"); models <- train_all_models()
source("R/06_build_slate.R"); build_slate()
```

Open `data/slate_today.xlsx`, fill in confirmed minutes for each player (or use `auto_lineup()` to estimate them from recent history), then:

```r
source("R/05_projections.R")
proj <- run_daily_projections()
```

This exports a formatted Excel workbook to `output/` and prints a summary table. To use the optional market calibration step, set `ODDS_API_KEY` in your `.Renviron` first (`usethis::edit_r_environ()`); without a key the pipeline still runs, just without that calibration input.

## Key modeling choices

- **Four separate models instead of one.** Shots, shots-on-target rate, goals, and assists behave differently (overdispersed counts, a bounded rate, a rare event conditional on volume, and a heavily zero-inflated count), so each gets the distribution that fits it: Negative Binomial, Binomial, Poisson, and Zero-Inflated Poisson respectively.
- **Decay-weighted rolling windows, not season averages.** A last-10-games window with exponential decay (alpha = 0.85) makes the model responsive to a player's current form and role, which changes over a season.
- **Position-specific opponent defense.** Team-level defensive rating hides a lot: a team can be excellent against wingers and poor against strikers. The model profiles each team's defense separately by position group.
- **Player random effects.** Each model includes a `(1 | player_id)` random intercept so individual tendencies beyond the observed features are captured, rather than assuming every player with the same stat line performs identically going forward.
- **Feature promotion requires an A/B test.** New candidate features (currently: receiving goals added, expected-pass differential) are only added to production models if they improve out-of-sample Brier skill on every modeled stat, not just one, via `compare_model_features()`.

## Sample output

`run_daily_projections()` produces a table like this (illustrative values):

| player | team | opp | mins | position | team_total | goals | assists | shots_ot | shots | g_or_a |
|---|---|---|---|---|---|---|---|---|---|---|
| J. Alvarado | Austin FC | Nashville SC | 90 | ST | 1.65 | 0.41 | 0.09 | 1.12 | 2.87 | 0.47 |
| M. Torres | Austin FC | Nashville SC | 80 | AM | 1.65 | 0.19 | 0.24 | 0.71 | 1.65 | 0.41 |
| K. Boakye | Nashville SC | Austin FC | 90 | W | 1.20 | 0.22 | 0.15 | 0.83 | 1.94 | 0.35 |
| D. Whitfield | Nashville SC | Austin FC | 75 | ST | 1.20 | 0.31 | 0.05 | 0.68 | 1.58 | 0.35 |

`team_total` is the calibrated expected goals for that player's team in this game. `goals`, `assists`, `shots_ot`, and `shots` are the player's own projected counts. `g_or_a` is the projected probability the player records at least one goal or assist. The full export also includes a game-environment section (season scoring averages, opponent defensive rank, xG over/under-performance) alongside these.

## Limitations

- **Lineup dependency.** Projections are only as good as the minutes input. Without a confirmed lineup, the model defaults to last game's minutes, which can be meaningfully wrong for rotated or injured players.
- **Goals model is weak standalone.** Its out-of-sample Brier skill is close to zero on its own; its value is ranking which players on a team are more likely to convert, with the team-level calibration step correcting the absolute rate.
- **New players are imputed.** Players with no game history in the dataset get position-group median features, which is a much less reliable projection than one built on real observed form.
- **Model strength varies by stat.** Out-of-sample Brier skill ranks shots highest, then shots on target, then assists, then goals. Trust the shot-volume projections more than the scoring projections in isolation.
- **External calibration is optional and can silently fall back.** If the market-implied signal is unavailable or team names fail to match, the pipeline falls back to the model's own raw team-level goal sum. This is logged to the console but easy to miss if you're not watching it.
- **Small, mostly single-user pipeline.** There is no automated test suite. Correctness is currently verified through the calibration backtest and manual review of weekly output, not unit tests.

## What I'd build next

- A proper test suite around the feature engineering and projection functions, so a schema change in an upstream API doesn't silently break a rolling feature.
- Injury and suspension status pulled automatically rather than set by hand in the slate.
- A rolling accuracy dashboard (the pieces exist in `R/08_backtest.R`, but there's no visualization layer yet) so calibration drift over a season is visible at a glance instead of read out of console output.
- Extending the position-specific opponent profile to account for a team's specific formation and pressing scheme, not just aggregate position-group results.
- A lightweight web interface for browsing projections by team or player without opening Excel.
