# ══════════════════════════════════════════════════════════════════════════════
# MLS PLAYER PROJECTIONS — WEEKLY RUNBOOK
# Copy and paste each block as needed. Run from the project root
# (open the repo folder in RStudio / set your working directory there first).
# ══════════════════════════════════════════════════════════════════════════════

# ── STEP 1: PULL FRESH DATA (once per week, Monday or Tuesday) ────────────────

source("R/00_setup.R")
source("R/01_pull_asa.R")
pull_all_asa(seasons = c("2023", "2024", "2025", "2026"))

# ── STEP 2: REBUILD FEATURES (after every data pull) ─────────────────────────

source("R/03_feature_engineering.R")
model_data <- build_model_dataset()

# ── STEP 3: RETRAIN MODELS (only needed after rebuilding features) ────────────

source("R/04_model.R")
models <- train_all_models()

# ── STEP 4: BUILD SLATE (once per week — populates with last-game minutes) ───

source("R/06_build_slate.R")
build_slate()

# ── STEP 5: FIRST PROJECTION RUN (before lineups — use as reference) ─────────

source("R/05_projections.R")
proj <- run_daily_projections()

# ── STEP 6: SET LINEUPS (when confirmed — run one block per team) ─────────────
# Replace names with actual starters/subs for that game
# Names are matched by last name — use exact spelling from the output

source("R/06_build_slate.R")

auto_lineup("Vancouver Whitecaps FC",
            starters = c("Takaoka", "Priso", "Laborda", "Ocampo",
                         "Cubas", "Berhalter", "Jackson", "White",
                         "Sabbi", "Badwal"),
            subs     = c("Muller", "Sabaly", "Larraz", "Johnson"))

# Repeat for each team on the slate:
# auto_lineup("Team Name",
#             starters = c("..."),
#             subs     = c("..."))

# ── STEP 7: FINAL PROJECTION RUN (after lineups set) ─────────────────────────

source("R/05_projections.R")
proj <- run_daily_projections()

# Output saved to: output/projections_YYYY-MM-DD.xlsx
# Open it:
system("open output/")

# ── STEP 8: LOG PROJECTIONS (immediately after final run) ─────────────────────

source("R/08_backtest.R")
log_projections(proj)

# ── STEP 9: LOG OUTCOMES (day after games — ASA updates ~24h later) ───────────

source("R/08_backtest.R")
log_outcomes(game_date = as.Date("2026-03-15"))   # ← change date each time

# ── STEP 10: CHECK BACKTEST STATUS (anytime) ──────────────────────────────────

source("R/08_backtest.R")
backtest_summary()

# ── OPTIONAL: TRACK PROJECTION ACCURACY (after 5+ weeks of logged data) ───────

source("R/08_backtest.R")
track_projection_accuracy()

# ── OPTIONAL: DEV FEATURE TEST (before promoting any new model features) ───────

source("R/08_backtest.R")
result <- compare_model_features()
# Only promote if result$promote == TRUE

# ── OPTIONAL: REBUILD DOCUMENTATION ───────────────────────────────────────────

source("R/09_documentation.R")
build_documentation()
