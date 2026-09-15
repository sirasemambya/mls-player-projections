# 07_market_calibration.R
# Pulls external market-implied team goal totals and backs out expected
# goals (lambda) per team via Poisson fitting.
#
# This is used as an anchor for the model's raw goal projections: markets
# aggregate a huge amount of information (injuries, tactics, public and
# sharp money) into a single number, so scaling the model's team-level
# goal sum to match that number is a standard calibration technique —
# it corrects the model's absolute level while preserving its ranking of
# which players on a team are more likely to score.
#
# Data source: The Odds API (https://the-odds-api.com)
# Sign up for a free API key, then add it to your .Renviron so it's
# never hardcoded:
#   usethis::edit_r_environ()  → add line: ODDS_API_KEY=your_key_here
# Free tier: 500 requests/month — more than enough for a weekly MLS slate

library(httr)
library(jsonlite)
library(tidyverse)

get_odds_key <- function() {
  key <- Sys.getenv("ODDS_API_KEY")
  if (key == "") stop("Set ODDS_API_KEY in your .Renviron file")
  key
}

ODDS_BASE  <- "https://api.the-odds-api.com/v4"
MLS_SPORT  <- "soccer_usa_mls"

# ── Throttled GET with 429 retry ──────────────────────────────────────────────

market_get <- function(url, query, pause = 0.5, max_retries = 3) {
  for (i in seq_len(max_retries)) {
    resp <- GET(url, query = query)
    if (status_code(resp) != 429) return(resp)
    wait <- 5 * i
    message("  Rate limited — waiting ", wait, "s (attempt ", i, "/", max_retries, ")")
    Sys.sleep(wait)
  }
  resp  # return last response even if still 429
}

# ── Pull and parse team total lines ───────────────────────────────────────────
# Team totals are only available on the event-specific endpoint

pull_team_totals <- function(providers = "pinnacle,circa") {

  # Step 1: get event IDs for upcoming MLS games
  events_resp <- GET(
    paste0(ODDS_BASE, "/sports/", MLS_SPORT, "/events/"),
    query = list(apiKey = get_odds_key())
  )

  if (status_code(events_resp) != 200) {
    stop("Events API error: ", status_code(events_resp), " — ",
         content(events_resp, "text"))
  }

  events <- fromJSON(content(events_resp, "text", encoding = "UTF-8"),
                     flatten = TRUE)

  if (length(events) == 0 || nrow(events) == 0) {
    message("No upcoming MLS events found")
    return(NULL)
  }

  message("Found ", nrow(events), " upcoming MLS events — pulling team totals...")

  # Step 2: for each event pull team_totals and parse inline
  last_remaining <- NULL

  lines <- map_dfr(events$id, function(event_id) {
    tryCatch({
      resp <- market_get(
        paste0(ODDS_BASE, "/sports/", MLS_SPORT, "/events/", event_id, "/odds/"),
        query = list(
          apiKey     = get_odds_key(),
          regions    = "us",
          markets    = "team_totals",
          bookmakers = providers,
          oddsFormat = "american"
        )
      )

      last_remaining <<- headers(resp)[["x-requests-remaining"]]

      if (status_code(resp) != 200) {
        message("  Skipping event ", event_id, ": ", status_code(resp))
        return(NULL)
      }

      ev <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)

      home_team <- ev$home_team
      away_team <- ev$away_team
      game_time <- as.POSIXct(ev$commence_time,
                              format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

      sources <- ev$bookmakers
      if (is.null(sources) || nrow(sources) == 0) return(NULL)

      map_dfr(seq_len(nrow(sources)), function(j) {
        source_key <- sources$key[j]
        markets    <- sources$markets[[j]]
        if (is.null(markets) || nrow(markets) == 0) return(NULL)

        tt <- markets %>% filter(key == "team_totals")
        if (nrow(tt) == 0) return(NULL)

        outcomes <- tt$outcomes[[1]]
        if (is.null(outcomes) || nrow(outcomes) == 0) return(NULL)

        outcomes %>%
          transmute(
            game_time     = game_time,
            home_team     = home_team,
            away_team     = away_team,
            source        = source_key,
            team          = description,
            direction     = name,
            line          = point,
            american_odds = price
          )
      })
    }, error = function(e) {
      message("  Skipping event ", event_id, " (parse error): ", e$message)
      NULL
    })
  })

  if (!is.null(last_remaining)) {
    message("API requests remaining this month: ", last_remaining)
  }

  if (is.null(lines) || nrow(lines) == 0) {
    message("No MLS team total markets available right now")
    return(NULL)
  }

  lines
}

# ── Convert american odds → implied probability ───────────────────────────────

american_to_implied <- function(odds) {
  ifelse(
    odds > 0,
    100 / (odds + 100),
    abs(odds) / (abs(odds) + 100)
  )
}

# ── Fit Poisson λ from team total Over lines ──────────────────────────────────
# Given multiple Over lines (Over 0.5, Over 1.5, Over 2.5) with implied probs,
# find the Poisson λ that best fits P(X > k) for each k

fit_poisson_lambda <- function(lines_df) {
  # lines_df: rows with columns line (0.5, 1.5, 2.5) and implied_prob (of Over)
  # P(Poisson(λ) > k) = 1 - ppois(k, λ)  where k = floor(line)

  objective <- function(lambda) {
    predicted <- 1 - ppois(floor(lines_df$line), lambda)
    sum((predicted - lines_df$implied_prob)^2)
  }

  opt <- optimize(objective, interval = c(0.01, 8))
  opt$minimum
}

# ── Build implied team goals per team ─────────────────────────────────────────

get_implied_team_goals <- function(providers = "pinnacle") {

  message("Pulling team total lines...")
  raw <- pull_team_totals(providers)
  if (is.null(raw)) return(NULL)

  lines <- raw

  # Remove vig: use only Over lines, average across sources
  over_lines <- lines %>%
    filter(direction == "Over") %>%
    mutate(implied_prob = american_to_implied(american_odds)) %>%
    group_by(game_time, home_team, away_team, team, line) %>%
    summarise(
      implied_prob = mean(implied_prob, na.rm = TRUE),   # avg across sources
      .groups      = "drop"
    )

  # Fit λ per team per game
  implied_goals <- over_lines %>%
    group_by(game_time, home_team, away_team, team) %>%
    group_modify(~ {
      if (nrow(.x) < 1) return(tibble(implied_lambda = NA_real_))
      tibble(implied_lambda = fit_poisson_lambda(.x))
    }) %>%
    ungroup()

  message("Implied team goals:")
  print(implied_goals %>%
    mutate(matchup = paste0(home_team, " vs ", away_team)) %>%
    select(matchup, team, implied_lambda) %>%
    arrange(matchup))

  implied_goals
}

# ── ESPN name → common name crosswalk (same as 06_build_slate.R) ──────────────

ESPN_TO_COMMON <- c(
  "LAFC"                = "Los Angeles FC",
  "Red Bull New York"   = "New York Red Bulls",
  "Portland Timbers"    = "Portland Timbers FC",
  "St. Louis CITY SC"   = "St. Louis City SC",
  "Vancouver Whitecaps" = "Vancouver Whitecaps FC"
)

normalize_team_name <- function(name) {
  ifelse(name %in% names(ESPN_TO_COMMON), ESPN_TO_COMMON[name], name)
}

# To run standalone (from the project root):
# source("R/00_setup.R")
# source("R/07_market_calibration.R")
# implied <- get_implied_team_goals()
