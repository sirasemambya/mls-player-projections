# 00_setup.R
# Install and load all dependencies for the MLS player projection model

packages <- c(
  "tidyverse",       # data wrangling
  "worldfootballR",  # FBref scraper
  "httr",            # ASA API calls
  "jsonlite",        # parse JSON responses
  "glmmTMB",         # Negative Binomial + Poisson hierarchical models
  "lme4",            # mixed effects (minutes model)
  "lubridate",       # date handling
  "zoo",             # rolling averages
  "gt",              # clean projection tables
  "stringi"          # name encoding fix (Latin → ASCII)
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

lapply(packages, install_if_missing)
lapply(packages, library, character.only = TRUE)

# worldfootballR is on GitHub — install via remotes if missing
if (!requireNamespace("worldfootballR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }
  remotes::install_github("JaseZiv/worldfootballR")
  library(worldfootballR)
}

message("All packages loaded.")
