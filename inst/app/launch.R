library(fiphde)
library(tidyverse)

## NOTE: submission_dir and app_dir may need to be reset to run locally
#submission_dir <- here::here("submission")
submission_dir <- "/submission/"
#app_dir <- here::here("inst/app")
app_dir <- "/src/explorer"

prepped_hosp <-
  get_hdgov_hosp(limitcols = TRUE) %>%
  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
  dplyr::filter(abbreviation != "DC")


fiphde_launcher <- function(.data, submission_dir,...) {
  ## attach these values from the args objects to the global env ...
  ## will be visible to running shiny app
  .GlobalEnv$.data <- .data
  .GlobalEnv$.submission_dir <- submission_dir
  ## side effect to clean up
  on.exit(rm(.submission_dir, .data, envir = .GlobalEnv))
  shiny::runApp(...)
}

fiphde_launcher(.data = prepped_hosp,
                submission_dir = submission_dir,
                host = "0.0.0.0",
                appDir = app_dir,
                launch.browser = TRUE,
                port = 80)
