library(fiphde)
library(tidyverse)

submission_dir <- here::here("submission")
## NOTE: using here::here("submission") may not work in automated pipeline app refresh
# submission_dir <- "/submission/"

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
                appDir = here::here("inst/app"),
                port = 3838)
