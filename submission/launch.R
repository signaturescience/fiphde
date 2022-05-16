library(fiphde)

## NOTE: submission_dir and app_dir may need to be reset to run locally
#submission_dir <- here::here("submission")
submission_dir <- "/submission/"

prepped_hosp <-
  get_hdgov_hosp(limitcols = TRUE) %>%
  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
  dplyr::filter(abbreviation != "DC")

fiphde_launcher(.data = prepped_hosp,
                submission_dir = submission_dir,
                host = "0.0.0.0",
                launch.browser = TRUE,
                port = 80)
