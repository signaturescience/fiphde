## validation test
library(tidyverse)
## inspired by tests in zoltpy ...
## which we called for C19FH submission validation
## https://github.com/reichlab/zoltpy/blob/6b2bc2444ea95898b212a586f144dfdf3cc842b0/zoltpy/covid19.py
## https://github.com/signaturescience/focustools/blob/master/R/submission.R#L1-L49

## read in an example submission file
subdat <- read_csv("submission/SigSci-CREG/2022-01-05-SigSci-CREG.csv")
## hack to force to monday to get this to validate completely ...
# subdat$forecast_date <- this_monday()
## otherwise we know the target end date expectation will fail because forecast date is wed
## see tests below

## get the flusight "config" which presumably will be used by their automated test suite
fsconfig <- jsonlite::read_json("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/project-config.json")

## create list to store validation results
valres <-
  list(
    locations = NA,
    targets = NA,
    values = NA,
    quantiles = NA,
    point = NA,
    date_format = NA,
    end_sat = NA,
    expect_sat = NA
  )

## now step through each check

## locations must be in list of location names
allowed_locs <- unlist(fsconfig$target_groups[[1]]$locations)
tst_loc <- all(subdat$location %in% allowed_locs)

if(tst_loc) {
  valres$locations <- list(valid = TRUE, msg = NULL)
} else {
  valres$locations <- list(valid = FALSE, msg = "The submission includes invalid locations.")
}

## targets must be in list of target names
req_targets <-
  unlist(fsconfig$target_groups[[1]]$targets) %>%
  sort() %>%
  paste0(., collapse = ",")

tst_targ <-
  subdat %>%
  group_by(location) %>%
  distinct(target, .keep_all = TRUE) %>%
  summarise(targets = paste0(target, collapse = ","), .groups = "drop") %>%
  mutate(ind = targets == req_targets) %>%
  pull(ind) %>%
  all(.)

if(tst_targ) {
  valres$targets <- list(valid = TRUE, msg = NULL)
} else {
  valres$targets <- list(valid = FALSE, msg = "The submission includes invalid targets.")
}


## value must be non-negative
tst_values <- all(subdat$value >= 0)

if(tst_values) {
  valres$values <- list(valid = TRUE, msg = NULL)
} else {
  valres$values <- list(valid = FALSE, msg = "The submission includes negative values.")
}

## quantiles must be in valid quantile
req_quants <-
  unlist(fsconfig$target_groups[[1]]$quantiles) %>%
  round(., 3) %>%
  sort() %>%
  paste0(., collapse = ",")

tst_quants <-
  subdat %>%
  filter(type == "quantile") %>%
  group_by(location,target) %>%
  arrange(quantile) %>%
  summarise(quants = paste0(quantile, collapse = ","), .groups = "drop") %>%
  mutate(ind = quants == req_quants) %>%
  pull(ind) %>%
  all(.)

if(tst_quants) {
  valres$quantiles <- list(valid = TRUE, msg = NULL)
} else {
  valres$quantiles <- list(valid = FALSE, msg = "The submission does not include all required quantiles or includes invalid quantiles.")
}

## point type must have NA value for quantile
tst_point <-
  subdat %>%
  filter(type == "point") %>%
  pull(quantile) %>%
  is.na() %>%
  all()

if(tst_point) {
  valres$point<- list(valid = TRUE, msg = NULL)
} else {
  valres$point <- list(valid = FALSE, msg = "The submission includes something other than 'NA' for quantile of point estimates.")
}

## date columns are in YYYY-mm-dd format
## doesnt need to be perfect
## just needs to make sure this is either 2021 or 2022 ...
## ... to check that the dates are formatted right
tst_date_format <-
  all(c(lubridate::year(strptime(subdat$forecast_date, format = "%Y-%m-%d")) %in% c(2021,2022),
        lubridate::year(strptime(subdat$target_end_date, format = "%Y-%m-%d")) %in% c(2021,2022)))

if(tst_date_format) {
  valres$date_format <- list(valid = TRUE, msg = NULL)
} else {
  valres$date_format <- list(valid = FALSE, msg = "The submission includes something other than 'NA' for quantile of point estimates.")
}

## end_date is a saturday
tst_end_sat <-
  all(weekdays(subdat$target_end_date, abbreviate = FALSE) == "Saturday")

if(tst_end_sat) {
  valres$end_sat <- list(valid = TRUE, msg = NULL)
} else {
  valres$end_sat <- list(valid = FALSE, msg = "The submission target end dates column includes date(s) that are not Saturday.")
}

forecast_day <- unique(weekdays(subdat$forecast_date))

## check that only one forecast date is specified
# length(forecast_day) == 1

## if forecast date is monday/sunday then expected end date is sat of week * horizon
## if forecast date is tuesday then expected end date is sat of week * 1 + horizon

tst_expect_sat <-
  subdat %>%
  tidyr::separate(target, into = c("horizon", "drop"), sep = " wk ", remove = FALSE) %>%
  select(-drop) %>%
  mutate(forecast_day = forecast_day) %>%
  ## this subtraction of horizon - 1 will make sure that when we multiply by 7 days ...
  ## ... we dont add any days to the current week (if the forecast day is sun or mon)
  mutate(horizon_days = ifelse(forecast_day %in% c("Sunday","Monday"), (as.numeric(horizon) - 1)*7, as.numeric(horizon) * 7)) %>%
  ## ceiling date will give you the sunday
  mutate(expected_sat = lubridate::ceiling_date(forecast_date + horizon_days, "weeks")-1) %>%
  mutate(ind = target_end_date == expected_sat) %>%
  pull(ind) %>%
  all(.)

if(tst_expect_sat) {
  valres$expect_sat <- list(valid = TRUE, msg = NULL)
} else {
  valres$expect_sat <- list(valid = FALSE, msg = "The submission target end dates do not line up with expected Saturdays by horizon. Note if submission forecast date is not Sunday or Monday, then forecasts are assumed to to start the following week.")
}

## are all conditions valid ??
valres$valid <- all(map_lgl(valres, "valid"))
valres$valid
