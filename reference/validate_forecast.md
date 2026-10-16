# Validate forecast submission

This function will take the prepped forecast data from
[format_for_submission](https://signaturescience.github.io/fiphde/reference/format_for_submission.md)
and run a series of tests to validate the format.

## Usage

``` r
validate_forecast(subdat)
```

## Arguments

- subdat:

  A `tibble` with submission ready forecasts prepped by and stored in
  output of
  [format_for_submission](https://signaturescience.github.io/fiphde/reference/format_for_submission.md)

## Value

Named list with elements for each test (including logical for whether or
not test passed and message if failed) and an overall "valid" logical
with `TRUE` if all tests passed an `FALSE` if at least one failed

## Examples

``` r
if (FALSE) { # \dontrun{
# Get raw data from healthdata.gov
h_raw <- get_hdgov_hosp(limitcols=TRUE)

# Prep, and make a tsibble
prepped_hosp <- prep_hdgov_hosp(h_raw, statesonly=TRUE)
prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
                                     epiyear = epiyear,
                                     epiweek=epiweek,
                                     key=location)
# Limit to only Virginia and US
prepped_hosp_tsibble <-
  prepped_hosp_tsibble %>%
  dplyr::filter(location %in% c("US", "51"))

# Fit a model
hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               covariates=TRUE)



# Format for submission
formatted_list <- format_for_submission(hosp_fitfor$tsfor, method = "ts", format = "legacy")
# Validate one of the forecasts
# Note that this expects forecast is prepared with forecast date = Monday of the current week
ens_forc <- formatted_list$ensemble
ens_forc$forecast_date <- this_monday()
validate_forecast(ens_forc)
} # }
```
