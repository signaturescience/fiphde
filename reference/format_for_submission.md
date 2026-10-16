# Format forecasts for submission

This function prepares forecasts to adhere to probabilistic forecast
submission guidelines for consortia such as FluSight.

## Usage

``` r
format_for_submission(
  .forecasts,
  method = "ts",
  .target = "wk ahead inc flu hosp",
  format = "hubverse",
  horizon_shift = 1
)
```

## Arguments

- .forecasts:

  Forecasts to be formatted for submission; if method is `"ts"` this
  should be forecasts from
  [ts_fit_forecast](https://signaturescience.github.io/fiphde/reference/ts_fit_forecast.md);
  otherwise this must be a `tibble` with forecast output (e.g., output
  from
  [glm_forecast](https://signaturescience.github.io/fiphde/reference/glm_forecast.md))
  with a column designating "location"

- method:

  Method for forecasting; default is `"ts"` which will trigger the use
  of
  [ts_format_for_submission](https://signaturescience.github.io/fiphde/reference/ts_format_for_submission.md)
  internally

- .target:

  Name of the target in the forecast; default is
  `"wk ahead inc flu hosp"`

- format:

  The submission format to be used; must be one of `"hubverse"` or
  `"legacy"`; default is `"hubverse"`

- horizon_shift:

  Number of horizons to shift backwards to align with reference date;
  only used if format is `"hubverse"`; default is `1`

## Value

A named list of tibbles with probabilistic forecasts (one for each
model), formatted for submission.

If format is `"hubverse"` each tibble will have the following columns:

- **reference_date**: Date of reference for forecast submission

- **horizon**: Horizon for the given forecast

- **target**: Name of forecasted target

- **target_end_date**: Last date of the forecasted target (e.g.,
  Saturday of the given epidemiological week)

- **location**: Name or geographic identifier (e.g., FIPS code) for
  location for the given forecast

- **output_type**: Type of forecasted value (e.g., "quantile")

- **output_type_id**: The quantile for the forecasted value if
  output_type is "quantile"

- **value**: The forecasted value

If format is `"legacy"` each tibble will have the following columns:

- **forecast_date**: Date of forecast

- **target**: Horizon and name of forecasted target

- **target_end_date**: Last date of the forecasted target (e.g.,
  Saturday of the given epidemiological week)

- **location**: Name or geographic identifier (e.g., FIPS code) for
  location for the given forecast

- **type**: One of either "point" or "quantile" for the forecasted value

- **quantile**: The quantile for the forecasted value; `NA` if "type" is
  `"point"`

- **value**: The forecasted value

## References

<https://github.com/signaturescience/FluSight-forecast-hub/tree/main/model-output#forecast-file-format>

<https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-forecasts/README.md>

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
formatted_list
} # }
```
