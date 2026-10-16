# Format time series forecast

This function specifically formats time series forecasts generated with
[ts_fit_forecast](https://signaturescience.github.io/fiphde/reference/ts_fit_forecast.md)
to adhere to probabilistic forecast submission guidelines for consortia
such as FluSight. It is used as a helper in
[format_for_submission](https://signaturescience.github.io/fiphde/reference/format_for_submission.md).

## Usage

``` r
ts_format_for_submission(
  tsfor,
  .target = "wk ahead inc flu hosp",
  .counts = TRUE
)
```

## Arguments

- tsfor:

  The forecast from
  [ts_fit_forecast](https://signaturescience.github.io/fiphde/reference/ts_fit_forecast.md)

- .target:

  Name of the target in the forecast; default is
  `"wk ahead inc flu hosp"`

- .counts:

  Logical; default `TRUE` indicates that the target outcome is a count,
  and should be rounded off at an integer

## Value

A named list of tibbles with probabilistic forecasts (one for each
model), formatted for submission with the following columns:

- **forecast_date**: Date of forecast

- **target**: Horizon and name of forecasted target

- **target_end_date**: Last date of the forecasted target (e.g.,
  Saturday of the given epidemiological week)

- **location**: FIPS code for location

- **type**: One of either "point" or "quantile" for the forecasted value

- **quantile**: The quantile for the forecasted value; `NA` if "type" is
  `"point"`

- **value**: The forecasted value

## Details

Uses quantiles `c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)`
in the built-in `fiphde:::q`, using an accessory table `fiphde:::quidk`.

## References

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
formatted_list <- ts_format_for_submission(hosp_fitfor$tsfor)
formatted_list
} # }
```
