# Forecast categorical targets

This function takes probabilistic flu hospitalization forecast input and
converts the forecasted values for each location to a categorical
"change" indicator. The criteria for each level ("large decrease",
"decrease", "stable", "increase", "large increase") was defined by the
CDC (see link in references). The algorithm evaluates absolute changes
in counts and rates (per 100k individuals) for the most recently
observed week and a 2 week ahead forecasted horizon. This procedure runs
independently for each location, and results in a formatted tabular
output that includes each possible level and its corresponding
probability of being observed (calculated from probabilistic quantiles)
for every location.

## Usage

``` r
forecast_categorical(
  .forecast,
  .observed,
  method = "density",
  format = "hubverse",
  horizon = 4
)
```

## Arguments

- .forecast:

  A tibble with "submission-ready" probabilistic flu hospitalization
  forecast data (i.e., tibble contained in list element returned from
  [format_for_submission](https://signaturescience.github.io/fiphde/reference/format_for_submission.md))

- .observed:

  A tibble with observed flu admission data (i.e., tibble output from
  [prep_hdgov_hosp](https://signaturescience.github.io/fiphde/reference/prep_hdgov_hosp.md))

- method:

  The categorical forecasting method to use; must be one of `"density"`
  or `"interpolation"`; default is `"density"`

- format:

  The submission format to be used; must be one of `"hubverse"` or
  `"legacy"`; default is `"hubverse"`

- horizon:

  The number of horizons ahead to forecast; must be one of `4` or `5`;
  default is `4`

## Value

A `tibble` with formatted categorical forecasts.

If format is `"hubverse"` the tibble will have the following columns:

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

If format is `"legacy"` the tibble will have the following columns:

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

<https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-experimental/README.md>

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve hospitalization data
h_raw <- get_hdgov_hosp(limitcols=TRUE)
# Prepare and summarize hospitalization data to weekly resolution
prepped_hosp <- prep_hdgov_hosp(h_raw)
# Create a keyed time series tibble with only locations of interest
prepped_tsibble <- make_tsibble(prepped_hosp,
                                     epiyear = epiyear,
                                     epiweek=epiweek,
                                     key=location)

# Run with default constrained ARIMA, nonseasonal ETS, no NNETAR
hosp_fitfor <- ts_fit_forecast(prepped_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               covariates=TRUE)
# Prepare forecast for quantile submission format
forc <- format_for_submission(hosp_fitfor$tsfor, method = "ts", format = "legacy")
# Run categorical summary of quantiles for the time series ensemble
forecast_categorical(forc$ensemble, prepped_hosp, method = "interpolation", format = "legacy")
} # }
```
