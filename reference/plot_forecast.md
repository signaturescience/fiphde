# Plot forecasts

This function serves as a plotting mechanism for prepped forecast
submission data. The plots show the historical trajectory of the truth
data supplied along with the forecasted point estimates and (optionally)
the prediction interval. All plots are faceted by location.

Note that the ".data" and "submission" arguments to this function expect
incoming data prepared in a certain format. See the argument
documentation and "Details" for more information.

## Usage

``` r
plot_forecast(
  .data,
  submission,
  location = "US",
  pi = 0.95,
  .model = NULL,
  .outcome = "flu.admits",
  format = "legacy"
)
```

## Arguments

- .data:

  A data frame with historical truth data for all locations and outcomes
  in submission targets

- submission:

  Formatted submission (e.g., a `tibble` containing forecasts prepped
  with
  [format_for_submission](https://signaturescience.github.io/fiphde/reference/format_for_submission.md))

- location:

  Vector specifying locations to filter to; `'US'` by default.

- pi:

  Width of prediction interval to plot; default is `0.95` for 95% PI; if
  set to `NULL` the PI will not be plotted

- .model:

  Name of the model used to generate forecasts; default is `NULL` and
  the name of the model will be assumed to be stored in a column called
  "model" in formatted submission file

- .outcome:

  The name of the outcome variable you're plotting in the historical
  data; defaults to `"flu.admits"`

- format:

  The submission format to be used; must be one of `"hubverse"` or
  `"legacy"`; default is `"legacy"`

## Value

A `ggplot2` plot object with line plots for outcome trajectories faceted
by location

## Details

To plot the forecasted output alongside the observed historical data,
both the ".data" and "submission" data must be prepared at the same
geographic and temporal resolutions. The data frame passed to ".data"
must include the column specified in the ".outcome" argument as well as
the following columns:

- **location**: FIPS location code

- **week_end**: Date of the last day (Saturday) in the given
  epidemiological week

If format is "legacy" the "submission" data should be a probabilistic
forecast prepared as a `tibble` with at minimum the following columns:

- **forecast_date**: Date of forecast

- **target**: Horizon and name of forecasted target

- **target_end_date**: Last date of the forecasted target (e.g.,
  Saturday of the given epidemiological week)

- **location**: FIPS code for location

- **type**: One of either "point" or "quantile" for the forecasted value

- **quantile**: The quantile for the forecasted value; `NA` if "type" is
  `"point"`

- **value**: The forecasted value

If format is "hubverse" the "submission" data should be a probabilistic
forecast prepared as a `tibble` with at minimum the following columns:

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

The "submission" data may optionally include a column with the name of
the model used, such that multiple models can be visualized in the same
plot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get some data
h_raw <- get_hdgov_hosp(limitcols=TRUE)

# Prep all the data
prepped_hosp_all <- prep_hdgov_hosp(h_raw)

# What are the last four weeks of recorded data?
last4 <-
  prepped_hosp_all %>%
  dplyr::distinct(week_start) %>%
  dplyr::arrange(week_start) %>%
  tail(4)

# Remove those
prepped_hosp <-
  prepped_hosp_all %>%
  dplyr::anti_join(last4, by="week_start")

# Make a tsibble
prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
                                     epiyear = epiyear,
                                     epiweek=epiweek,
                                     key=location)
# Limit to just one state and US
prepped_hosp_tsibble <-
  prepped_hosp_tsibble %>%
  dplyr::filter(location %in% c("US", "51"))

# Fit models and forecasts
hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               trim_date=NULL,
                               covariates=TRUE)

# Format for submission
hosp_formatted <- ts_format_for_submission(hosp_fitfor$tsfor)

# Plot with current and all data
plot_forecast(prepped_hosp, hosp_formatted$ensemble)
plot_forecast(prepped_hosp_all, hosp_formatted$ensemble)
plot_forecast(prepped_hosp, hosp_formatted$ensemble, location=c("US", "51"))
plot_forecast(prepped_hosp_all, hosp_formatted$ensemble, location=c("US", "51"))
plot_forecast(prepped_hosp, hosp_formatted$ets)
plot_forecast(prepped_hosp_all, hosp_formatted$ets)
plot_forecast(prepped_hosp, hosp_formatted$arima)
plot_forecast(prepped_hosp_all, hosp_formatted$arima)

# Demonstrating multiple models
prepped_hosp <-
  h_raw %>%
  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
  dplyr::filter(abbreviation != "DC") %>%
  dplyr::filter(week_start < as.Date("2022-01-08", format = "%Y-%m-%d"))

tsens_20220110 <-
  system.file("extdata/2022-01-10-SigSci-TSENS.csv", package="fiphde") %>%
  readr::read_csv(show_col_types = FALSE)
creg_20220110 <-
  system.file("extdata/2022-01-10-SigSci-CREG.csv", package="fiphde") %>%
  readr::read_csv(show_col_types = FALSE)
combo_20220110 <- dplyr::bind_rows(
  dplyr::mutate(tsens_20220110, model = "SigSci-TSENS"),
  dplyr::mutate(creg_20220110, model = "SigSci-CREG")
)
plot_forecast(prepped_hosp, combo_20220110, location = "24")
plot_forecast(prepped_hosp, tsens_20220110, location = "24")
plot_forecast(prepped_hosp, combo_20220110, location = c("34","36"))
plot_forecast(prepped_hosp, creg_20220110, location = "US", .model = "SigSci-CREG")
plot_forecast(prepped_hosp, creg_20220110, location = "US", .model = "SigSci-CREG")

## demonstrating different prediction interval widths
plot_forecast(prepped_hosp, combo_20220110, location = "24", pi = 0.5)
plot_forecast(prepped_hosp, combo_20220110, location = "24", pi = 0.9)
plot_forecast(prepped_hosp, combo_20220110, location = "24", pi = 0.95)
plot_forecast(prepped_hosp, combo_20220110, location = "24", pi = NULL)
} # }
```
