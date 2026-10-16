# Forecast ILI

This function forecasts ILI up to a specified future horizon. The models
used can be parameterized with a "models" argument (for more details see
[ts_fit_forecast](https://signaturescience.github.io/fiphde/reference/ts_fit_forecast.md)).
By default, the function will use an ARIMA approach to model all
locations in the input historical ILI data and then use the fitted
models to forecast out to each of the horizons.

## Usage

``` r
forecast_ili(
  ilidat,
  horizon = 4L,
  trim_date = NULL,
  models = list(arima = "PDQ(0,0,0)+pdq(1:2,0:2,0)")
)
```

## Arguments

- ilidat:

  Data returned from
  [get_cdc_ili](https://signaturescience.github.io/fiphde/reference/get_cdc_ili.md)

- horizon:

  Optional horizon periods through which the forecasts should be
  generated; default is `4`

- trim_date:

  Earliest start date you want to use for ILI data; default `NULL`
  doesn't trim

- models:

  The list of model parameters passed to
  [ts_fit_forecast](https://signaturescience.github.io/fiphde/reference/ts_fit_forecast.md);
  defaults to `list(arima="PDQ(0,0,0)+pdq(1:2,0:2,0)"`

## Value

A named list containing:

- **ilidat**: The data sent into the function filtered to the location
  and the `trim_date`. Select columns returned.

- **ilidat_tsibble**: The `tsibble` class object returned by running
  [make_tsibble](https://signaturescience.github.io/fiphde/reference/make_tsibble.md)
  on the data above.

- **ili_fit**: The fit from
  [fabletools::model](https://fabletools.tidyverts.org/reference/model.html).

- **ili_forecast**: The forecast from
  [fabletools::forecast](https://fabletools.tidyverts.org/reference/reexports.html)
  at the specified horizon.

- **ili_future**: The `horizon`-number of weeks of ILI data forecasted
  into the future.

- **ili_bound**: The data in 1 bound to the data in 5.

- **arima_params**: A tibble with ARIMA model parameters for each
  location (if `type="arima"`).

- **locstats**: A tibble with missing data information on all locations.

- **removed**: A tibble with locations removed because of high missing
  ILI data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve ILI data
ilidat <- get_cdc_ili(region = c("national", "state", "hhs"),
                      years = 2010:lubridate::year(lubridate::today()))

# Using data only from march 2020 forward, for US only
ilidat_us <- ilidat %>% dplyr::filter(location=="US")
# Replace most recent week with nowcast data, and nowcast last week
ilidat_us <- ilidat_us %>% replace_ili_nowcast(weeks_to_replace=1)
ilifor_us <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01")
# Take a look at objects that come out ILI forecasting procedure
ilifor_us$ili_fit
ilifor_us$arima_params
ilifor_us$ili_forecast
head(ilifor_us$ili_bound)
tail(ilifor_us$ili_bound, 10)
} # }
```
