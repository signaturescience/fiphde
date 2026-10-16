# Fit and forecast with time-series approaches

This function allows the user to fit time series models and forecast
values out to a specified horizon. Starting from a `tsibble` object (see
[make_tsibble](https://signaturescience.github.io/fiphde/reference/make_tsibble.md)),
the function fits the models specified as a list in the "models"
argument. The "Details" section provides more information on how to
parameterize the models used. Note that if the input `tsibble` is
"keyed" (e.g., grouped by location) then the procedure will fit and
forecast independently for each grouping.

## Usage

``` r
ts_fit_forecast(
  prepped_tsibble,
  outcome = "flu.admits",
  horizon = 4L,
  trim_date = "2021-01-01",
  models = list(arima = "PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)", ets =
    "season(method=\"N\")", nnetar = NULL),
  covariates = TRUE,
  ensemble = TRUE
)
```

## Arguments

- prepped_tsibble:

  A `tsibble` with data formatted via
  [make_tsibble](https://signaturescience.github.io/fiphde/reference/make_tsibble.md)

- outcome:

  The outcome variable to model; default is `"flu.admits"`

- horizon:

  Number of weeks ahead to forecast

- trim_date:

  The date (YYYY-MM-DD) at which time series models should start
  fitting; default `"2021-01-01"`; if set to `NULL` the input data will
  not be trimmed (i.e., all data will be used to fit time series models)

- models:

  A list of right hand side formula contents for models you want to run;
  default is
  `list(arima='PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)', ets='season(method="N")', nnetar=NULL)`
  which runs a constrained ARIMA, non-seasonal ETS, and ignores the
  NNETAR model; see "Details" for more information

- covariates:

  Logical. Should flu hospitalization-specific covariates that should be
  modeled with the time series? If so, historical hospitalization and
  ILI rank for each epidemiological week, brought in with
  [prep_hdgov_hosp](https://signaturescience.github.io/fiphde/reference/prep_hdgov_hosp.md),
  is added to the ARIMA model.

- ensemble:

  Logical as to whether or not the models should be ensembled (using
  mean); default `TRUE`

## Value

A list of the time series fit, time series forecast, and model formulas.

- **tsfit**: A `mdl_df` class "mable" with one row for each location,
  columns for arima and ets models.

- **tsfor**: A `fbl_ts` class "fable" with one row per
  location-model-timepoint up to `horizon` number of time points.

- **formulas**: A list of ARIMA, ETS, and/or NNETAR formulas

## Details

When fitting time series models, the set of models used (and their
parameters) can be defined via a named list passed to the "models"
argument. The list should contain elements that define the right-hand
side of model formulas. The function internally uses the
[fable::fable](https://fable.tidyverts.org/reference/fable-package.html)
package, and any models provided must be part of the `fable` ecosystem
of time series models. The models passed must be named as "arima",
"ets", and "nnetar". To skip any one of these models set the named
argument for the given model to `NULL`. The "models" argument defaults
to
`list(arima = "PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)", ets = "season(method='N')", nnetar = NULL)`.
To run an unconstrained ARIMA: `list(arima='PDQ() + pdq()')` (see
[fable::ARIMA](https://fable.tidyverts.org/reference/ARIMA.html)). To
run a seasonal exponential smoothing:
`list(ets='season(method=c("A", "M", "N"), period="3 months")')` (see
[fable::ETS](https://fable.tidyverts.org/reference/ETS.html)). To run an
autoregressive neural net with P=1: `list(nnetar="AR(P=1)")` (see
[fable::NNETAR](https://fable.tidyverts.org/reference/NNETAR.html)).

## References

<https://fable.tidyverts.org/>

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
                                key=location) %>%
  dplyr::filter(location %in% c("US", "51"))

# Run with default constrained ARIMA, nonseasonal ETS, no NNETAR
hospfor1 <- ts_fit_forecast(prepped_tsibble,
                            horizon=4L,
                            outcome="flu.admits",
                            covariates=TRUE)
# Run an unconstrained ARIMA, seasonal ETS, no NNETAR
hospfor2 <- ts_fit_forecast(prepped_tsibble,
                            horizon=4L,
                            outcome="flu.admits",
                            covariates=TRUE,
                            models=list(arima='PDQ() + pdq()',
                                        ets='season(method=c("A", "M", "N"), period="3 months")',
                                        nnetar=NULL))
# Run an unconstrained ARIMA, seasonal ETS, NNETAR
hospfor3 <- ts_fit_forecast(prepped_tsibble,
                            horizon=4L,
                            outcome="flu.admits",
                            covariates=TRUE,
                            models=list(arima='PDQ() + pdq()',
                                        ets='season(method=c("A", "M", "N"), period="3 months")',
                                        nnetar="AR(P=1)"))
} # }
```
