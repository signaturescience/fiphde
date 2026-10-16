# Forecast glm models

This function uses fitted model object from
[glm_fit](https://signaturescience.github.io/fiphde/reference/glm_fit.md)
and future covariate data to create probablistic forecasts at specific
quantiles derived from the "alpha" parameter.

## Usage

``` r
glm_forecast(
  .data,
  new_covariates = NULL,
  fit,
  alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2
)
```

## Arguments

- .data:

  Data including all explanatory and outcome variables needed for
  modeling

- new_covariates:

  Tibble with one column per covariate, and n rows for n horizons being
  forecasted

- fit:

  Fitted model object from
  [glm_fit](https://signaturescience.github.io/fiphde/reference/glm_fit.md);
  note must be accessed from first element in "fit" column

- alpha:

  Vector specifying the threshold(s) to be used for prediction
  intervals; alpha of `0.05` would correspond to 95% PI; default is
  `c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2` to create a range of
  intervals

## Value

A `tibble` with forecasted data including the following columns:

- **epiweek**: The epidemiological week for the forecasted horizon

- **epiyear**: The epidemiological year for the forecasted horizon

- **quantile**: The quantile for the forecasted value; `NA` for point
  estimate

- **value**: The forecasted value
