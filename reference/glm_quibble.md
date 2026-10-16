# Get quantiles from prediction intervals

This helper function runs the
[trending::predict.trending_fit](https://rdrr.io/pkg/trending/man/predict.trending_fit.html)
method on a fitted model at specified values of "alpha" in order to
create a range of prediction intervals. The processing also includes
steps to convert the alpha to corresponding quantile values at upper and
lower bounds. See "Details" for more information on the translation of
"alpha" to quantile values. This function is used internally in
[glm_forecast](https://signaturescience.github.io/fiphde/reference/glm_forecast.md).

## Usage

``` r
glm_quibble(
  fit,
  new_data,
  alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2
)
```

## Arguments

- fit:

  Fitted model object from
  [glm_fit](https://signaturescience.github.io/fiphde/reference/glm_fit.md);
  note must be accessed from first element in "fit" column

- new_data:

  A `tibble` with new data on which the
  [trending::predict.trending_fit](https://rdrr.io/pkg/trending/man/predict.trending_fit.html)
  method should run

- alpha:

  Vector specifying the threshold(s) to be used for prediction intervals
  (PI); alpha of `0.05` would correspond to 95% PI; default is
  `c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2` to create a range of
  intervals

## Value

A `tibble` with forecasted data including the following columns:

- **epiweek**: The epidemiological week for the forecasted horizon

- **epiyear**: The epidemiological year for the forecasted horizon

- **quantile**: The quantile for the forecasted value; `NA` for point
  estimate

- **value**: The forecasted value

## Details

The "alpha" parameter defines the width of prediction interval (PI). For
example, an alpha = 0.05 would correspond to a 95% PI. This function
uses the PI(s) (per the alpha value(s) specified) to construct a range
of quantiles that fall at lower and upper bound of each PI. Continuing
from the example of alpha = 0.05, the quantile estimates returned would
fall at 0.025 (lower bound of PI) and 0.975 (upper bound of PI).
