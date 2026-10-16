# Calculate categorical probability density

This unexported helper function is used to build a distribution from
quantile forecasts and then calculate the probability density for the
thresholds associated with each category forecasted: large increase,
increase, stable, decrease, large decrease.

## Usage

``` r
density_probs(df, n_horizons = 5, ...)
```

## Arguments

- df:

  Data frame with forecasts and categorical thresholds joined

- n_horizons:

  Number of horizons ahead

- ...:

  Additional arguments passed to `distfromq::distfromq()`

## Value

Data frame with probabilities for each rate change category
