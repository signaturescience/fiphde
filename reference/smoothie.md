# Calculate smoothed and weighted averages of previous observations

This helper function calculates a weighted average of the last n
observations.

## Usage

``` r
smoothie(x, n = 4, weights = c(1, 2, 3, 4))
```

## Arguments

- x:

  Incoming vector of observations

- n:

  Number of recent observations to smooth; default is `4`

- weights:

  Vector of weights to be applied to last n observations during
  averaging

## Value

Vector of length 1 with the weighted average of last n observations.

## Examples

``` r
if (FALSE) { # \dontrun{
## pull and prep weekly US flu hospitalization data
hosp_us <-
  get_hdgov_hosp() %>%
  prep_hdgov_hosp() %>%
  dplyr::filter(location == "US")

## what do the last 4 observations look like?
tail(hosp_us$flu.admits, 4)

## smooth over last 4 with default weights
smoothie(hosp_us$flu.admits, n=4, weights=c(1,2,3,4))

## try smoothing over last 4 with different weights (exponential this time)
smoothie(hosp_us$flu.admits, n=4, weights=exp(1:4))
} # }
```
