# Simple Poisson count forecaster

This function is a helper that forecasts Poisson counts for near-term
horizons based on characteristics of recently observed count data. The
function effectively takes a rolling average of most recent observations
(augmenting with each forecasted horizon as the horizons progress), then
uses this average as the parameter for Lambda in a random draw from a
Poisson distribution.

## Usage

``` r
pois_forc(.data, .location, .var, horizon = 4)
```

## Arguments

- .data:

  Data frame with incoming data that includes a variable with counts
  (see ".var" argument), and location (must be stored in a column called
  "location") and a variable for sorting by date (must be stored in a
  column called "week_start")

- .location:

  The name of the location of interest

- .var:

  Bare, unquoted name of the variable with counts to be forecasted

- horizon:

  The number of horizons ahead to forecast; must be one of `4` or `5`;
  default is `4`

## Value

Vector with Poisson forecasts for the number of horizons specified.

## Examples

``` r
if (FALSE) { # \dontrun{
all_clin <- get_cdc_clin()
va_ahead <-
  dplyr::tibble(
    n_positive = pois_forc(all_clin, .location = "51", n_positive),
    total = pois_forc(all_clin, .location = "51", total),
    p_positive = n_positive / total)
va_ahead
} # }
```
