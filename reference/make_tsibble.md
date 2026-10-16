# Make `tsibble`

This function converts an input `tibble` with columns for
[lubridate::epiyear](https://lubridate.tidyverse.org/reference/year.html)
and
[lubridate::epiweek](https://lubridate.tidyverse.org/reference/week.html)
into a
[tsibble::tsibble](https://tsibble.tidyverts.org/reference/tsibble.html)
object. The `tsibble` has columns specifying indices for the time series
as well as a date for the Monday of the epiyear/epiweek combination at
each row.

## Usage

``` r
make_tsibble(df, epiyear, epiweek, key = location)
```

## Arguments

- df:

  A `tibble` containing columns `epiyear` and `epiweek`.

- epiyear:

  Unquoted variable name containing the MMWR epiyear.

- epiweek:

  Unquoted variable name containing the MMWR epiweek.

- key:

  Unquoted variable name containing the name of the column to be the
  tsibble key. See tsibble::as_tsibble.

## Value

A `tsibble` containing additional columns `monday` indicating the date
for the Monday of that epiweek, and `yweek` (a yearweek vctr class
object) that indexes the `tsibble` in 1 week increments.

## Examples

``` r
# Create an example tibble
d <- tibble::tibble(epiyear=c(2020, 2020, 2021, 2021),
                    epiweek=c(52, 53, 1, 2),
                    location="US",
                    somedata=101:104)
# Convert to tsibble (keyed time series tibble)
make_tsibble(d, epiyear = epiyear, epiweek=epiweek, key=location)
#> # A tsibble: 4 x 6 [1W]
#> # Key:       location [1]
#>   epiyear epiweek monday        yweek location somedata
#>     <dbl>   <dbl> <date>       <week> <chr>       <int>
#> 1    2020      52 2020-12-21 2020 W52 US            101
#> 2    2020      53 2020-12-28 2020 W53 US            102
#> 3    2021       1 2021-01-04 2021 W01 US            103
#> 4    2021       2 2021-01-11 2021 W02 US            104
```
