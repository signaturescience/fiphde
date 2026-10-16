# Round and preserve vector

This unexported helper is used to ensure that categorical forecasts are
rounded to sum to 1.

## Usage

``` r
round_preserve(x, digits = 0)
```

## Arguments

- x:

  Numeric vector with values to round

- digits:

  The number of digits to use in precision; defalut is `0`

## Value

Vector of same length as "x" with values rounded
