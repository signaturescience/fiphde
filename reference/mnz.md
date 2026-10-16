# Minimum non-zero

Helper function to get the minimum non-zero positive value from a
vector. Used internally in
[mnz_replace](https://signaturescience.github.io/fiphde/reference/mnz_replace.md).

## Usage

``` r
mnz(x)
```

## Arguments

- x:

  A numeric vector

## Value

The minimum non-zero positive value from `x`

## Examples

``` r
x <- c(.1, 0, -.2, NA, .3, .4, .0001, -.3, NA, 999)
x
#>  [1]   0.1000   0.0000  -0.2000       NA   0.3000   0.4000   0.0001  -0.3000
#>  [9]       NA 999.0000
mnz(x)
#> [1] 1e-04
```
