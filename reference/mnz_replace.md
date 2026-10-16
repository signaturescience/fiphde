# Minimum non-zero replacement

Replace zeros and negative values with the minimum non-zero positive
value from a vector.

## Usage

``` r
mnz_replace(x)
```

## Arguments

- x:

  A numeric vector

## Value

A vector of the same length with negatives and zeros replaced with the
minimum nonzero value of that vector.

## Examples

``` r
x <- c(.1, 0, -.2, NA, .3, .4, .0001, -.3, NA, 999)
x
#>  [1]   0.1000   0.0000  -0.2000       NA   0.3000   0.4000   0.0001  -0.3000
#>  [9]       NA 999.0000
mnz(x)
#> [1] 1e-04
mnz_replace(x)
#>  [1]   0.1000   0.0001   0.0001       NA   0.3000   0.4000   0.0001   0.0001
#>  [9]       NA 999.0000
tibble::tibble(x) %>% dplyr::mutate(x2=mnz_replace(x))
#> # A tibble: 10 × 2
#>           x       x2
#>       <dbl>    <dbl>
#>  1   0.1      0.1   
#>  2   0        0.0001
#>  3  -0.2      0.0001
#>  4  NA       NA     
#>  5   0.3      0.3   
#>  6   0.4      0.4   
#>  7   0.0001   0.0001
#>  8  -0.3      0.0001
#>  9  NA       NA     
#> 10 999      999     
```
