# Convert MMWR format to date

Adapted from `cdcfluview::mmwr_week_to_date`.

This function transforms MMWR epidemiological year+week (or
year+week+day) to a date object. This was implemented based on the
`cdcfluview::mmwr_week_to_date` function, which adapted similar
functionality from the `MMWRweek` package.

## Usage

``` r
mmwr_week_to_date(year, week, day = NULL)
```

## Arguments

- year:

  Vector of epidemiological year(s); must be same length as "week" and
  "day" (unless "day" is `NULL`)

- week:

  Vector of epidemiological week(s); must be same length as "year" and
  "day" (unless "day" is `NULL`)

- day:

  Vector of day(s); must be same length as "week" and "year" (unless set
  to is `NULL`); default is `NULL` and the day returned will be the
  first day of the epidemiological week (i.e., Sunday)

## Value

Vector of date objects as with as many elements as input year(s),
week(s), day(s)

## References

- [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)

## Examples

``` r
mmwr_week_to_date(2020,1)
#> [1] "2019-12-29"
mmwr_week_to_date(2020,1,5)
#> [1] "2020-01-02"
mmwr_week_to_date(c(2020,2021,2022),c(1,2,8), c(1,1,7))
#> [1] "2019-12-29" "2021-01-10" "2022-02-26"
```
