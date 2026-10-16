# Replace ILINet data with nowcast

This function replaces the weighted ILI retrieved from
[get_cdc_ili](https://signaturescience.github.io/fiphde/reference/get_cdc_ili.md)
with nowcast data for each of the locations in the original data. The
function will first attempt to use ILI Nearby nowcasts pulled using
[get_nowcast_ili](https://signaturescience.github.io/fiphde/reference/get_nowcast_ili.md).
If the ILI Nearby nowcasts are unavailable, the function will optionally
fallback to a pseudo nowcast method that averages the observed ILI for
the 4 most recent weeks. The nowcast data will be used to add 1
additional week to the observed ILI data and (optionally) replace the
number of weeks specified in the "weeks_to_replace" argument.

## Usage

``` r
replace_ili_nowcast(
  ilidat,
  start_date = NULL,
  weeks_to_replace = 1,
  fallback = TRUE,
  try_api = TRUE
)
```

## Arguments

- ilidat:

  ILI data retrieved via
  [get_cdc_ili](https://signaturescience.github.io/fiphde/reference/get_cdc_ili.md)

- start_date:

  Date from which to start nowcasting; default is
  [lubridate::today](https://lubridate.tidyverse.org/reference/now.html)

- weeks_to_replace:

  Number of weeks of `ilidat` to replace; default is `1`

- fallback:

  Logical as to whether or not to fall back to pseudo nowcast (average
  of last 4 ILI weeks in the given location) if nowcast data is
  unavailable; default is `TRUE`

- try_api:

  Logical as to whether or not the function should try the ILI Nearby
  nowcast API; default is `TRUE`; if `FALSE` then the function will not
  attempt to query the API at all

## Value

A `tibble` with the following columns:

- **location**: FIPS code for the location

- **region_type**: The type of location

- **abbreviation**: Abbreviation for the location

- **region**: Name of the region

- **epiyear**: Year of reporting (in epidemiological week calendar)

- **epiweek**: Week of reporting (in epidemiological week calendar)

- **week_start**: Date of beginning (Sunday) of the given
  epidemiological week

- **weighted_ili**: Population-weighted percentage of ILI outpatient
  visits

## Examples

``` r
if (FALSE) { # \dontrun{
ilidat <- get_cdc_ili(years=2021)
ilidat <-
  ilidat %>%
  dplyr::filter(location=="US" | abbreviation=="VA") %>%
  dplyr::group_by(location) %>%
  dplyr::slice_max(week_start, n=4) %>%
  dplyr::select(location:weighted_ili)
ilidat
iliaug <- replace_ili_nowcast(ilidat, weeks_to_replace=1)
iliaug
} # }
```
