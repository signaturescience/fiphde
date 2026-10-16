# Retrieve clinical laboratory percent positive flu data

This function returns weekly state and/or national clinical laboratory
percent positivity data from the NREVSS reporting instrument via the CDC
FluView API.

## Usage

``` r
get_cdc_clin(region = "both", years = NULL)
```

## Arguments

- region:

  Either "state", "national", or "both". Defaults to `"both"` to return
  state and national data combined.

- years:

  A vector of years to retrieve data for. CDC has data going back
  to 1997. Default value (`NULL`) retrieves **all** years.

## Value

A `tibble` with the following columns:

- **abbreviation**: Abbreviation for the location

- **location**: FIPS code for the location

- **epiyear**: Year of reporting (in epidemiological week calendar)

- **epiweek**: Week of reporting (in epidemiological week calendar)

- **week_start**: Date of beginning (Sunday) of the given
  epidemiological week

- **p_positive**: Percentage of positive specimens

- **n_positive**: Total number of positive specimens

- **total**: Total number of specimens tested

## References

<https://gis.cdc.gov/grasp/fluview/Phase_6_Cleared_Help.pdf>

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all clinical lab flu positivity data
all_clin <- get_cdc_clin()
all_clin
# Alternatively look at a specific location and time
# This 2021 will return weekly data
# Starting at beginning of 2021/22 season
# Ending the week before start of 2022/23 season
va_clin <-
  get_cdc_clin(region = "state", years = 2021) %>%
  dplyr::filter(location == "51")
va_clin
} # }
```
