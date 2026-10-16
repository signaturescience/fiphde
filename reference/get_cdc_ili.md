# Retrieve ILI data from ILINet

This function pulls ILINet data from the CDC FluView API. Data are
available historically and can be pulled at the state, national, or HHS
region level.

## Usage

``` r
get_cdc_ili(region = c("national", "state", "hhs"), years = NULL)
```

## Arguments

- region:

  Either "state", "national", or "hhs"; defaults to
  `c("national", "state", "hhs")` for all three.

- years:

  A vector of years to retrieve data for. CDC has data going back
  to 1997. Default value (`NULL`) retrieves **all** years.

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

- **unweighted_ili**: Unweighted percentage of ILI outpatient visits

- **ilitotal**: Total number of ILI outpatient visits reported

- **num_providers**: Number of providers reporting

- **total_patients**: Total number of outpatient visits reported

- **population**: Total population for the given location

## References

<https://gis.cdc.gov/grasp/fluview/FluViewPhase1QuickReferenceGuide.pdf>

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve ILI data for specific years and regions
get_cdc_ili(region="national", years=2021)
get_cdc_ili(region="hhs", years=2021)
get_cdc_ili(region="state", years=2021) %>% dplyr::filter(abbreviation=="VA")
get_cdc_ili(region=c("national", "state"), years=2021)
} # }
```
