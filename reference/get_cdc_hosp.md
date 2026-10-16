# Retrieve hospitalization data from FluSurv-NET

This function retrieves historical FluSurv-NET hospitalization data via
either RESP-NET or the CDC FluView API (see 'Details' section).

## Usage

``` r
get_cdc_hosp(source = "fluview", years = NULL)
```

## Arguments

- source:

  The source for the hospitalization data; must be one of `"fluview"` or
  `"resp-net"`; default is `"fluview"`

- years:

  A vector of years to retrieve data for (i.e. 2014 for CDC flu season
  2014-2015). CDC has data going back to 2009 and up until the
  *previous* flu season. Default value (`NULL`) retrieves **all** years.
  Only used if `source="fluview"`.

## Value

A `tibble` with the following columns:

- **location**: FIPS code for the location

- **abbreviation**: Abbreviation for the location

- **region**: Name of the region

- **epiyear**: Year of reporting (in epidemiological week calendar)

- **epiweek**: Week of reporting (in epidemiological week calendar)

- **week_start**: Date of beginning (Sunday) of the given
  epidemiological week

- **week_end**: Date of end (Saturday) of the given epidemiological week

- **rate**: The cumulative rate per 100k

- **weekly_rate**: The weekly rate per 100k

- **season**: The flu season to which the given epidemiological week
  belongs

## Details

The data retrieval from FluView and and RESP-NET pulls FluSurv-NET
lab-confirmed hospitalization data as overall cumulative rates and
weekly incident rates across reporting sites, age groups, sex, and
race/ethnicity categories. Note that as of October 2024, the FluSurv-NET
hospitalizations from RESP-NET begin in the 2018-19 season, while the
FluView data goes back to 2009-10.

## References

- [Hospital Portal](https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html)

- [cdcfluview package](https://github.com/hrbrmstr/cdcfluview)

- [RESP-NET Data
  Portal](https://data.cdc.gov/Public-Health-Surveillance/Rates-of-Laboratory-Confirmed-RSV-COVID-19-and-Flu/kvib-3txy/about_data)

- [RESP-NET Dashboard](https://www.cdc.gov/resp-net/dashboard/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve FluSurv-Net hospitalization data for specific year(s)
get_cdc_hosp()
} # }
```
