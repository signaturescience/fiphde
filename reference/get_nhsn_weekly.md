# Retrieve weekly NHSN flu hospitalization data

This function retrieves weekly aggregated NHSN hospital respiratory data
API. The function was written to use the default API endpoint (see
description of "endpoint" argument and link in references). Note one the
available endpoints includes data flagged as "preliminary". All reported
weekly aggregates include the number of facilities reporting. In the
weeks between April 28, 2024 and November 02, 2024 the NHSN flu
hospitalization signal was not required to be reported.

## Usage

``` r
get_nhsn_weekly(endpoint = "https://data.cdc.gov/api/views/mpgq-jmmr/rows.csv")
```

## Arguments

- endpoint:

  URL to data.cdc.gov endpoint; default is
  `"https://data.cdc.gov/api/views/mpgq-jmmr/rows.csv"` for the
  preliminary reporting signal

## Value

A `tibble` with the following columns:

- **abbreviation**: Abbreviation of the state or US aggregate

- **week_end**: End date for the epiweek/epiyear being reported

- **flu.admits**: Count of incident flu cases among hospitalized
  patients

- **flu.admits.cov**: Coverage (number of hospitals reporting) for
  incident flu cases

- **flu.admits.cov.perc**: Coverage (percentage of hospitals reporting)
  for incident flu cases

## References

<https://data.cdc.gov/Public-Health-Surveillance/Weekly-Hospital-Respiratory-Data-HRD-Metrics-by-Ju/mpgq-jmmr/about_data>

<https://data.cdc.gov/Public-Health-Surveillance/Weekly-Hospital-Respiratory-Data-HRD-Metrics-by-Ju/ua7e-t2fy/about_data>
