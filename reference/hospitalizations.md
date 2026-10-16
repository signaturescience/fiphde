# Laboratory-confirmed influenza hospitalizations

Adapted from `cdcfluview::hospitalizations`.

This unexported helper function leverages the CDC FluView API to pull
influenza hospitalizations collected by surveillance instruments
(including FluSurv-NET). The data retrieved can be parameterized by
geographic granularity and/or flu season, and includes hospitalization
rates by age group. The function is used internally by
[get_cdc_hosp](https://signaturescience.github.io/fiphde/reference/get_cdc_hosp.md).

## Usage

``` r
hospitalizations(
  surveillance_area = c("flusurv", "eip", "ihsp"),
  region = "all",
  years = NULL
)
```

## Arguments

- surveillance_area:

  One of `"flusurv"`, `"eip"`, or `"ihsp"`

- region:

  Individual region within the surveillance area selected; default
  `"all"` mimics selecting "Entire Network" from the CDC FluView
  application drop down; see "Details" for list of valid region values
  for each surveillance area

- years:

  A vector of years to retrieve data for (i.e. `2014` for CDC flu season
  2014-2015). CDC has data for this API going back to 2009 and up until
  the *previous* flu season. Default value (`NULL`) means retrieve
  **all** years. NOTE: if you happen to specify a 2-digit season value
  (i.e. `56` == 2016-2017) the function is smart enough to retrieve by
  season ID vs convert that to a year.

## Details

**NOTE**: The list of regions was compiled in February 2023 by querying
the CDC FluView API. Individual regions may not be accessible in all
cases. As of late 2023, the query was only returning results for the
"Entire Network" selection.

Each possible value "surveillance_area" (`"flusurv"`, `"eip"`, or
`"ihsp"`) can be further queried by region. The following is a list of
valid regions:

- **flusurv**: "Entire Network"

- **eip**: "Entire Network", "California", "Colorado", "Connecticut",
  "Georgia", "Maryland", "Minnesota", "New Mexico", "New York - Albany",
  "New York - Rochester", "Oregon, "Tennessee"

- **ihsp**: "Entire Network", "Idaho", "Iowa", "Michigan", "Ohio",
  "Oklahoma", "Rhode Island", "South Dakota", "Utah"

## References

- [Hospital Portal](https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html)

- [cdcfluview package](https://github.com/hrbrmstr/cdcfluview)
