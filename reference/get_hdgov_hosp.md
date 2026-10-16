# Retrieve hospitalization data from HHS

This function retrieves hospital utilization time series data
distributed through healthdata.gov. Data are aggregated to the state
granularity from facility level reports via HHS TeleTracking, HHS
Protect, and the National Healthcare Safety Network (historically).
Users can optionally filter to include all fields or restrict to a
prespecified set of fields relevant to COVID and influenza hospital
utilization. The results are returned as a `tibble`.

## Usage

``` r
get_hdgov_hosp(
  endpoint = "https://healthdata.gov/api/views/g62h-syeh/rows.csv",
  app_token = Sys.getenv("HEALTHDATA_APP_TOKEN"),
  limitcols = FALSE,
  shift_back = TRUE
)
```

## Arguments

- endpoint:

  URL to healthdata.gov endpoint

- app_token:

  App token from healthdata.gov; default is to look for environment
  variable called `"HEALTHDATA_APP_TOKEN"` and if a token is not
  supplied to proceed with possibility of rate limitation (see "Details"
  for more information)

- limitcols:

  Logical as to whether or not to limit to prespecified set of columns
  (see "Value" section for more details); default `FALSE`

- shift_back:

  Logical as to whether or not the dates for the retrieved data should
  be shifted back to reflect previous day; default is `TRUE`

## Value

A `tibble` with at least the following columns:

- **state**: Abbreviation of the state

- **date**: Date of report

- **flu.admits**: Count of flu cases among admitted patients on previous
  day

- **flu.admits.cov**: Coverage (number of hospitals reporting) for
  incident flu cases

- **flu.deaths**: Count of flu deaths on previous day

- **flu.deaths.cov**: Coverage (number of hospitals reporting) for flu
  deaths

- **flu.icu**: Count of flu cases among ICU patients on previous day

- **flu.icu.cov**: Coverage (number of hospitals reporting) for flu ICU
  cases

- **flu.tot**: Count of total flu cases among admitted patients

- **flu.tot.cov**: Coverage (number of hospitals reporting) for total
  flu cases

- **cov.admits**: Count of COVID cases among admitted patients on
  previous day

- **cov.admits.cov**: Coverage (number of hospitals reporting) for
  incident COVID cases

- **cov.deaths**: Count of COVID deaths on previous day

- **cov.deaths.cov**: Coverage (number of hospitals reporting) for COVID
  deaths

If `limitcols=TRUE` then the only columns returned will be those listed
above. However, if `limitcols=FALSE` then the function will additionally
return *all* other fields in the state-aggregated hospitalization data.

## Details

The data retrieval will proceed whether or not an API token has been
supplied via the `app_token` argument. However, to avoid possible rate
limits it is recommended to retrieve a token for the healthdata.gov API
(<https://healthdata.gov/profile/edit/developer_settings>), and add that
token as an entry to `.Renviron` with
`HEALTHDATA_APP_TOKEN="yourtokenhere"`.

## References

<https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh>

<https://dev.socrata.com/foundry/healthdata.gov/g62h-syeh>

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve hospitalization data (all columns)
get_hdgov_hosp()
# Retrieve hospitalization data (limited columns)
get_hdgov_hosp(limitcols=TRUE)
} # }
```
