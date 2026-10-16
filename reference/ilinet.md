# Retrieve ILINet surveillance data

Adapted from `cdcfluview::ilinet`.

This unexported helper function retrieves current and historical ILINet
surveillance data for the identified region via the CDC FluView API. The
function is used internally in
[get_cdc_ili](https://signaturescience.github.io/fiphde/reference/get_cdc_ili.md).
Data returned include weighted and unweighted ILI percentage, as well as
age-specific ILI outpatient visit counts for each location /
epidemiological week.

## Usage

``` r
ilinet(region = c("national", "hhs", "census", "state"), years = NULL)
```

## Arguments

- region:

  One of "`national`", "`hhs`", "`census`", or "`state`"

- years:

  A vector of years to retrieve data for (i.e. `2014` for CDC flu season
  2014-2015). CDC has data for this API going back to 1997. Default
  value (`NULL`) means retrieve **all** years. NOTE: if you happen to
  specify a 2-digit season value (i.e. `57` == 2017-2018) the function
  is smart enough to retrieve by season ID vs convert that to a year.

## References

- [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)

- [CDC FluView
  Portal](https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html)
