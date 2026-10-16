# WHO/NREVSS clinical lab surveillance data

Adapted from `cdcfluview::who_nrevss`.

This unexported helper function leverages the CDC FluView API to pull
flu surveillance data collected from U.S. World Health Organization
(WHO) Collaborating Laboratories and National Respiratory and Enteric
Virus Surveillance System (NREVSS) laboratories. The data retrieved can
be parameterized by geographic granularity and/or flu season. The
function is used internally by
[get_cdc_clin](https://signaturescience.github.io/fiphde/reference/get_cdc_clin.md).

## Usage

``` r
who_nrevss(region = c("national", "hhs", "census", "state"), years = NULL)
```

## Arguments

- region:

  One of "`national`", "`hhs`", "`census`", or "`state`"

- years:

  A vector of years to retrieve data for (i.e. `2014` for CDC flu season
  2014-2015). CDC has data for this API going back to 1997. Default
  value (`NULL`) means retrieve **all** years. NOTE: if you specify a
  2-digit season value the function will convert that to the
  corresponding season identifier (i.e. `57` == 2017-2018).

## References

- [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)
