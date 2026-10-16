# Retrieve data from RESP-NET

This unexported helper function retrieves data from the RESP-NET API for
respiratory hospitalizations. The data retrieved can be filtered to
specific network(s) reported. The function is used internally by
[get_cdc_hosp](https://signaturescience.github.io/fiphde/reference/get_cdc_hosp.md).

## Usage

``` r
get_respnet(network = "FluSurv-NET")
```

## Arguments

- network:

  The name of the RESP-NET network to query; must be one of
  `"FluSurv-NET"`, `"COVID-NET"`, `"RSV-NET"`, or `"Combined"`; default
  is `"FluSurv-NET"`

## References

- [RESP-NET Data
  Portal](https://data.cdc.gov/Public-Health-Surveillance/Rates-of-Laboratory-Confirmed-RSV-COVID-19-and-Flu/kvib-3txy/about_data)

- [RESP-NET Dashboard](https://www.cdc.gov/resp-net/dashboard/)
