# Hubverse formatting

This unexported helper is used internally inside in
[format_for_submission](https://signaturescience.github.io/fiphde/reference/format_for_submission.md).
It specifically updates formatting for Hubverse guidelines.

## Usage

``` r
hubverse_format(dat, horizon_shift = 1)
```

## Arguments

- dat:

  Forecast prepped in "legacy" format

- horizon_shift:

  Number of horizons to shift backwards to align with reference date;
  default is `1`

## Value

Formatted `tibble`

## References

<https://github.com/signaturescience/FluSight-forecast-hub/tree/main/model-output#forecast-file-format>

<https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-forecasts/README.md>
