# Non-seasonal flu hospitalization imputation

This unexported helper function is used to create a "non-seasonal",
location-specific imputation estimate for weekly NHSN flu
hospitalization counts. The imputation approach was motivated by the
change in reporting requirements for the NHSN hospital respiratory
disease metrics, which became optional from April 2024 to November 2024.
This function includes four different approaches (see 'Details' for
more) for adjusting and/or filling the gap in state-level flu
hospitalization reporting.

## Usage

``` r
ns_impute(
  dat,
  location,
  method = "val",
  begin_date = "2024-04-28",
  end_date = "2024-11-02"
)
```

## Arguments

- dat:

  A `tibble` with hospitalization data prepared either by
  [prep_hdgov_hosp](https://signaturescience.github.io/fiphde/reference/prep_hdgov_hosp.md)
  or
  [prep_nhsn_weekly](https://signaturescience.github.io/fiphde/reference/prep_nhsn_weekly.md)

- location:

  FIPS code for location to impute

- method:

  Imputation method to use; must be one of `"val"`, `"diff"`,
  `"median"`, or `"partial"` (see 'Details' for more); default is
  `"val"`

- begin_date:

  Start date for imputation in YYYY-MM-DD format; default is
  `"2024-04-28"`

- end_date:

  End date for imputation in YYYY-MM-DD; default is `"2024-11-02"`

## Value

A `tibble` with the same structure as the input for the "dat" argument,
but with weeks between "begin_date" and "end_date" imputed.

## Details

There are four possible methods for imputing non-seasonal weeks
implemented in this function:

- "val": Random sampling from a vector of values including all flu
  hospitalizations reported weeks between June-October 2022 and
  June-October 2023 for the given location; first and last values are
  defined as median of the random sample and the most recent un-imputed
  value (i.e., the week before imputation begins and the week after
  imputation ends)

- "diff": Random sampling from a vector of week-to-week differences in
  flu hospitalizations reported in weeks between June-October 2022 and
  June-October 2023 for the given location

- "median": Median of 2022 and 2023 values reported for the given
  epiweek

- "partial": Uses the `adjust_partial=TRUE` flag for the
  [prep_nhsn_weekly](https://signaturescience.github.io/fiphde/reference/prep_nhsn_weekly.md)
  function to fill the weeks in the date range specified
