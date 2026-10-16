# Nowcast clinical laboratory percent positive flu data

This function provides a naive nowcasting method for clinical laboratory
percent positive flu data. The methodology simply averages the last 4
weeks of available data and uses this average as the value for the
number of weeks specified to replace. The function will always add 1
additional week to the observed data and (optionally) replace the number
of weeks specified in the "weeks_to_replace" argument. This is useful
given that there is reporting lag in the NREVSS clinical laboratory
percent positive flu data.

## Usage

``` r
clin_nowcast(clin, weeks_to_replace = 1)
```

## Arguments

- clin:

  Data prepared with
  [get_cdc_clin](https://signaturescience.github.io/fiphde/reference/get_cdc_clin.md)

- weeks_to_replace:

  Number of retrospective weeks to replace with nowcast; default is `1`

## Value

A `tibble` with the following columns:

- **abbreviation**: Abbreviation for the location

- **location**: FIPS code for the location

- **epiyear**: Year of reporting (in epidemiological week calendar)

- **epiweek**: Week of reporting (in epidemiological week calendar)

- **week_start**: Date of beginning (Sunday) of the given
  epidemiological week

- **p_positive**: Percentage of positive specimens

- **n_positive**: Total number of positive specimens

- **total**: Total number of specimens tested

## Examples

``` r
if (FALSE) { # \dontrun{

# Get data for Texas
tx_clin <-
  get_cdc_clin(region = "state") %>%
  dplyr::filter(location == "48")

# Look at most recent observations
tx_clin %>%
  dplyr::arrange(week_start) %>%
  tail()

# Now augment with default 1 week nowcast
tx_clin %>%
  clin_nowcast(., weeks_to_replace = 1) %>%
  dplyr::arrange(week_start) %>%
  tail()

# And again augmented with 2 week nowcast instead
tx_clin %>%
  clin_nowcast(., weeks_to_replace = 2) %>%
  dplyr::arrange(week_start) %>%
  tail()

} # }
```
