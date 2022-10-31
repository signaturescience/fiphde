# fiphde 0.3.1

Rearranged the submission script to pull common items (data retrieval, package loads, etc.) to the top of the script, so that CREG and TSENS can be run independently as needed.

# fiphde 0.3.0

## New features

This release brings in additional data retrieval and processing functions. The package now includes code adapted from `cdcfluview` to query the NREVSS surveillance system and pull clinical laboratory percent positive flu data. The `who_nrevss()` function is unexported but is used in the user-facing `get_cdc_clin()` function, which pulls and preps the percent positive data. A complementary function, `clin_nowcast()`, is now available to augment the clinical laboratory percent positivity with a nowcasted value.

The package also now includes helper functions to prepare covariates for modeling. For example, the new `pois_forc()` helper creates a forecasted counts based on recently observed values. This can be used to create forecasted values for number of tests and number of positive flu specimens (and thefore percent positivity) to feed into models that include percent positive lab results as a covariate. The new `smoothie()` function is also available to create weighted averages of recent observations, which can be used as covariate data in models.

# fiphde 0.2.1

Minor fix to include "start_date" parameter in `replace_ili_nowcast()` function so that ILI nowcasting can be performed retrospectively if needed.

# fiphde 0.2.0

## New features

This release includes code adapted from `cdcfluview` to query ILI and flu hospitalization data. The functions to do so are `ilinet()` and `hospitalizations()`, which are unexported but used as helper functions internally. For example, `ilinet()` is used by the exported `get_cdc_ili()` function. By including these functions, we have minimized the number of dependencies for `fiphde` thereby streamlining the package installation procedure.

# fiphde 0.1.0

Initial release !

Built and checked successfully on Ubuntu 20.04, Windows Server 2022, Mac OSX.
