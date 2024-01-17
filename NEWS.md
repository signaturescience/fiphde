# fiphde 2.0.1

## Bug fixes

### Explorer app internal object naming

This release resolves an issue with naming of an internal object in the plausibility analysis section of the explorer app.

# fiphde 2.0.0

## New features

### Accommodation for Hubverse formatting 

As of this release, the package now optionally supports forecast submission prep and visualization for the Hubverse forecast format (https://hubdocs.readthedocs.io/en/latest/user-guide/model-output.html) that is being used in the 2023-24 FluSight initiative (https://github.com/signaturescience/FluSight-forecast-hub/tree/main/model-output#forecast-file-format). To accommodate this format, several functions now have an additional option for "format". Affected functions include `format_for_submission()`, `forecast_categorical()`, and `plot_forecast()`. Additionally, the explorer app (launched by `fiphde_launcher()`) now includes an option to specify the format for input forecasts. In all cases, the format must either be "hubverse" or "legacy" (i.e., the previously standardized quantile forecast format).

### Shift back for HHS hospitalization data

The data retrieved via the `get_hdgov_hosp()` function reports the count of flu hospitalizations for the prior day. The retrieval function now includes an option to shift the date back from the report date to the date of the previous day. This could impact weekly aggregation with `prep_hdgov_hosp()`. By default this "shift_back" option is set to `TRUE`.

### Probability density method for categorical forecasts

The `forecast_categorical()` function converts quantile forecasts to categorical rate change targets. Previously, the method for conversion was exclusively an interpolation procedure. In this release, the function now also supports a probability density estimation method using the `distfromq` package (https://reichlab.io/distfromq/). This option can be toggled using the "method" argument. As mentioned above, the `forecast_categorical()` function now also supports Hubverse formatting. The density method is only available for quantile forecasts given the "hubverse" format option. Likewise, the "interpolation" method is only available for quantile forecasts prepared in the "legacy" format.

### Expanded horizons

Some of the functions now accept an argument to adjust horizons in forecasting. In particular, the `pois_forc()` helper and the `forecast_categorical()` function now both can be parameterized to forecast different horizons.

### Updated rate change thresholds

The 2022-23 FluSight guidelines solicited categorical rate change forecasts for only the 2 week horizon. As of 2023-24, FluSight now solicits forecasts for all horizons. With that in mind, we retrieved updated rate change threshold data in .csv format from the FluSight repository and now include both "legacy" and "hubverse" rate change data internally. In both cases, these internal datasets provide the threshold at which the rate change should be considered an increase / decrease for each given population.

### Plausibility analysis in explorer app

As of this release, the explorer app now includes a plausibility analysis feature built using the `rplanes` package (https://signaturescience.github.io/rplanes/). The plausibility analysis is intended to help guide human forecast review via visualizatons. Currently the scoring uses default parameters, although users can define the components to use via a select input prior to running the scoring in the explorer app.

### Additional example data

Given the new Hubverse compatibility features, the package now includes data for forecasts prepared in that format to motivate examples and testing.

# fiphde 1.1.1

## New features

### Optionally bypass ILI nowcast API

The `replace_ili_nowcast()` function includes an option to "try_api", which is set to `TRUE` by default and will try to query the ILI Nearby nowcast API. If set to `FALSE`, the function will not try to query the API at all. 

# fiphde 1.1.0

## New features

### Improved API for GLM modeling

This release future-proofs the GLM modeling and forecasting functionality. In particular, the API now conforms to updates to the `trending` and `trendeval` packages, both of which are used internally inside `glm_wrap()` and its helpers. Both `trending` and `trendeval` introduced significant changes to their respective APIs in recent releases. The `fiphde` package now requires `trending` >= 0.1.0 and `trendeval` >= 0.1.0 to ensure all package functionality works as expected.

### Explorer app enhancements

As of this release the data explorer app (see `?fiphde_launcher()`) is now organized into modules for easier backend maintenance. The app also has several new features for usability, including a new CSS theme and a "busy" icon to indicate that the app is loading when data processing delays outputs from rendering.

# fiphde 1.0.0

This major release introduces new features, improved documentation throughout, automated unit tests, and a simplified API. 

## New features

### `plot_forecast_categorical()`

The package now includes a data visualization method for categorical forecast summaries via the `plot_forecast_categorical()` function. This function returns a `ggplot2` object with a stacked barplot showing the probabilities assigned to each categorical bin at every location. 

### Parity between objects returned

Package functions previously returned a mix of `data.frame` and `tibble` objects. As of this release, each such function returns a `tibble` for consistency. 

###  Messaging for `get_nowcast_ili()`

The `get_nowcast_ili()` uses the Delphi ILI Nearby API. As of this release, the API was documented as no longer being updated. The function (and any others that call this function, like `replace_ili_nowcast()`) now issue the following warning:

> As of October 2022 ILInearby was no longer being updated. This will likely return 'NA'. See https://github.com/cmu-delphi/delphi-epidata/issues/993

### Simplified API

The API for the package has been simplified to remove outmoded functions and harden arguments where needed. The list below enumerates all of these updates: 

- Removed `wis_score()` function
- Removed `get_cdc_vax()` function
- Removed `state_replace_ili_nowcast_all()` function
- Removed `submission/` directory from the source code repository on GitHub
- More intuitive handling of arguments in `ts_fit_forecast()`, including case-insenstive model names, better handling of optional covariates, and removing the inoperative "remove_null_models" argument

### Documentation

All exported functions are now robustly documented with links between package functions, detailed descriptions, and examples that include inline comments.

# fiphde 0.3.4

## New features

This release makes two minor updates to the submission script. The script now includes plots of categorical forecasts to help wit downstream forecast review. Additionally, the script is now configured to only use the quasipoisson model family in the count regression grid search procedure.

## Bug fixes

Previously the `forecast_categorical()` function would only include probabilities for the "type_id" values (e.g., increase, large_decrease, etc.) that were observed in at least one location. As of this release we now include all "type_id" values, assigning those that were not observed in any location to probability of 0.

# fiphde 0.3.3

## New features

In this release, we introduce `forecast_categorical()` to generate categorical rate change targets. The function takes a probabilistic forecast input (with values for each expected quantile) and converts it to the probability that the 2 week ahead trend will be a large decrease, decrease, increase, large increase, or stable. The function is implemented in the `fiphde` explorer Shiny app (see `?fiphde_launcher()`) such that the user can download the submission-ready categorical forecasts *and* probabilistic forecast files.

## Bug fixes

The `plot_forecast()` documentation included an example with an outmoded version of the `ts_fit_forecast()` API. We have updated the example code so that it runs with the current version of the `ts_fit_forecast()` function.

# fiphde 0.3.2

## New features

As of this release the submission script now includes code to prepare submission-ready files for "experimental" categorical rate change targets.

## Bug fixes

Previously the validation script's date format check was limited to 2021 and 2022. We have extended this check to work with 2023 dates.

The R CMD CHECK GitHub action has been incremented to use "v2" R actions, which addresses an issue with failing continuous integration checks failing for the repository.

# fiphde 0.3.1

## New features

This release includes a rearranged submission script that now executes common procedures (data retrieval, package loads, etc.) at the top of the script, so that CREG and TSENS can be run independently as needed.

## Bug fixes

Prior to this release the explorer app (see `?fiphde_launcher`) would default visualizations to *all* locations in multiple submission modeling methods loaded into the app. However, if any of the locations were available for one but not all model submission files, then the location(s) would remain selected and the forecast visualizations would break until the missing location(s) was unselected for the model. As of this release, the app now will default selected locations to just those that are present in the given submission file.

# fiphde 0.3.0

## New features

This release brings in additional data retrieval and processing functions. The package now includes code adapted from `cdcfluview` to query the NREVSS surveillance system and pull clinical laboratory percent positive flu data. The `who_nrevss()` function is unexported but is used in the user-facing `get_cdc_clin()` function, which pulls and preps the percent positive data. A complementary function, `clin_nowcast()`, is now available to augment the clinical laboratory percent positivity with a nowcasted value.

The package also now includes helper functions to prepare covariates for modeling. For example, the new `pois_forc()` helper creates a forecasted counts based on recently observed values. This can be used to create forecasted values for number of tests and number of positive flu specimens (and thefore percent positivity) to feed into models that include percent positive lab results as a covariate. The new `smoothie()` function is also available to create weighted averages of recent observations, which can be used as covariate data in models.

# fiphde 0.2.1

## Bug fixes

Minor fix to include "start_date" parameter in `replace_ili_nowcast()` function so that ILI nowcasting can be performed retrospectively if needed.

# fiphde 0.2.0

## New features

This release includes code adapted from `cdcfluview` to query ILI and flu hospitalization data. The functions to do so are `ilinet()` and `hospitalizations()`, which are unexported but used as helper functions internally. For example, `ilinet()` is used by the exported `get_cdc_ili()` function. By including these functions, we have minimized the number of dependencies for `fiphde` thereby streamlining the package installation procedure.

# fiphde 0.1.0

Initial release !
