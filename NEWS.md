# fiphde 0.2.0

## New features

This release includes code adapted from `cdcfluview` to query ILI and flu hospitalization data. The functions to do so are `ilinet()` and `hospitalizations()`, which are unexported but used as helper functions internally. For example, `ilinet()` is used by the exported `get_cdc_ili()` function. By including these functions, we have minimized the number of dependencies for `fiphde` thereby streamlining the package installation procedure.

# fiphde 0.1.0

Initial release !

Built and checked successfully on Ubuntu 20.04, Windows Server 2022, Mac OSX.
