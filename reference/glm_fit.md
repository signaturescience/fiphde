# Fit glm models

This helper function is used in
[glm_wrap](https://signaturescience.github.io/fiphde/reference/glm_wrap.md)
to fit a list of models and select the best one. The model selection
procedure will use the root mean square error (RMSE) metric implemented
in
[yardstick::rmse](https://yardstick.tidymodels.org/reference/rmse.html)
to select the best model.

## Usage

``` r
glm_fit(.data, .models, complete = TRUE)
```

## Arguments

- .data:

  Data including all explanatory and outcome variables needed for
  modeling; must include column for "location"

- .models:

  List of models defined as
  [trending::trending_model](https://rdrr.io/pkg/trending/man/trending_model.html)
  objects

- complete:

  Logical as to whether or not all observations for covariates must be
  available in a given model; default is `TRUE`

## Value

A `tibble` containing characteristics from the "best" `glm` model
including:

- **model_class**: The "type" of model for the best fit

- **fit**: The fitted model object for the best fit stored as a list
  column

- **location**: The geographic unit being modeled

- **data**: Original model fit data as a `tibble` in a list column
