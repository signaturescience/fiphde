# Run glm modeling and forecasting

This is a wrapper function that pipelines influenza hospitalization
modeling
([glm_fit](https://signaturescience.github.io/fiphde/reference/glm_fit.md))
and forecasting
([glm_forecast](https://signaturescience.github.io/fiphde/reference/glm_forecast.md)).

## Usage

``` r
glm_wrap(
  .data,
  .models,
  new_covariates = NULL,
  horizon = 4,
  alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2
)
```

## Arguments

- .data:

  Data including all explanatory and outcome variables needed for
  modeling

- .models:

  List of models defined as
  [trending::trending_model](https://rdrr.io/pkg/trending/man/trending_model.html)
  objects

- new_covariates:

  A `tibble` with one column per covariate, and n rows for n horizons
  being forecasted

- horizon:

  Number of weeks ahead for forecasting

- alpha:

  Vector specifying the threshold(s) to be used for prediction intervals
  (PI); alpha of `0.05` would correspond to 95% PI; default is
  `c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2` to create a range of
  intervals

## Value

Named list with two elements:

- **model**: Output from
  [glm_fit](https://signaturescience.github.io/fiphde/reference/glm_fit.md)
  with selected model fit

- **forecasts**: Output from
  [glm_forecast](https://signaturescience.github.io/fiphde/reference/glm_forecast.md)
  with forecasts from each horizon combined as a single tibble

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve data to be used in fitting models
hosp_va <-
 get_hdgov_hosp(limitcols=TRUE) %>%
 prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
 dplyr::filter(abbreviation == "VA")

# Define list of models
models <-
 list(
   poisson = trending::glm_model(flu.admits ~ hosp_rank + ili_rank, family = "poisson"),
   quasipoisson = trending::glm_model(flu.admits ~ hosp_rank + ili_rank, family = "quasipoisson"),
   negbin = trending::glm_nb_model(flu.admits ~ hosp_rank + ili_rank)
 )

# Create new covariate data to feed into forecast procedure
new_cov <-
  dplyr::tibble(
    date = max(hosp_va$week_start) + c(7,14,21,28),
    epiweek = lubridate::epiweek(date),
    epiyear = lubridate::epiyear(date)
  ) %>%
  dplyr::left_join(
    fiphde:::historical_severity, by="epiweek"
  ) %>%
  dplyr::select(-epiweek,-epiyear)

# Run the glm wrapper to fit and forecast
va_glm_res <- glm_wrap(.data = hosp_va, .models = models, new_covariates = new_cov, horizon = 4)
va_glm_res

} # }
```
