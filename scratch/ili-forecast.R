library(fiphde)
# library(tidyverse)
# library(lubridate)
# library(focustools)
# library(fabletools)
# library(fable)

### Function arguments
#' @param trim_date Earliest start date you want to use for ILI data
#' @param horizon Optional horizon periods through which the forecasts should be generated; default is `4`
#' @param region Either "national" or "state" or c("national", "state") for both national and state-level data.
#' @param location Vector specifying locations to filter to; `'US'` by default.
trim_date="2020-03-01"
horizon=4
region="national"
location="US"

# Set the years passed to get_cdc_ili starting with the year of the trim_date to the current year
years <- lubridate::year(trim_date):lubridate::year(lubridate::today())

## retreive ILI data
ilidat_all <- get_cdc_ili(region=region, years=years)

## subset to US only and subset to the trim date to present
ilidat <-
  ilidat_all %>%
  dplyr::filter(location %in% location) %>%
  dplyr::filter(week_start > as.Date(trim_date, format = "%Y-%m-%d")) %>%
  dplyr::select(location, year, week, weighted_ili)

## make a tsibble. do not chop the last week - because this is weekly data we won't have an incomplete final week
ilidat_tsibble <-
  ilidat %>%
  fiphde::make_tsibble(epiyear = year, epiweek = week, chop=FALSE)
tail(ilidat_tsibble)

# Nonseasonal fit: PDQ(0, 0, 0)
# Nonseasonal components unrestricted: pdq(0:5,0:5,0:5)
ili_fit <- fabletools::model(ilidat_tsibble,
                             arima = fable::ARIMA(weighted_ili ~ PDQ(0,0,0) + pdq(0:5,0:5,0:5),
                                                  stepwise=FALSE,
                                                  approximation=FALSE))
# ili_fit %>% focustools::extract_arima_params()

## oddly this WORKS (even though the outcome is not icases) ... need to fix that behavior in focustools
# ili_forecast <- focustools::ts_forecast(ili_fit, outcome = "icases")

# Get the forecast
ili_forecast <- fabletools::forecast(ili_fit, h=horizon)

## Look at the quantiles
# ili_forecast %>%
#   fabletools::hilo()
# ili_forecast %>%
#   fabletools::hilo() %>%
#   fabletools::unpack_hilo(`80%`) %>%
#   fabletools::unpack_hilo(`95%`)

# Get the next #horizon weeks in a tibble
ili_future <- ili_forecast %>%
  tibble::as_tibble() %>%
  dplyr::mutate(year=lubridate::epiyear(yweek)) %>%
  dplyr::mutate(week=lubridate::epiweek(yweek)) %>%
  dplyr::select(location, year, week, weighted_ili=.mean)


ili_bound <- dplyr::bind_rows(ilidat     %>% dplyr::mutate(forecasted=FALSE),
                              ili_future %>% dplyr::mutate(forecasted=TRUE))

return(ili_bound)

source("glm.R")

hosp <- get_hdgov_hosp(mindate="2021-04-01", maxdate="2021-12-12")

tmp_weekly_flu <-
  hosp %>%
  mutate(date = date - 1) %>%
  mutate(epiweek = lubridate::epiweek(date),
         epiyear = lubridate::epiyear(date)) %>%
  group_by(epiweek, epiyear) %>%
  summarise(flu.admits = sum(flu.admits, na.rm = TRUE),
            flu.admits.cov = sum(flu.admits.cov, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(location = "US", .before = "epiweek") %>%
  left_join(ilidat)

## add lag columns
tmp_weekly_flu_w_lag <-
  tmp_weekly_flu %>%
  mutate(lag_1 = lag(flu.admits, 1),
         lag_2 = lag(flu.admits, 2),
         lag_3 = lag(flu.admits, 3),
         lag_4 = lag(flu.admits, 4)) %>%
  filter(complete.cases(.)) %>%
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  filter(date >= max(date) - 7*24)

train_dat <- tmp_weekly_flu_w_lag %>% filter(row_number() < n() - 3)
test_dat <- tmp_weekly_flu_w_lag %>% filter(row_number() >= n() - 3)

models <-
  list(
    glm_poisson_ili = trending::glm_model(flu.admits ~ weighted_ili, family = "poisson"),
    glm_quasipoisson_ili = trending::glm_model(flu.admits ~ weighted_ili, family = "quasipoisson"),
    glm_negbin_ili = trending::glm_nb_model(flu.admits ~ weighted_ili),
    glm_poisson_ili_offset = trending::glm_model(flu.admits ~ weighted_ili + offset(flu.admits.cov), family = "poisson"),
    glm_quasipoisson_ili_offset = trending::glm_model(flu.admits ~ weighted_ili + offset(flu.admits.cov), family = "quasipoisson"),
    glm_negbin_ili_offset = trending::glm_nb_model(flu.admits ~ weighted_ili + offset(flu.admits.cov)),
    glm_poisson_ili_lags = trending::glm_model(flu.admits ~ weighted_ili + lag_1 + lag_2 + lag_3 + lag_4, family = "poisson"),
    glm_quasipoisson_ili_lags = trending::glm_model(flu.admits ~ weighted_ili + lag_1 + lag_2 + lag_3 + lag_4, family = "quasipoisson"),
    glm_negbin_ili_lags = trending::glm_nb_model(flu.admits ~ weighted_ili + lag_1 + lag_2 + lag_3 + lag_4),
    glm_poisson_ili_lags_offset = trending::glm_model(flu.admits ~ weighted_ili + lag_1 + lag_2 + lag_3 + lag_4 + offset(flu.admits.cov), family = "poisson"),
    glm_quasipoisson_ili_lags_offset = trending::glm_model(flu.admits ~ weighted_ili + lag_1 + lag_2 + lag_3 + lag_4 + offset(flu.admits.cov), family = "quasipoisson"),
    glm_negbin_ili_lags_offset = trending::glm_nb_model(flu.admits ~ weighted_ili + lag_1 + lag_2 + lag_3 + lag_4 + offset(flu.admits.cov))
  )

res <- glm_wrap(train_dat,
                ## NOTE: hardcoding ili covariates ... butshould actually get from forecasts above
                new_covariates = tibble(flu.admits.cov = rep(tail(train_dat$flu.admits.cov,1), 4), weighted_ili = c(2.31,2.43,2.45,2.68)),
                .models = models,
                alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2)

res$forecasts
res$model

plot_forc(res$forecasts, train_dat, test_dat)
