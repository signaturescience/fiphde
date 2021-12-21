library(fiphde)
ilidat <- get_cdc_ili(region="national", years=2010:lubridate::year(lubridate::today()))
ilifor <- forecast_ili(ilidat, horizon=4L, location="US", trim_date="2020-03-01", constrained=TRUE)
ilifor$arima_params
ilifor$ili_bound %>% tail()

source(here::here("scratch/glm.R"))

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
  left_join(ilidat) %>%
  print()

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
