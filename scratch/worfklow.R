library(tidyverse)
library(fiphde)

# Get data
# the years argument for cdcfluview::ilinet gets the *season* corresponding to the year.
# so, 2019 = the 2019-2020 flu season. If you only get 2020-2021, you're getting the
# 2020-2021 and 2021-2022 flu season, meaning that you will *NOT* capture early 2020 data.
ilidat <- get_cdc_ili(region="national", years=2019:lubridate::year(lubridate::today()))

# Subset to US only
ilidat_us <- ilidat %>% filter(location=="US")

# Forecast ILI
ilifor <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01", constrained=TRUE)

# What are the arima params?
ilifor$arima_params

# Take a look at the forecasted data
ilifor$ili_future

# Take a look at the forecasted data bound do the real data
ilifor$ili_bound %>% tail(8)

# Plot actual versus forecasted values
p.ili <-
  ilifor$ili_bound %>%
  mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>%
  ggplot(aes(date, ili)) +
  geom_line(alpha=.5, lwd=.2) +
  geom_point(aes(col=forecasted)) +
  theme_bw() +
  labs(x="Date", y="Unweighted ILI", title="ILI forecast")
p.ili

hosp <- get_hdgov_hosp(mindate="2021-04-01", maxdate="2021-12-25")

tmp_weekly_flu <-
  hosp %>%
  mutate(date = date - 1) %>%
  mutate(flu.admits = as.numeric(flu.admits),
         flu.admits.cov = as.numeric(flu.admits.cov)) %>%
  mutate(epiweek = lubridate::epiweek(date),
         epiyear = lubridate::epiyear(date)) %>%
  group_by(epiyear, epiweek) %>%
  summarise(flu.admits = sum(flu.admits, na.rm = TRUE),
            flu.admits.cov = sum(flu.admits.cov, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(location = "US", .before = "epiweek") %>%
  left_join(ilifor$ilidat, by = c("epiyear", "location", "epiweek"))

## add lag columns
tmp_weekly_flu_w_lag <-
  tmp_weekly_flu %>%
  mutate(lag_1 = lag(flu.admits, 1)) %>%
  filter(complete.cases(.)) %>%
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  inner_join(fiphde:::historical_severity, by="epiweek") %>%
  filter(date >= max(date) - 7*24)
  print()
tmp_weekly_flu_w_lag

train_dat <- tmp_weekly_flu_w_lag %>% filter(row_number() < n() - 3)
test_dat <- tmp_weekly_flu_w_lag %>% filter(row_number() >= n() - 3)

models <-
  list(
    # glm_poisson_ili = trending::glm_model(flu.admits ~ ili, family = "poisson"),
    # glm_quasipoisson_ili = trending::glm_model(flu.admits ~ ili, family = "quasipoisson"),
    # glm_negbin_ili = trending::glm_nb_model(flu.admits ~ ili),
    # glm_poisson_ili_offset = trending::glm_model(flu.admits ~ ili + offset(flu.admits.cov), family = "poisson"),
    # glm_quasipoisson_ili_offset = trending::glm_model(flu.admits ~ ili + offset(flu.admits.cov), family = "quasipoisson"),
    # glm_negbin_ili_offset = trending::glm_nb_model(flu.admits ~ ili + offset(flu.admits.cov)),
    # glm_poisson_ili_lags = trending::glm_model(flu.admits ~ ili + lag_1 + lag_2 + lag_3 + lag_4, family = "poisson"),
    # glm_quasipoisson_ili_lags = trending::glm_model(flu.admits ~ ili + lag_1 + lag_2 + lag_3 + lag_4, family = "quasipoisson"),
    # glm_negbin_lags_means = trending::glm_nb_model(flu.admits ~ lag_1 + ili_mean + hosp_mean),
    glm_negbin_lags_ranks = trending::glm_nb_model(flu.admits ~ lag_1 + ili_rank + hosp_rank),
    glm_negbin_ili_lags_ranks = trending::glm_nb_model(flu.admits ~ ili + lag_1 + ili_rank + hosp_rank),
    glm_negbin_ili_lags_ranks_offset = trending::glm_nb_model(flu.admits ~ ili + lag_1 + ili_rank + hosp_rank + offset(flu.admits.cov))
    # glm_negbin_ili_lags = trending::glm_nb_model(flu.admits ~ ili + lag_1 + lag_2 + lag_3 + lag_4),
    # glm_poisson_ili_lags_offset = trending::glm_model(flu.admits ~ ili + lag_1 + lag_2 + lag_3 + lag_4 + offset(flu.admits.cov), family = "poisson"),
    # glm_quasipoisson_ili_lags_offset = trending::glm_model(flu.admits ~ ili + lag_1 + lag_2 + lag_3 + lag_4 + offset(flu.admits.cov), family = "quasipoisson"),
    # glm_negbin_ili_lags_offset = trending::glm_nb_model(flu.admits ~ ili + lag_1 + lag_2 + lag_3 + lag_4 + offset(flu.admits.cov))
  )

res <- glm_wrap(train_dat,
                new_covariates = tibble(flu.admits.cov = rep(tail(train_dat$flu.admits.cov,1), 4),
                                        ili_rank=test_dat$ili_rank,
                                        hosp_rank=test_dat$hosp_rank,
                                        ili_mean=test_dat$ili_mean,
                                        hosp_mean=test_dat$hosp_mean,
                                        ili=test_dat$ili),
                .models = models,
                alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2)

res$forecasts
res$model

p.hosp <- plot_forc(res$forecasts, train_dat, test_dat)
p.hosp

library(patchwork)
p.ili / p.hosp
