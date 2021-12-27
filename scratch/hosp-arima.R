library(tidyverse)
library(fiphde)

tmp_hosp_dat <-
  get_hdgov_hosp(maxdate="2021-12-26") %>%
  mutate(date = date - 1) %>%
  mutate(flu.admits = as.numeric(flu.admits),
         flu.admits.cov = as.numeric(flu.admits.cov)) %>%
  mutate(epiweek = lubridate::epiweek(date),
         epiyear = lubridate::epiyear(date)) %>%
  group_by(epiyear, epiweek) %>%
  summarise(flu.admits = sum(flu.admits, na.rm = TRUE),
            flu.admits.cov = sum(flu.admits.cov, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(location = "US", .before = "epiweek")

## date filter
tmp_hosp_dat <-
  tmp_hosp_dat %>%
  mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  ## 2020-10-11 is the date of the *last epiweek for which hospitals werent regularly reporting
  filter(date > as.Date("2020-10-11", format = "%Y-%m-%d"))

train_hosp_dat <- tmp_hosp_dat %>% filter(row_number() < n() - 3)
test_hosp_dat <- tmp_hosp_dat %>% filter(row_number() >= n() - 3)

hospdat_tsibble <-
  train_hosp_dat %>%
  make_tsibble(epiyear = epiyear, epiweek = epiweek, key=location, chop=FALSE)

hosp_fit <- fabletools::model(hospdat_tsibble,
                             arima = fable::ARIMA(flu.admits,
                                                  stepwise=TRUE,
                                                  approximation=NULL))

hosp_fit


myforecast <- fabletools::forecast(hosp_fit, h=4)

q5 <-
  myforecast %>%
  tibble::as_tibble() %>%
  dplyr::transmute(.model, yweek, location, quantile=0.5, value=.mean, type="quantile")

point_estimates <-
  myforecast %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(quantile=NA_real_, .after=yweek) %>%
  dplyr::mutate(type="point") %>%
  dplyr::rename(value=.mean) %>%
  dplyr::select(.model, yweek, location, quantile, value, type)

forcs <-
  myforecast %>%
  fabletools::hilo(sort(unique(focustools:::quidk$interval))) %>%
  fabletools::unpack_hilo(dplyr::ends_with("%")) %>%
  tidyr::gather(key, value, dplyr::contains("%")) %>%
  dplyr::inner_join(focustools:::quidk, by="key") %>%
  tibble::as_tibble() %>%
  dplyr::transmute(.model, yweek, location, quantile, value, type="quantile") %>%
  dplyr::bind_rows(q5) %>%
  dplyr::bind_rows(point_estimates) %>%
  dplyr::arrange(yweek, quantile) %>%
  dplyr::mutate(epiweek = epiweek(yweek),
                epiyear = epiyear(yweek)) %>%
  select(epiweek, epiyear, quantile, value)

plot_forc(forcs, train_hosp_dat, test_hosp_dat)
