library(tidyverse)
library(fiphde)
library(furrr)

# Get data
# the years argument for cdcfluview::ilinet gets the *season* corresponding to the year.
# so, 2019 = the 2019-2020 flu season. If you only get 2020-2021, you're getting the
# 2020-2021 and 2021-2022 flu season, meaning that you will *NOT* capture early 2020 data.
ilidat <- get_cdc_ili(region=c("national","state"), years=2019:lubridate::year(lubridate::today()))
iliaug <- replace_ili_nowcast(ilidat, weeks_to_replace=1)
ilidat_st <- iliaug %>% dplyr::filter(region_type=="States")
ilifor_st <- forecast_ili(ilidat_st, horizon=4L, trim_date="2020-03-01")

hosp <- get_hdgov_hosp(maxdate="2021-12-29")

## data list by location
datl <-
  prep_hdgov_hosp(hosp, min_per_week = 0) %>%
  mutate(lag_1 = lag(flu.admits, 1)) %>%
  filter(!is.na(lag_1)) %>%
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  ## states only (not US or DC) and FL doesnt have ILI data
  ## TODO: add nowcast ILI data for FL so we can forecast?
  filter(!abbreviation %in% c("US","FL","DC")) %>%
  left_join(ilifor_st$ilidat, by = c("epiyear", "location", "epiweek")) %>%
  group_split(., location)

models <-
  list(
    poisson1 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "poisson"),
    poisson2 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "poisson"),
    poisson3 = trending::glm_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank, family = "poisson"),
    poisson4 = trending::glm_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "poisson"),
    quasipoisson1 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "quasipoisson"),
    quasipoisson2 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "quasipoisson"),
    quasipoisson3 = trending::glm_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank, family = "quasipoisson"),
    quasipoisson4 = trending::glm_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "quasipoisson"),
    negbin1 = trending::glm_nb_model(flu.admits ~ ili + hosp_rank + ili_rank),
    negbin2 = trending::glm_nb_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov)),
    negbin3 = trending::glm_nb_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank),
    negbin4 = trending::glm_nb_model(flu.admits ~ lag_1 + ili + ili_rank + hosp_rank + offset(flu.admits.cov))
  )

## use furrr mapping to speed up
run_forc <- function(dat) {
  tryCatch({
    message(unique(dat$abbreviation))

    new_cov <-
      ilifor_st$ili_future %>%
      filter(location %in% unique(dat$location)) %>%
      left_join(fiphde:::historical_severity) %>%
      ## assume the coverage will be the average of the last 8 weeks of reporting
      bind_cols(.,tibble(flu.admits.cov = rep(mean(dat$flu.admits.cov,8), 4))) %>%
      select(-epiweek,-epiyear)

    tmp_res <- glm_wrap(dat,
                        new_covariates = new_cov,
                        .models = models,
                        alpha = c(0.01, 0.025, seq(0.05, 0.5, by = 0.05)) * 2)

    approach <- paste0("GLM-", as.character(tmp_res$model$fit$fitted_model$family)[1],
                       "\n",
                       paste0(names(tmp_res$model$fit$fitted_model$coefficients), collapse = " + "))

    future_dat <-
      new_cov %>%
      mutate(flu.admits = NA) %>%
      mutate(date = max(dat$date) + c(7,14,21,28)) %>%
      mutate(epiweek = lubridate::epiweek(date), epiyear = lubridate::epiyear(date))

    p.hosp <- plot_forc(tmp_res$forecasts, dat, future_dat) +
      labs(caption = paste0(unique(dat$abbreviation), "\n", approach, "\n", max(dat$date))) +
      theme(plot.caption = element_text(hjust = 0))

    list(
      location = unique(dat$location),
      location_abb = unique(dat$abbreviaton),
      results = tmp_res,
      data = list(training = dat, testing = future_dat),
      plots = list(p.hosp = p.hosp),
      thru_week = max(dat$date),
      approach = approach)

  },
  error = function(cond) {
    message("Skipping location due to error ... ")
    list(
      location = unique(dat$location),
      location_abb = unique(dat$abbreviaton),
      results = NA,
      data = NA,
      plots = NA,
      thru_week = NA,
      approach = NA)
  })
}

plan(multisession, workers = 10)
system.time({
  forcres <- future_map(datl, ~run_forc(.x))
})

## see below (after national forecasting) for formatting of state forecasts

############################################################################
## now we need to get the national forecasts

## use ilidat from above
ilidat_us <- iliaug %>% dplyr::filter(location=="US")
ilifor_us <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01")

## data list by location
dat_us <-
  prep_hdgov_hosp(hosp, min_per_week = 0) %>%
  mutate(lag_1 = lag(flu.admits, 1)) %>%
  filter(!is.na(lag_1)) %>%
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  filter(abbreviation == "US") %>%
  left_join(ilifor_us$ilidat, by = c("epiyear", "location", "epiweek"))

models <-
  list(
    poisson1 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "poisson"),
    poisson2 = trending::glm_model(flu.admits ~ ili + hosp_rank + offset(flu.admits.cov), family = "poisson"),
    poisson3 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "poisson"),
    quasipoisson1 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "quasipoisson"),
    quasipoisson2 = trending::glm_model(flu.admits ~ ili + hosp_rank + offset(flu.admits.cov), family = "quasipoisson"),
    quasipoisson3 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "quasipoisson"),
    negbin1 = trending::glm_nb_model(flu.admits ~ ili + hosp_rank + ili_rank),
    negbin2 = trending::glm_nb_model(flu.admits ~  ili + hosp_rank + offset(flu.admits.cov)),
    negbin3 = trending::glm_nb_model(flu.admits ~  ili + hosp_rank + ili_rank + offset(flu.admits.cov))
  )

new_cov <-
  ilifor_us$ili_future %>%
  filter(location %in% unique(dat_us$location)) %>%
  left_join(fiphde:::historical_severity) %>%
  ## assume the coverage will be the average of the last 8 weeks of reporting
  bind_cols(.,tibble(flu.admits.cov = rep(mean(dat_us$flu.admits.cov,8), 4))) %>%
  select(-epiweek,-epiyear)

res <- glm_wrap(dat_us,
                new_covariates = new_cov,
                .models = models,
                alpha = c(0.01, 0.025, seq(0.05, 0.5, by = 0.05)) * 2)


subform <- function(.forecasts, .location) {

  .forecasts %>%
    dplyr::arrange(epiyear,epiweek) %>%
    dplyr::group_by(epiyear,epiweek) %>%
    dplyr::mutate(horizon = dplyr::group_indices()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(target = paste0(horizon, " wk ahead inc flu hosp")) %>%
    dplyr::mutate(target_end_date = MMWRweek::MMWRweek2Date(epiyear,epiweek,7)) %>%
    dplyr::mutate(forecast_date = lubridate::today()) %>%
    dplyr::mutate(type = ifelse(is.na(quantile), "point", "quantile")) %>%
    dplyr::mutate(location = .location) %>%
    dplyr::mutate(quantile = round(quantile,3)) %>%
    dplyr::select(forecast_date,target,target_end_date,location,type,quantile,value) %>%
    ## call to distinct to get rid of duplicated 0.5 quantile
    ## BETTER WAY TO DO THIS ??
    dplyr::distinct()
}


state_locs <-
  forcres %>%
  purrr::keep(~!is.na(.x$approach)) %>%
  purrr::map_chr(., "location")

state_forecasts <-
  forcres %>%
  purrr::keep(~!is.na(.x$approach)) %>%
  purrr::map(., "results") %>%
  purrr::map(., "forecasts")

state_glm_prepped <- map2_df(.x = state_forecasts, .y = state_locs, ~subform(.x, .y))
us_glm_prepped <- subform(res$forecasts, "US")

bind_rows(us_glm_prepped,state_glm_prepped) %>%
  write_csv(., paste0("submission/SigSci-CREG/", lubridate::today(), "-SigSci-CREG.csv"))
