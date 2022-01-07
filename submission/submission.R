library(tidyverse)
library(fiphde)
library(furrr)


################################################################################
## SigSci-CREG

## first an option
## do we want to log-transform ILI ???
tologili <- TRUE
# Get data
# the years argument for cdcfluview::ilinet gets the *season* corresponding to the year.
# so, 2019 = the 2019-2020 flu season. If you only get 2020-2021, you're getting the
# 2020-2021 and 2021-2022 flu season, meaning that you will *NOT* capture early 2020 data.
ilidat <- get_cdc_ili(region=c("national","state"), years=2019:lubridate::year(lubridate::today()))
iliaug <- replace_ili_nowcast(ilidat, weeks_to_replace=1)
ilidat_st <- iliaug %>% dplyr::filter(region_type=="States")
ilifor_st <- forecast_ili(ilidat_st, horizon=4L, trim_date="2020-03-01")

hosp <- get_hdgov_hosp(limitcols = TRUE)

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
  ## optionally log tranform ILI ???
  ## see above
  ## NOTE: we *need* to add a column with the logical so eval doesnt recycle ili
  mutate(iliopt = tologili) %>%
  mutate(ili = ifelse(iliopt, log(ili), ili)) %>%
  select(-iliopt) %>%
  group_split(., location)

models <-
  list(
    poisson1 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "poisson"),
    poisson2 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "poisson"),
    # poisson3 = trending::glm_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank, family = "poisson"),
    # poisson4 = trending::glm_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "poisson"),
    quasipoisson1 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "quasipoisson"),
    quasipoisson2 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "quasipoisson"),
    # quasipoisson3 = trending::glm_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank, family = "quasipoisson"),
    # quasipoisson4 = trending::glm_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "quasipoisson"),
    negbin1 = trending::glm_nb_model(flu.admits ~ ili + hosp_rank + ili_rank),
    negbin2 = trending::glm_nb_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov))
    # negbin3 = trending::glm_nb_model(flu.admits ~ lag_1 + ili + hosp_rank + ili_rank),
    # negbin4 = trending::glm_nb_model(flu.admits ~ lag_1 + ili + ili_rank + hosp_rank + offset(flu.admits.cov))
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
      select(-epiweek,-epiyear) %>%
      mutate(ili = log(ili))

    tmp_res <- glm_wrap(dat,
                        new_covariates = new_cov,
                        .models = models,
                        alpha = c(0.01, 0.025, seq(0.05, 0.5, by = 0.05)) * 2)

    tmp_res$forecasts$location <- unique(dat$location)

    approach <- paste0("GLM-", as.character(tmp_res$model$fit$fitted_model$family)[1],
                       "\n",
                       paste0(names(tmp_res$model$fit$fitted_model$coefficients), collapse = " + "))

    # future_dat <-
    #   new_cov %>%
    #   mutate(flu.admits = NA) %>%
    #   mutate(date = max(dat$date) + c(7,14,21,28)) %>%
    #   mutate(epiweek = lubridate::epiweek(date), epiyear = lubridate::epiyear(date))

    # p.hosp <- plot_forc(tmp_res$forecasts, dat, future_dat) +
    #   labs(caption = paste0(unique(dat$abbreviation), "\n", approach, "\n", max(dat$date))) +
    #   theme(plot.caption = element_text(hjust = 0))

    list(
      location = unique(dat$location),
      location_abb = unique(dat$abbreviaton),
      results = tmp_res,
      # data = list(training = dat, testing = future_dat),
      data = list(training = dat),
      # plots = list(p.hosp = p.hosp),
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
      # plots = NA,
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
  left_join(ilifor_us$ilidat, by = c("epiyear", "location", "epiweek")) %>%
  ## optionally log tranform ILI ???
  ## see above
  ## NOTE: we *need* to add a column with the logical so eval doesnt recycle ili
  mutate(iliopt = tologili) %>%
  mutate(ili = ifelse(iliopt, log(ili), ili)) %>%
  select(-iliopt)

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
  select(-epiweek,-epiyear) %>%
  mutate(ili = log(ili))

res <- glm_wrap(dat_us,
                new_covariates = new_cov,
                .models = models,
                alpha = c(0.01, 0.025, seq(0.05, 0.5, by = 0.05)) * 2)

res$forecasts$location <- "US"

us_approach <- paste0("GLM-", as.character(res$model$fit$fitted_model$family)[1],
                   "\n",
                   paste0(names(res$model$fit$fitted_model$coefficients), collapse = " + "))

us_forcres <-
  list(
    list(
      location = "US",
      location_abb = "US",
      results = res,
      data = list(training = dat_us),
      thru_week = max(dat_us$date),
      approach = us_approach)
  )

state_locs <-
  forcres %>%
  purrr::keep(~!is.na(.x$approach)) %>%
  purrr::map_chr(., "location")

state_forecasts <-
  forcres %>%
  purrr::keep(~!is.na(.x$approach)) %>%
  purrr::map(., "results") %>%
  purrr::map(., "forecasts")

state_glm_prepped <- map_df(state_forecasts, ~format_for_submission(.x, method = "CREG"))
us_glm_prepped <- format_for_submission(res$forecasts, method = "CREG")
all_prepped <- bind_rows(us_glm_prepped$CREG,state_glm_prepped$CREG)

## force to monday (required to validate a forecast created on a day other than sunday or monday)
all_prepped$forecast_date <- this_monday()

validate_forecast(all_prepped)

all_prepped %>%
  write_csv(., paste0("submission/SigSci-CREG/", this_monday(), "-SigSci-CREG.csv"))

bound_truth <-
  do.call("rbind", datl) %>%
  bind_rows(., dat_us)

pdf(paste0("~/Downloads/SigSci-CREG-", this_monday(), ".pdf"), width=11.5, height=8)
for(loc in unique(all_prepped$location)) {
  p <- plot_forecast(bound_truth, all_prepped, location = loc)
  print(p)
}
dev.off()

################################################################################
## SigSci-TSENS

# Prep, and make a tsibble
## NOTE: uses hosp from above but preps slightly differently
prepped_hosp <-
  hosp %>%
  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0) %>%
  dplyr::filter(abbreviation != "DC")

prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
                                     epiyear = epiyear,
                                     epiweek=epiweek,
                                     key=location)
# Fit a model
hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               constrained=TRUE,
                               covariates=c("hosp_rank", "ili_rank"))

# format for submission
formatted_list <- format_for_submission(hosp_fitfor$tsfor, method = "ts")

## ARIMA
## force to monday (required to validate a forecast created on a day other than sunday or monday)
formatted_list$arima$forecast_date <- this_monday()
validate_forecast(formatted_list$arima)
formatted_list$arima %>%
  write_csv(., paste0("submission/SigSci-TSENS/", this_monday(), "-SigSci-TSENS-ARIMA.csv"))

pdf(paste0("~/Downloads/SigSci-TSENS-ARIMA-", this_monday(), ".pdf"), width=11.5, height=8)
for(loc in unique(formatted_list$arima$location)) {
  p <- plot_forecast(prepped_hosp, formatted_list$arima, location = loc)
  print(p)
}
dev.off()

## ETS
## force to monday (required to validate a forecast created on a day other than sunday or monday)
formatted_list$ets$forecast_date <- this_monday()
validate_forecast(formatted_list$ets)
formatted_list$ets %>%
  write_csv(., paste0("submission/SigSci-TSENS/", this_monday(), "-SigSci-TSENS-ETS.csv"))

pdf(paste0("~/Downloads/SigSci-TSENS-ETS-", this_monday(), ".pdf"), width=11.5, height=8)
for(loc in unique(formatted_list$ets$location)) {
  p <- plot_forecast(prepped_hosp, formatted_list$ets, location = loc)
  print(p)
}
dev.off()

## ensemble
## force to monday (required to validate a forecast created on a day other than sunday or monday)
formatted_list$ensemble$forecast_date <- this_monday()
validate_forecast(formatted_list$ensemble)
formatted_list$ensemble %>%
  write_csv(., paste0("submission/SigSci-TSENS/", this_monday(), "-SigSci-TSENS-ensemble.csv"))

pdf(paste0("~/Downloads/SigSci-TSENS-ensemble-", this_monday(), ".pdf"), width=11.5, height=8)
for(loc in unique(formatted_list$ensemble$location)) {
  p <- plot_forecast(prepped_hosp, formatted_list$ensemble, location = loc)
  print(p)
}
dev.off()


################################################################################
## save model formulas / arima params / objects for posterity

ili_params <-
  bind_rows(ilifor_us$arima_params, ilifor_st$arima_params) %>%
  mutate(forecast_date = this_monday())

hosp_arima_params <-
  map(hosp_fitfor$tsfit$arima, "fit") %>%
  map_df("spec") %>%
  mutate(location = hosp_fitfor$tsfit$location, .before = "p") %>%
  mutate(forecast_date = this_monday())

hosp_ets_formula <- hosp_fitfor$ets_formula

glm_forcres <- c(forcres, us_forcres)
glm_model_info <-
  glm_forcres %>%
  map("results") %>%
  map_df("model") %>%
  mutate(forecast_date = this_monday())

save(glm_forcres, glm_model_info, file = paste0("~/Downloads/SigSci-CREG-model-info-", this_monday(), ".rda"))
save(ili_params,hosp_arima_params, hosp_ets_formula, file = paste0("~/Downloads/SigSci-TSENS-model-info-", this_monday(), ".rda"))
