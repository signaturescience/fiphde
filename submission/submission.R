library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(fiphde)
library(furrr)

################################################################################
## SigSci-CREG

## first an option
## do we want to log-transform ILI ???
tologili <- TRUE
## another option for the number of cores to use in parallelization
## use almost all of the cores you have (-2)
n_workers <- max(1, parallel::detectCores()-2)
## or alterantively just set at a value (eg 4)
# n_workers <- 4
## another option whether or not to use remove_incomplete feature in prepping hdgov hosp
## if this is set to TRUE it will expect ...
## the hospitalization data will be reported for the entire last week
## BUT this will break the code if we try to run on a sunday
## last week
if(as.POSIXlt(lubridate::today())$wday == 0) {
  ri <- FALSE
} else {
  ri <-  TRUE
}
## if there are lots of warnings we want to print them in the pipeline script
## especially non-interactively (i.e. when the automated instance runs the script)
if(!interactive()) {
  message("setting option for max number of warnings printed to be 10000 ... ")
  options(nwarnings = 10000)
}
# Get data
# the years argument for ilinet gets the *season* corresponding to the year.
# so, 2019 = the 2019-2020 flu season. If you only get 2020-2021, you're getting the
# 2020-2021 and 2021-2022 flu season, meaning that you will *NOT* capture early 2020 data.
ilidat <- get_cdc_ili(region=c("national","state"), years=2019:lubridate::year(lubridate::today()))
ilidat <- state_replace_ili_nowcast_all(ilidat, state="FL")
iliaug <- replace_ili_nowcast(ilidat, weeks_to_replace=1)
ilidat_st <- iliaug %>% dplyr::filter(region_type=="States")
ilifor_st <- forecast_ili(ilidat_st, horizon=4L, trim_date="2020-03-01", stepwise=FALSE, approximation=FALSE)

hosp <- get_hdgov_hosp(limitcols = TRUE)

# If using log(ili), make all the zeros be the minimum nonzero value
if (tologili) {
  ilidat    <- ilidat    %>% mutate(weighted_ili=mnz_replace(weighted_ili))
  iliaug    <- iliaug    %>% mutate(weighted_ili=mnz_replace(weighted_ili))
  ilidat_st <- ilidat_st %>% mutate(weighted_ili=mnz_replace(weighted_ili))
  ilifor_st$ilidat     <- ilifor_st$ilidat     %>% mutate(ili=mnz_replace(ili))
  ilifor_st$ili_future <- ilifor_st$ili_future %>% mutate(ili=mnz_replace(ili))
  ilifor_st$ili_bound  <- ilifor_st$ili_bound  %>% mutate(ili=mnz_replace(ili))
}

## data list by location
datl <-
  prep_hdgov_hosp(hosp, min_per_week = 0, remove_incomplete = ri) %>%
  mutate(lag_1 = lag(flu.admits, 1)) %>%
  filter(!is.na(lag_1)) %>%
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  ## states only (not US or DC)
  filter(!abbreviation %in% c("US","DC")) %>%
  left_join(ilifor_st$ilidat, by = c("epiyear", "location", "epiweek")) %>%
  ## optionally log tranform ILI ???
  ## see above
  ## NOTE: we *need* to add a column with the logical so eval doesnt recycle ili
  mutate(iliopt = tologili) %>%
  mutate(ili = ifelse(iliopt, log(ili), ili)) %>%
  select(-iliopt) %>%
  mutate(flu.admits.cov=log(flu.admits.cov)) %>%
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

plan(multisession, workers = n_workers)
system.time({
  forcres <- future_map(datl, ~run_forc(.x))
})

## view any warnings
warnings()

## see below (after national forecasting) for formatting of state forecasts

############################################################################
## now we need to get the national forecasts

## use ilidat from above
ilidat_us <- iliaug %>% dplyr::filter(location=="US")
ilifor_us <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01", stepwise=FALSE, approximation=FALSE)

# If using log(ili), make all the zeros be the minimum nonzero value
if (tologili) {
  iliaug    <- iliaug    %>% mutate(weighted_ili=mnz_replace(weighted_ili))
  ilidat_us <- ilidat_us %>% mutate(weighted_ili=mnz_replace(weighted_ili))
  ilifor_us$ilidat     <- ilifor_us$ilidat     %>% mutate(ili=mnz_replace(ili))
  ilifor_us$ili_future <- ilifor_us$ili_future %>% mutate(ili=mnz_replace(ili))
  ilifor_us$ili_bound  <- ilifor_us$ili_bound  %>% mutate(ili=mnz_replace(ili))
}

## data list by location
dat_us <-
  prep_hdgov_hosp(hosp, min_per_week = 0, remove_incomplete = ri) %>%
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
  mutate(flu.admits.cov=log(flu.admits.cov)) %>%
  select(-iliopt)

models <-
  list(
    poisson1 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "poisson"),
    poisson2 = trending::glm_model(flu.admits ~ ili + hosp_rank + offset(flu.admits.cov), family = "poisson"),
    #poisson3 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "poisson"),
    quasipoisson1 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "quasipoisson"),
    quasipoisson2 = trending::glm_model(flu.admits ~ ili + hosp_rank + offset(flu.admits.cov), family = "quasipoisson"),
    #quasipoisson3 = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank + offset(flu.admits.cov), family = "quasipoisson"),
    negbin1 = trending::glm_nb_model(flu.admits ~ ili + hosp_rank + ili_rank),
    negbin2 = trending::glm_nb_model(flu.admits ~  ili + hosp_rank + offset(flu.admits.cov))
    #negbin3 = trending::glm_nb_model(flu.admits ~  ili + hosp_rank + ili_rank + offset(flu.admits.cov))
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
  write_csv(., paste0("submission/SigSci-CREG/", this_monday(), "-SigSci-CREG.candidate.csv"))

bound_truth <-
  do.call("rbind", datl) %>%
  bind_rows(., dat_us)

pdf(paste0("submission/SigSci-CREG/artifacts/plots/", this_monday(), "-SigSci-CREG.pdf"), width=11.5, height=8)
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
  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = ri) %>%
  dplyr::filter(abbreviation != "DC")

prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
                                     epiyear = epiyear,
                                     epiweek=epiweek,
                                     key=location)
# Fit a model
hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               covariates=c("hosp_rank", "ili_rank"),
                               stepwise=FALSE, approximation=FALSE)

# format for submission
formatted_list <- format_for_submission(hosp_fitfor$tsfor, method = "ts")

## ARIMA
## force to monday (required to validate a forecast created on a day other than sunday or monday)
formatted_list$arima$forecast_date <- this_monday()
validate_forecast(formatted_list$arima)
# formatted_list$arima %>%
#   write_csv(., paste0("submission/SigSci-TSENS/", this_monday(), "-SigSci-TSENS-ARIMA.candidate.csv"))

pdf(paste0("submission/SigSci-TSENS/artifacts/plots/", this_monday(), "-SigSci-TSENS-ARIMA.pdf"), width=11.5, height=8)
for(loc in unique(formatted_list$arima$location)) {
  p <- plot_forecast(prepped_hosp, formatted_list$arima, location = loc)
  print(p)
}
dev.off()

## ETS
## force to monday (required to validate a forecast created on a day other than sunday or monday)
formatted_list$ets$forecast_date <- this_monday()
validate_forecast(formatted_list$ets)
# formatted_list$ets %>%
#   write_csv(., paste0("submission/SigSci-TSENS/", this_monday(), "-SigSci-TSENS-ETS.candidate.csv"))

pdf(paste0("submission/SigSci-TSENS/artifacts/plots/", this_monday(), "-SigSci-TSENS-ETS.pdf"), width=11.5, height=8)
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
  write_csv(., paste0("submission/SigSci-TSENS/", this_monday(), "-SigSci-TSENS.candidate.csv"))

pdf(paste0("submission/SigSci-TSENS/artifacts/plots/", this_monday(), "-SigSci-TSENS-ensemble.pdf"), width=11.5, height=8)
for(loc in unique(formatted_list$ensemble$location)) {
  p <- plot_forecast(prepped_hosp, formatted_list$ensemble, location = loc)
  print(p)
}
dev.off()


################################################################################
## create a PDF with all models plotted together
combo_sub <-
  bind_rows(
    mutate(all_prepped, model = "SigSci-CREG"),
    mutate(formatted_list$ensemble, model = "SigSci-TSENS"),
    mutate(formatted_list$ets, model = "SigSci-TSENS (ETS)"),
    mutate(formatted_list$arima, model = "SigSci-TSENS (ARIMA)")
  )

pdf(paste0("submission/", this_monday(), "-all-models.pdf"), width=11.5, height=8)
for(loc in unique(combo_sub$location)) {
  p <- plot_forecast(bound_truth, combo_sub, location = loc)
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

hosp_ets_formula <- hosp_fitfor$formulas$ets

glm_forcres <- c(forcres, us_forcres)
glm_model_info <-
  glm_forcres %>%
  map("results") %>%
  map_df("model") %>%
  mutate(forecast_date = this_monday())

## save tsens component forecasts
hosp_ets_forc <- formatted_list$ets
hosp_arima_forc <- formatted_list$arima

## Save locations/models which were null
hosp_tsens_null_models <- hosp_fitfor$nullmodels

save(glm_forcres, glm_model_info, ilidat_st, ilifor_st, ilidat_us, ilifor_us, file = paste0("submission/SigSci-CREG/artifacts/params/", this_monday(), "-SigSci-CREG-model-info.rda"))
save(ili_params,hosp_arima_params, hosp_ets_formula, hosp_ets_forc, hosp_arima_forc, hosp_tsens_null_models, file = paste0("submission/SigSci-TSENS/artifacts/params/", this_monday(), "-SigSci-TSENS-model-info.rda"))
