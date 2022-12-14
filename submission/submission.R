################################################################################
## Common: run this section for both CREG and TSENS ----
# Load libs
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(fiphde)
library(furrr)
# Return incomplete?
## if this is set to TRUE it will expect ...
## the hospitalization data will be reported for the entire last week
## BUT this will break the code if we try to run on a sunday
## last week
if(as.POSIXlt(lubridate::today())$wday == 0) {
  ri <- FALSE
} else {
  ri <-  TRUE
}
# Get hosp data
hosp <- get_hdgov_hosp(limitcols = TRUE)

################################################################################
## SigSci-CREG ----

## first an option
## do we want to log-transform ILI ???
tologili <- TRUE
## another option for the number of cores to use in parallelization
## use almost all of the cores you have (-2)
n_workers <- max(1, parallel::detectCores()-2)
## or alterantively just set at a value (eg 4)
#n_workers <- 4
## another option whether or not to use remove_incomplete feature in prepping hdgov hosp
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
ilidat <- replace_ili_nowcast(ilidat, weeks_to_replace=1)
ilifor <- forecast_ili(ilidat, horizon=4L, trim_date="2020-03-01", stepwise=FALSE, approximation=FALSE)

## retrieve data and run "nowcast" to get one week ahead to handle reporting delay
labdat <- get_cdc_clin()
## NOTE: will need to figure how how many weeks we actually need to "nowcast" on mondays ...
labdat <- clin_nowcast(labdat, 1)


labdatfor <-
  labdat %>%
  group_by(location) %>%
  summarise(total = pois_forc(., .location = location, total),
            n_positive = pois_forc(., .location = location, n_positive),
            epiyear = lubridate::epiyear(lubridate::today() + c(0,7,14,21)),
            epiweek = lubridate::epiweek(lubridate::today() + c(0,7,14,21)),
            .groups = "drop") %>%
  mutate(p_positive = (n_positive / total)*100) %>%
  mutate(p_positive = fiphde::mnz_replace(p_positive)) %>%
  mutate(p_positive = log(p_positive))

# If using log(ili), make all the zeros be the minimum nonzero value
if (tologili) {
  ilidat    <- ilidat    %>% mutate(weighted_ili=mnz_replace(weighted_ili))
  # iliaug    <- iliaug    %>% mutate(weighted_ili=mnz_replace(weighted_ili))
  ilifor$ilidat     <- ilifor$ilidat     %>% mutate(ili=mnz_replace(ili))
  ilifor$ili_future <- ilifor$ili_future %>% mutate(ili=mnz_replace(ili))
  ilifor$ili_bound  <- ilifor$ili_bound  %>% mutate(ili=mnz_replace(ili))
}

## data list by location
tmp_dat <-
  prep_hdgov_hosp(hosp, min_per_week = 0, remove_incomplete = ri) %>%
  mutate(lag_1 = lag(flu.admits, 1)) %>%
  filter(!is.na(lag_1)) %>%
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  ## not dc
  filter(!abbreviation %in% c("DC")) %>%
  left_join(ilifor$ilidat, by = c("epiyear", "location", "epiweek")) %>%
  left_join(labdat, by = c("epiyear", "location", "abbreviation", "week_start", "epiweek")) %>%
  ## optionally log tranform ILI ???
  ## see above
  ## NOTE: we *need* to add a column with the logical so eval doesnt recycle ili
  mutate(iliopt = tologili) %>%
  mutate(ili = ifelse(iliopt, log(ili), ili)) %>%
  ## NOTE: using same option for log tranform ILI to determine log transform p_positive
  mutate(p_positive = ifelse(p_positive == 0, fiphde::mnz_replace(p_positive), p_positive)) %>%
  mutate(p_positive = ifelse(iliopt, log(p_positive), p_positive)) %>%
  select(-iliopt) %>%
  mutate(flu.admits.cov=log(flu.admits.cov))

smoothed_admits_dat <-
  tmp_dat %>%
  group_by(location) %>%
  ## NOTE: the first 3 obs will be NA because we cant smooth backwards ...
  ## ... almost certainly a better way to handle with slider but this works for now
  summarise(smoothed_admits = c(NA,NA,NA, map_dbl(4:n(), function(x) smoothie(flu.admits[1:x], weights = c(1,1.33,1.66,2)))),
            epiweek = epiweek,
            epiyear = epiyear,
            .groups = "drop")

datl <-
  tmp_dat %>%
  left_join(smoothed_admits_dat) %>%
  ## the first 3 obs will be NA so this mutate fills in those values with "regular" (non smoothed) admit signal
  mutate(smoothed_admits = ifelse(is.na(smoothed_admits), flu.admits, smoothed_admits)) %>%
  group_split(., location)

models <-
  list(
    poisson1 = trending::glm_model(flu.admits ~ p_positive + ili + smoothed_admits, family = "poisson"),
    poisson2 = trending::glm_model(flu.admits ~ p_positive + ili + offset(flu.admits.cov) + smoothed_admits, family = "poisson"),
    poisson3 = trending::glm_model(flu.admits ~ ili + smoothed_admits, family = "poisson"),
    poisson4 = trending::glm_model(flu.admits ~ p_positive + smoothed_admits, family = "poisson"),
    quasipoisson1 = trending::glm_model(flu.admits ~ p_positive + ili + smoothed_admits, family = "quasipoisson"),
    quasipoisson2 = trending::glm_model(flu.admits ~ p_positive + ili + offset(flu.admits.cov) + smoothed_admits, family = "quasipoisson"),
    quasipoisson3 = trending::glm_model(flu.admits ~ p_positive + smoothed_admits, family = "quasipoisson"),
    quasipoisson4 = trending::glm_model(flu.admits ~ ili + smoothed_admits, family = "quasipoisson"),
    negbin1 = trending::glm_nb_model(flu.admits ~ p_positive + ili + smoothed_admits),
    negbin2 = trending::glm_nb_model(flu.admits ~ p_positive + ili + offset(flu.admits.cov) + smoothed_admits),
    negbin3 = trending::glm_nb_model(flu.admits ~ ili + smoothed_admits),
    negbin4 = trending::glm_nb_model(flu.admits ~ p_positive + smoothed_admits)
  )


## use furrr mapping to speed up
run_forc <- function(dat) {
  tryCatch({
    message(unique(dat$abbreviation))

    new_cov <-
      ilifor$ili_future %>%
      left_join(labdatfor, by = c("epiyear", "epiweek", "location")) %>%
      filter(location %in% unique(dat$location)) %>%
      left_join(fiphde:::historical_severity) %>%
      ## assume the coverage will be the average of the last 8 weeks of reporting
      bind_cols(.,tibble(flu.admits.cov = rep(mean(dat$flu.admits.cov,8), 4))) %>%
      select(-epiweek,-epiyear) %>%
      mutate(ili = log(ili)) %>%
      mutate(smoothed_admits = smoothie(dat$flu.admits, weights = c(1,1.33,1.66,2)))


    tmp_res <- glm_wrap(dat,
                        new_covariates = new_cov,
                        .models = models,
                        alpha = c(0.01, 0.025, seq(0.05, 0.5, by = 0.05)) * 2)

    tmp_res$forecasts$location <- unique(dat$location)

    approach <- paste0("GLM-", as.character(tmp_res$model$fit$fitted_model$family)[1],
                       "\n",
                       paste0(names(tmp_res$model$fit$fitted_model$coefficients), collapse = " + "))

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

all_forecasts <-
  forcres %>%
  purrr::keep(~!is.na(.x$approach)) %>%
  purrr::map(., "results") %>%
  purrr::map_df(., "forecasts")

all_prepped <- format_for_submission(all_forecasts, method = "CREG")$CREG

## force to monday (required to validate a forecast created on a day other than sunday or monday)
all_prepped$forecast_date <- this_monday()

validate_forecast(all_prepped)

all_prepped %>%
  write_csv(., paste0("submission/SigSci-CREG/", this_monday(), "-SigSci-CREG.candidate.csv"))

bound_truth <- do.call("rbind", datl)

pdf(paste0("submission/SigSci-CREG/artifacts/plots/", this_monday(), "-SigSci-CREG.pdf"), width=11.5, height=8)
for(loc in unique(all_prepped$location)) {
  p <- plot_forecast(bound_truth, all_prepped, location = loc)
  print(p)
}
dev.off()

################################################################################
## SigSci-TSENS ----

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
  ilifor$arima_params %>%
  mutate(forecast_date = this_monday())

hosp_arima_params <-
  map(hosp_fitfor$tsfit$arima, "fit") %>%
  map_df("spec") %>%
  mutate(location = hosp_fitfor$tsfit$location, .before = "p") %>%
  mutate(forecast_date = this_monday())

hosp_ets_formula <- hosp_fitfor$formulas$ets

glm_forcres <- c(forcres)
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

save(glm_forcres, glm_model_info, ilidat, ilifor, labdat, labdatfor,  file = paste0("submission/SigSci-CREG/artifacts/params/", this_monday(), "-SigSci-CREG-model-info.rda"))
save(ili_params,hosp_arima_params, hosp_ets_formula, hosp_ets_forc, hosp_arima_forc, hosp_tsens_null_models, file = paste0("submission/SigSci-TSENS/artifacts/params/", this_monday(), "-SigSci-TSENS-model-info.rda"))

################################################################################
## experimental categorical targets

## get creg forecast
creg_forc <- all_prepped
## get tsens forecast
tsens_forc <- formatted_list$ensemble


forecast_categorical <- function(.forecast,.observed) {

  ## prep the .forecast object for experimental target summary
  forc4exp <-
    .forecast %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::mutate(quantile = as.numeric(quantile)) %>%
    ## only looking at 2 week ahead for now
    dplyr::filter(target == "2 wk ahead inc flu hosp") %>%
    ## join to internal locations object that has population data
    dplyr::left_join(fiphde:::locations) %>%
    ## calculate rate per 100k
    dplyr::mutate(rate = (value/population)*100000) %>%
    ## exclude point estimates
    dplyr::filter(type == "quantile") %>%
    ## get columns of interest
    dplyr::select(forecast_date, location, quantile, value, rate)

  hosp4exp <-
    .observed %>%
    ## find observed data that is prior to the 1 week ahead forecast
    dplyr::filter(week_end == min(.forecast$target_end_date) - 7) %>%
    ## join to internal locations object that has population data
    left_join(fiphde:::locations) %>%
    ## calculate rate per 100k
    mutate(lag_rate = (flu.admits/population)*100000) %>%
    ## get columns of interest
    select(location, lag_value = flu.admits, lag_rate)

  ## get "probability range" from each quantile ...
  ## for example: 0.99 and 0.01 quantiles have same prob value (0.01)
  quants <-
    forc4exp %>%
    dplyr::filter(quantile < 0.5) %>%
    dplyr::pull(quantile) %>%
    unique(.)

  quant_denom <-
    c(quants,quants,0.5) %>%
    sum(.)

  ## join prepped forecast and prepped observed
  dplyr::left_join(forc4exp,hosp4exp) %>%
    ## calculate component indicators
    dplyr::mutate(
      ind_count = abs(value - lag_value),
      ind_rate = abs(rate - lag_rate),
      ind_rate2 = ifelse(rate - lag_rate > 0, "positive", "negative")
    ) %>%
    ## use component indicators to assess overall type per CDC flowchart
    dplyr::mutate(type_id =
                    dplyr::case_when(
                      ind_count < 20 | ind_rate < 0.00001 ~ "stable",
                      (ind_count < 40 | ind_rate < 0.00002) & ind_rate2 == "positive" ~ "increase",
                      (ind_count < 40 | ind_rate < 0.00002) & ind_rate2 == "negative" ~ "decrease",
                      (ind_count >= 40 & ind_rate >= 0.00002) & ind_rate2 == "positive" ~ "large_increase",
                      (ind_count >= 40 & ind_rate >= 0.00002) & ind_rate2 == "negative" ~ "large_decrease"
                    )) %>%
    ## convert quantiles to "probability magnitude"
    dplyr::mutate(quantile = ifelse(quantile > 0.5, 1-quantile, quantile)) %>%
    dplyr::group_by(location,type_id) %>%
    ## sum up quantiles as probability magnitude over the total sum of quantiles
    dplyr::summarise(value = sum(quantile)/ (quant_denom), .groups = "drop") %>%
    ## fill in any missing type_ids in a given location with 0
    tidyr::complete(location,type_id, fill = list(value = 0)) %>%
    ## prep the submission format
    dplyr::mutate(forecast_date = unique(.forecast$forecast_date),
                  target = "2 wk flu hosp rate change",
                  type = "category") %>%
    dplyr::select(forecast_date, target,location, type, type_id, value)

}


tsens_exp <- forecast_categorical(tsens_forc, prepped_hosp)
write_csv(tsens_exp, paste0("submission/SigSci-TSENS/", this_monday(), "-SigSci-TSENS.candidate.experimental.csv"))
creg_exp <- forecast_categorical(creg_forc, prepped_hosp)
write_csv(tsens_exp, paste0("submission/SigSci-CREG/", this_monday(), "-SigSci-CREG.candidate.experimental.csv"))
