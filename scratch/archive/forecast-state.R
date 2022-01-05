library(tidyverse)
library(fiphde)

# Get data
# the years argument for cdcfluview::ilinet gets the *season* corresponding to the year.
# so, 2019 = the 2019-2020 flu season. If you only get 2020-2021, you're getting the
# 2020-2021 and 2021-2022 flu season, meaning that you will *NOT* capture early 2020 data.
ilidat <- get_cdc_ili(region=c("national","state"), years=2019:lubridate::year(lubridate::today()))

# save(ilidat, file = "~/Downloads/ilidat-2021-12-12")

ilidat_st <- ilidat %>% dplyr::filter(region_type=="States")
ilifor_st <- forecast_ili(ilidat_st, horizon=5L, trim_date="2020-03-01")

## get most recent week in forecast because ILInet is one week behind
## NOTE: need a more robust way to do this!
## FIX ME
pseudo_nowcast <-
  ilifor_st$ili_forecast %>%
  as_tibble() %>%
  arrange(location,yweek) %>%
  group_by(location) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-ili) %>%
  select(location,yweek,ili=.mean) %>%
  mutate(epiweek = lubridate::epiweek(yweek),
         epiyear = lubridate::epiyear(yweek)) %>%
  select(location,epiyear,epiweek,ili)

## add the "nowcast" back to the ilidat for joining below
ilifor_st$ilidat <-
  ilifor_st$ilidat %>%
  bind_rows(.,pseudo_nowcast) %>%
  arrange(location,epiyear,epiweek)

## now remove the first week ahead forecast ("nowcast") from above
## NOTE: this wont be an issue if we get a nowcast from another source

ilifor_st$ili_future <-
  ilifor_st$ili_future %>%
  arrange(location, epiyear,epiweek) %>%
  group_by(location) %>%
  filter(row_number() > 1) %>%
  ungroup()

# What are the arima params?
ilifor_st$arima_params

# Take a look at the forecasted data
ilifor_st$ili_future

# Take a look at the forecasted data bound do the real data
ilifor_st$ili_bound %>% tail(8)

# Plot actual versus forecasted values
p.ili <-
  ilifor_st$ili_bound %>%
  mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>%
  ggplot(aes(date, ili)) +
  geom_line(alpha=.5, lwd=.2) +
  geom_point(aes(col=forecasted)) +
  theme_bw() +
  labs(x="Date", y="Unweighted ILI", title="ILI forecast") +
  facet_wrap(~ location)
p.ili

hosp <- get_hdgov_hosp(maxdate="2021-12-29")

tmp_weekly_flu <-
  hosp %>%
  # mutate(date = date - 1) %>%
  mutate(flu.admits = as.numeric(flu.admits),
         flu.admits.cov = as.numeric(flu.admits.cov)) %>%
  mutate(epiweek = lubridate::epiweek(date),
         epiyear = lubridate::epiyear(date)) %>%
  group_by(state,epiyear, epiweek) %>%
  summarise(flu.admits = sum(flu.admits, na.rm = TRUE),
            flu.admits.cov = sum(flu.admits.cov, na.rm = TRUE),
            .groups = "drop") %>%
  rename(abbreviation = state) %>%
  left_join(select(fiphde:::locations, location, abbreviation)) %>%
  left_join(ilifor_st$ilidat, by = c("epiyear", "location", "epiweek")) %>%
  filter(abbreviation %in% state.abb)

## add lag columns
tmp_weekly_flu_w_lag <-
  tmp_weekly_flu %>%
  group_by(location) %>%
  mutate(lag_1 = lag(flu.admits, 1)) %>%
  ungroup() %>%
  ## NOTE: FL will be removed because there is no ILI data
  ## FIX THIS
  ## impute with ILI for HHS region for FL?
  ## ALSO ... this takes care of the current, partial weeks (which will have missing ILI)
  ## for example if run during current week the ILI will be NA because it wont be reported in ILInet
  ## but we need a better way to handle that too!
  filter(complete.cases(.)) %>%
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  left_join(fiphde:::historical_severity, by="epiweek") %>%
  ## NOTE: we need to make sure we filter for the date when states (generally) started reporting flu hosps
  ## do we need to check for start dates state by state ???
  filter(date > as.Date("2020-10-18", format = "%Y-%m-%d"))

tmp_weekly_flu_w_lag

## data list by location
datl <- group_split(tmp_weekly_flu_w_lag, location)

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

# forcres <- list()
#
# system.time({
#
#   for(i in 1:length(datl)) {
#
#     forcres[[i]] <-
#       tryCatch({
#         dat <- datl[[i]]
#         message(unique(dat$abbreviation))
#
#         new_cov <-
#           ilifor_st$ili_future %>%
#           filter(location %in% unique(dat$location)) %>%
#           left_join(fiphde:::historical_severity) %>%
#           ## assume the coverage will be the average of the last 8 weeks of reporting
#           bind_cols(.,tibble(flu.admits.cov = rep(mean(dat$flu.admits.cov,8), 4))) %>%
#           select(-epiweek,-epiyear)
#
#         tmp_res <- glm_wrap(dat,
#                             new_covariates = new_cov,
#                             .models = models,
#                             alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2)
#
#         approach <- paste0("GLM-", as.character(tmp_res$model$fit$fitted_model$family)[1],
#                            "\n",
#                            paste0(names(tmp_res$model$fit$fitted_model$coefficients), collapse = " + "))
#
#         future_dat <-
#           new_cov %>%
#           mutate(flu.admits = NA) %>%
#           mutate(date = max(dat$date) + c(7,14,21,28)) %>%
#           mutate(epiweek = lubridate::epiweek(date), epiyear = lubridate::epiyear(date))
#
#         p.hosp <- plot_forc(tmp_res$forecasts, dat, future_dat) +
#           labs(caption = paste0(unique(dat$abbreviation), "\n", approach, "\n", max(dat$date))) +
#           theme(plot.caption = element_text(hjust = 0))
#
#         print(p.hosp)
#
#         list(
#           location = unique(dat$abbreviation),
#           results = tmp_res,
#           data = list(training = dat, testing = future_dat),
#           plots = list(p.hosp = p.hosp),
#           thru_week = max(dat$date),
#           approach = approach)
#
#       },
#       error = function(cond) {
#         message("Skipping location due to error ... ")
#         list(
#           location = unique(dat$abbreviation),
#           results = NA,
#           data = NA,
#           plots = NA,
#           thru_week = NA,
#           approach = NA)
#       })
#   }
# })


plot_forc <- function(.forecasts, .train, .test) {

  forc_dat <-
    .forecasts %>%
    dplyr::filter(quantile %in% c(NA,0.025,0.975)) %>%
    tidyr::spread(quantile,value) %>%
    dplyr::rename(lower = `0.025`, upper = `0.975`, mean = `<NA>`)

  .test %>%
    dplyr::bind_rows(.train) %>%
    dplyr::select(epiweek,epiyear, truth = flu.admits, location) %>%
    dplyr::left_join(forc_dat) %>%
    dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(date,truth), lwd = 2, col = "black") +
    ggplot2::geom_line(ggplot2::aes(date,mean), lwd = 2, alpha = 0.5, lty = "solid", col = "firebrick") +
    ggplot2::geom_ribbon(ggplot2::aes(date, ymin = lower, ymax = upper), alpha = 0.25, fill = "firebrick") +
    ## get an upper limit from whatever the max of observed or forcasted hospitalizations is
    ggplot2::scale_y_continuous(limits = c(0,max(c(.test$flu.admits, .train$flu.admits, forc_dat$upper)))) +
    ggplot2::scale_x_date(date_labels = "%Y-%m", date_breaks = "month") +
    ggplot2::labs(x = "Date", y = "Count", title = "Influenza hospitalizations") +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~ location)

}

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
                        alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2)

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
      location = unique(dat$abbreviation),
      results = tmp_res,
      data = list(training = dat, testing = future_dat),
      plots = list(p.hosp = p.hosp),
      thru_week = max(dat$date),
      approach = approach)

  },
  error = function(cond) {
    message("Skipping location due to error ... ")
    list(
      location = unique(dat$abbreviation),
      results = NA,
      data = NA,
      plots = NA,
      thru_week = NA,
      approach = NA)
  })
}

library(furrr)

plan(multisession, workers = 8)
system.time({
forcres <- future_map(datl, ~run_forc(.x))
})

pdf("~/Downloads/glm_states_ahead_v2.pdf", width=11.5, height=8)
for(i in 1:length(forcres)) {

  if(!is.na(forcres[[i]]$plots)) {
    print(forcres[[i]]$plots$p.hosp)
  } else {
    next
  }
}
dev.off()
save(forcres, file = "~/Downloads/fiphde-temp-glm-states-forcres-v2.rda")
