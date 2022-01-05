library(tidyverse)
library(fiphde)

# Get data
# the years argument for cdcfluview::ilinet gets the *season* corresponding to the year.
# so, 2019 = the 2019-2020 flu season. If you only get 2020-2021, you're getting the
# 2020-2021 and 2021-2022 flu season, meaning that you will *NOT* capture early 2020 data.
ilidat <- get_cdc_ili(region=c("national","state"), years=2019:lubridate::year(lubridate::today()))

# save(ilidat, file = "~/Downloads/ilidat-2021-12-12")

ilidat_st <- ilidat %>% dplyr::filter(region_type=="States")
ilifor_st <- forecast_ili(ilidat_st, horizon=4L, trim_date="2020-03-01")

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

hosp <- get_hdgov_hosp(maxdate="2021-12-26")

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
  ## NOTE: FL will be removed because there is no ILI data ... need to fix this!
  ## impute with ILI for HHS region for FL?
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

res <- list()

system.time({

for(i in 1:length(datl)) {

  res[[i]] <-
    tryCatch({
      dat <- datl[[i]]
      message(unique(dat$abbreviation))

      train_dat <- dat %>% filter(row_number() < n() - 3)
      test_dat <- dat %>% filter(row_number() >= n() - 3)

      tmp_res <- glm_wrap(train_dat,
                          new_covariates = tibble(flu.admits.cov = rep(tail(train_dat$flu.admits.cov,1), 4),
                                                  ili_rank=test_dat$ili_rank,
                                                  hosp_rank=test_dat$hosp_rank,
                                                  ili_mean=test_dat$ili_mean,
                                                  hosp_mean=test_dat$hosp_mean,
                                                  ili=test_dat$ili),
                          .models = models,
                          alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2)

      approach <- paste0("GLM-", as.character(tmp_res$model$fit$fitted_model$family)[1],
                         "\n",
                         paste0(names(tmp_res$model$fit$fitted_model$coefficients), collapse = " + "))

      p.hosp <- plot_forc(tmp_res$forecasts, train_dat, test_dat) +
        labs(caption = paste0(unique(dat$abbreviation), "\n", approach, "\n", max(train_dat$date))) +
        theme(plot.caption = element_text(hjust = 0))

      print(p.hosp)

      list(
        location = unique(dat$abbreviation),
        results = tmp_res,
        data = list(training = train_dat, testing = test_dat),
        plots = list(p.hosp = p.hosp),
        thru_week = max(train_dat$date),
        approach = approach)

    },
    error = function(cond) {
      message("Skipping location due to error ... ")
      ## NOTE: is this working ???
      ## seems like error is just skipping without assigning anything
      ## TO FIX
      list(
        location = unique(dat$abbreviation),
        results = NA,
        data = NA,
        plots = NA,
        thru_week = NA,
        approach = NA)
    })
}

})

pdf("~/Downloads/glm_states.pdf", width=11.5, height=8)
for(i in 1:length(res)) {

  if(!is.na(res[[i]]$plots)) {
    print(res[[i]]$plots$p.hosp)
  } else {
    next
  }
}
dev.off()
