library(tidyverse)
library(fiphde)
library(fable)
theme_set(theme_bw())
## some helpers used in the eval_wrap wrapper

## Function to make new data with historical epiweek severity
make_new_data <- function(.data, .horizon=4) {
  tsibble::new_data(.data, n=.horizon) %>%
    dplyr::mutate(epiweek=lubridate::epiweek(yweek)) %>%
    dplyr::inner_join(fiphde:::historical_severity)
}

prep_mable <- function(myforecast) {
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
    dplyr::mutate(epiweek = lubridate::epiweek(yweek),
                  epiyear = lubridate::epiyear(yweek)) %>%
    select(-type)

  return(forcs)
}


## write wrapper function for data prep / modeling with different dates
eval_wrap <- function(ts_method = NULL, hosp = NULL, ilidat = NULL, min_hhs = "2020-10-12", max_hhs = Sys.Date(), min_flu = "2020-03-01", complete = TRUE, covariates = NULL, .models, alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2) {

  ## ILI forecasting
  message(sprintf("Step 1: ILI forecasting using historical data from %s to present ...", min_flu))

  if(is.null(ilidat)) {
    ilidat <- get_cdc_ili(region="national", years=2019:lubridate::year(lubridate::today()))
  }

  # Subset to US only
  ilidat_us <- ilidat %>% filter(location=="US")
  # Forecast ILI
  ilifor <- forecast_ili(ilidat_us, horizon=4L, trim_date=min_flu, constrained=TRUE)

  # Plot actual versus forecasted values
  p.ili <-
    ilifor$ili_bound %>%
    mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>%
    ggplot(aes(date, ili)) +
    geom_line(alpha=.5, lwd=.2) +
    geom_point(aes(col=forecasted)) +
    theme_bw() +
    labs(x="Date", y="Unweighted ILI", title="ILI forecast")

  if(is.null(hosp)) {

    message(sprintf("Step 2: Retrieving HHS hospitalization data from %s to %s ...", min_hhs, max_hhs))

    hosp <-
      get_hdgov_hosp(mindate = min_hhs, maxdate=max_hhs) %>%
      # mutate(date = date - 1) %>%
      mutate(flu.admits = as.numeric(flu.admits),
             flu.admits.cov = as.numeric(flu.admits.cov)) %>%
      mutate(epiweek = lubridate::epiweek(date),
             epiyear = lubridate::epiyear(date)) %>%
      group_by(date) %>%
      summarise(flu.admits = sum(flu.admits, na.rm = TRUE),
                flu.admits.cov = sum(flu.admits.cov, na.rm = TRUE),
                epiyear = first(epiyear),
                epiweek = first(epiweek),
                .groups = "drop") %>%
      mutate(location = "US", .before = "epiweek") %>%
      group_by(epiyear, epiweek, location) %>%
      summarise(flu.admits =  sum(flu.admits),
                flu.admits.cov = sum(flu.admits.cov),
                n_days = n(),
                .groups = "drop") %>%
      dplyr::left_join(ilifor$ilidat, by = c("epiyear", "location", "epiweek"))

    if(complete) {
      hosp <-
        hosp %>%
        dplyr::filter(n_days == 7)
    }

  } else {
    hosp <-
      hosp %>%
      dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
      dplyr::filter(date >= min_hhs & date <= max_hhs) %>%
      dplyr::select(-date) %>%
      dplyr::left_join(ilifor$ilidat, by = c("epiyear", "location", "epiweek"))

    if(complete) {
      hosp <-
        hosp %>%
        dplyr::filter(n_days == 7)
    }

  }

  if(is.null(ts_method)) {
    tmp_weekly_flu_w_lag <-
      hosp %>%
      dplyr::select(-n_days) %>%
      dplyr::mutate(lag_1 = lag(flu.admits, 1)) %>%
      dplyr::mutate(lag_2 = lag(flu.admits, 2)) %>%
      dplyr::mutate(lag_3 = lag(flu.admits, 3)) %>%
      dplyr::mutate(lag_4 = lag(flu.admits, 4)) %>%
      dplyr::filter(complete.cases(.)) %>%
      dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
      dplyr::inner_join(fiphde:::historical_severity, by="epiweek")

    train_dat <- tmp_weekly_flu_w_lag %>% filter(row_number() < n() - 3)
    test_dat <- tmp_weekly_flu_w_lag %>% filter(row_number() >= n() - 3)

    new_covariates <-
      dplyr::tibble(flu.admits.cov = rep(tail(train_dat$flu.admits.cov,1), 4),
                    ili_rank=test_dat$ili_rank,
                    hosp_rank=test_dat$hosp_rank,
                    ili_mean=test_dat$ili_mean,
                    hosp_mean=test_dat$hosp_mean,
                    ili=test_dat$ili)

    if(!is.null(covariates)) {
      new_covariates <- cbind(new_covariates, covariates)
    }

    message(sprintf("Step 3: Fitting %d glm models ...", length(.models)))
    res <- glm_wrap(train_dat, new_covariates = new_covariates,.models = .models, alpha = alpha)

    approach <- paste0("GLM-", as.character(res$model$fit$fitted_model$family)[1],
                       "\n",
                       paste0(names(res$model$fit$fitted_model$coefficients), collapse = " + "))

  } else {
    message(sprintf("Step 3: Fitting TS %s model ...", ts_method))
    hosp_tsibble <-
      hosp %>%
      left_join(fiphde:::historical_severity, by="epiweek") %>%
      make_tsibble(epiyear, epiweek, key=location, chop=FALSE) %>%
      mutate(date = MMWRweek::MMWRweek2Date(epiyear,epiweek)) %>%
      filter(location=="US")

    train_dat <- hosp_tsibble %>% filter(row_number() < n() - 3)
    test_dat <- make_new_data(train_dat)

    tmp_fit <-
      train_dat %>%
      model(ets=ETS(flu.admits ~ season(method="N")),
            rw_naive=NAIVE(flu.admits),
            rw_drift=RW(flu.admits~drift()),
            arima=ARIMA(flu.admits~PDQ(0,0,0)),
            arima_hosp=ARIMA(flu.admits~PDQ(0,0,0)+ hosp_rank),
            arima_ili=ARIMA(flu.admits~PDQ(0,0,0)+ ili_rank)) %>%
      mutate(ensemble=(ets+arima_hosp+arima_ili)/3)

    myforecast <-
      tmp_fit %>%
      forecast(new_data=test_dat)

    forcs <-
      myforecast %>%
      prep_mable() %>%
      filter(.model == ts_method)

    train_dat <- as_tibble(train_dat)

    test_dat <-
      as_tibble(test_dat) %>%
      left_join(select(hosp_tsibble,epiweek,epiyear,flu.admits))

    res <- list(forecasts = forcs, model = tmp_fit)

    approach <- paste0("TS-",ts_method)

  }

  p.hosp <-
    plot_forc(res$forecasts, train_dat, test_dat) +
    labs(caption = paste0(approach, "\n", max(train_dat$date)))

  scores <-
    wis_score(res$forecasts, test_dat) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(horizon = 1:n())

  list(plots = list(p.hosp = p.hosp, p.ili = p.ili),
       results = res,
       data = list(hosp = hosp, ili = ilidat, training = train_dat, testing = test_dat),
       scores = scores ,
       min_hhs = min_hhs,
       max_hhs = max_hhs,
       min_flu = min_flu,
       thru_week = max(train_dat$date),
       method = approach)
}


## lets set up the hosp data to pass in ...
## do it once and trim as many times as we want
## alternative is to keep hosp=NULL param to eval_wrap and the function will retrieve each time
hosp_dat <-
  get_hdgov_hosp(mindate =  "2020-10-12", maxdate="2021-12-27") %>%
  mutate(flu.admits = as.numeric(flu.admits),
         flu.admits.cov = as.numeric(flu.admits.cov)) %>%
  mutate(epiweek = lubridate::epiweek(date),
         epiyear = lubridate::epiyear(date)) %>%
  group_by(date) %>%
  summarise(flu.admits = sum(flu.admits, na.rm = TRUE),
            flu.admits.cov = sum(flu.admits.cov, na.rm = TRUE),
            epiyear = first(epiyear),
            epiweek = first(epiweek),
            .groups = "drop") %>%
  mutate(location = "US", .before = "epiweek") %>%
  group_by(epiyear, epiweek, location) %>%
  summarise(flu.admits =  sum(flu.admits),
            flu.admits.cov = sum(flu.admits.cov),
            n_days = n(),
            .groups = "drop")

## vector of dates through which try to mask training data to iteratively fit models
thru_dates <- seq(as.Date("2020-11-15", format = "%Y-%m-%d"), as.Date("2021-12-26", format = "%Y-%m-%d"), by = 7)

## hack with two step call to eval_wrap map
## ... because i cant use a NULL when defining a vector ...
to_loop <-
  crossing(
    # ts_methods = c("ets","ensemble","arima",NULL),
    ts_methods = c("ets","ensemble", "arima", "arima_hosp", "arima_ili", "rw_naive", "rw_drift"),
    thru_dates = thru_dates)

forc_res_ts <- map2(to_loop$ts_methods, to_loop$thru_dates, .f = ~eval_wrap(hosp = hosp_dat, ts_method = .x, max_hhs = .y, .models = NULL))

glm_models <-
  list(
    glm_negbin_lags_ranks = trending::glm_nb_model(flu.admits ~ lag_1 + ili_rank + hosp_rank),
    glm_negbin_ili_lags_ranks = trending::glm_nb_model(flu.admits ~ ili + lag_1 + ili_rank + hosp_rank),
    glm_negbin_ili_lags_ranks_offset = trending::glm_nb_model(flu.admits ~ ili + lag_1 + ili_rank + hosp_rank + offset(flu.admits.cov))
  )

## NOTE: glm wont fit at earlier dates ...
forc_res_glm <- map(thru_dates[10:length(thru_dates)], .f = ~eval_wrap(hosp = hosp_dat, ts_method = NULL, max_hhs = .x, .models = glm_models))

forc_res <- c(forc_res_glm,forc_res_ts)

## that ^ took a while!
## save for later
# save(forc_res, file = "~/Downloads/forc_res.rda")
load("~/Downloads/forc_res.rda")

## NOTE: ran this once and included in analysis below ... worse than GLM with no trim so leaving out
# forc_res_glm2 <- map(thru_dates[10:length(thru_dates)], .f = ~eval_wrap(hosp = hosp_dat, ts_method = NULL, min_hhs = .x - (7*28),  max_hhs = .x, .models = glm_models))

## helper to pull out scores and methods
get_scores <- function(x) {
  x$scores %>%
    mutate(method = x$method) %>%
    mutate(thru_week = x$thru_week)
}

scores_by_method <-
  map_df(forc_res, get_scores) %>%
  ## fragile: only looks for negative binomial as method (would need to edit if poisson was chosen )
  mutate(method = ifelse(grepl("Negative", method), "GLM-NB", method))

# scores_by_method2 <-
#   map_df(forc_res_glm2, get_scores) %>%
#   mutate(method = "GLM-NB-trim6")

scores_by_method %>%
  mutate(Horizon = as.factor(horizon)) %>%
  ggplot(aes(thru_week, wis)) +
  geom_col(aes(fill = Horizon), position = "dodge") +
  scale_x_date(date_breaks = "month", date_labels = "%Y-%m") +
  facet_wrap(~method) +
  theme(legend.position= "bottom", axis.text.x = element_text(angle = 90)) +
  labs(y = "WIS", x = "Last week of training data")

# summarize over all horizons
scores_by_method %>%
  group_by(epiweek, epiyear, method, thru_week) %>%
  summarize(wis=mean(wis)) %>%
  ggplot(aes(thru_week, wis)) +
  geom_col() +
  scale_x_date(date_breaks = "month", date_labels = "%Y-%m") +
  facet_wrap(~method) +
  theme(legend.position= "bottom", axis.text.x = element_text(angle = 90)) +
  labs(y = "WIS", x = "Last week of training data")

# summarize over all horizons, compare to naive model
sbm <- scores_by_method %>%
  group_by(epiweek, epiyear, method, thru_week) %>%
  summarize(wis=mean(wis), .groups="drop")
sbm %>%
  filter(method=="TS-rw_naive") %>%
  select(epiweek, epiyear, thru_week, naive=wis) %>%
  inner_join(sbm) %>%
  mutate(rwis=(wis-naive)/naive) %>%
  ggplot(aes(thru_week, rwis)) +
  geom_col() +
  scale_x_date(date_breaks = "month", date_labels = "%Y-%m") +
  facet_wrap(~method) +
  theme(legend.position= "bottom", axis.text.x = element_text(angle = 90)) +
  labs(y = "WIS", x = "Last week of training data")

## tabulate wis across methods
scores_by_method %>%
  ## this bit of code ensures that the weeks evaluated overlap all methods
  add_count(thru_week) %>%
  filter(n == n_distinct(method)*4) %>%
  group_by(method) %>%
  summarise(wis_median = median(wis, na.rm=TRUE),
            wis_mean = mean(wis, na.rm=TRUE),
            wis = sum(wis, na.rm=TRUE),
            n = n())

## same thing as above but also grouping by horizon
scores_by_method %>%
  ## this bit of code ensures that the weeks evaluated overlap all methods
  add_count(thru_week) %>%
  filter(n == n_distinct(method)*4) %>%
  group_by(horizon,method) %>%
  summarise(wis_median = median(wis, na.rm=TRUE),
            wis_mean = mean(wis, na.rm=TRUE),
            wis = sum(wis, na.rm=TRUE),
            n = n())

# ## you can also use this on individual models
# forc_res <- eval_wrap(ts_method = "ets", hosp = hosp_dat, .models = NULL)
# forc_res$plots$p.hosp
# forc_res$scores
#
# ## NOTE: we could do this differently per call to eval_wrap if we wanted to assess different families of models
# models <-
#   list(
#     glm_negbin_lags_ranks = trending::glm_nb_model(flu.admits ~ lag_1 + ili_rank + hosp_rank),
#     glm_negbin_ili_lags_ranks = trending::glm_nb_model(flu.admits ~ ili + lag_1 + ili_rank + hosp_rank),
#     glm_negbin_ili_lags_ranks_offset = trending::glm_nb_model(flu.admits ~ ili + lag_1 + ili_rank + hosp_rank + offset(flu.admits.cov))
#   )
#
#
# ## if we want the function to retrieve the data each time keep hosp = NULL
# ## complete = TRUE means only the full epiweeks will be used in modeling
# # forc_res <- eval_wrap(hosp = NULL, .models = models, complete = TRUE)
# # forc_res$plots$p.hosp
# # forc_res$scores
#
# ## use defaults; HHS is 2020-10-12 to current date; ILI data used for forecasting starts at 2020-03-01
# forc_res <- eval_wrap(hosp = hosp_dat, .models = models)
# forc_res$plots$p.hosp
# forc_res$scores
#
# ## move the max hhs back
# forc_res2 <- eval_wrap(hosp = hosp_dat, .models = models, max_hhs = "2021-10-27")
# forc_res2$plots$p.hosp
# forc_res2$scores
#
# ## again
# forc_res3 <- eval_wrap(hosp = hosp_dat, .models = models, max_hhs = "2021-06-27")
# forc_res3$plots$p.hosp
# forc_res3$scores
#
# ## what about shortening the window for training data used
# ## 6 months plus 4 weeks to account for train/test split at last four weeks
# forc_res4 <- eval_wrap(hosp = hosp_dat, .models = models, min_hhs = Sys.Date() - (6*30 + 28), max_hhs = Sys.Date())
# forc_res4$plots$p.hosp
# forc_res4$scores
#
# ## shorter?
# ## 3 months plus 4 weeks to account for train/test split at last 4 weeks
# forc_res5 <- eval_wrap(hosp = hosp_dat, .models = models, min_hhs = Sys.Date() - (3*30 + 28), max_hhs = Sys.Date())
# forc_res5$plots$p.hosp
# forc_res5$scores
#
# ## what does that do with moving max hhs back?
# forc_res6 <- eval_wrap(hosp = hosp_dat, .models = models, min_hhs = as.Date("2021-10-27", format = "%Y-%m-%d") - (3*30 + 28), max_hhs = "2021-10-27")
# forc_res6$plots$p.hosp
# forc_res6$scores
