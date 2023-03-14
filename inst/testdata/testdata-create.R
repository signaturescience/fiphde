library(tidyverse)
library(fiphde)

set.seed(2023-01-30)

ilidat <-
  get_cdc_ili(region = c("national", "state", "hhs"), years = 2019:lubridate::year(lubridate::today())) %>%
  filter(abbreviation %in% c("US", "NY")) %>%
  filter(week_start>="2020-03-01" & week_start<="2022-06-18")
ilidat

ilifor <- forecast_ili(ilidat, horizon=4L, trim_date="2020-03-01")
ilifor

hosp_raw <-
  get_hdgov_hosp(limitcols=TRUE) %>%
  filter(state %in% c("US", "NY")) %>%
  filter(date>="2020-10-18" & date<="2022-06-18")
hosp_raw

prepped_hosp <- prep_hdgov_hosp(hosp_raw)
prepped_hosp

prepped_tsibble <- make_tsibble(prepped_hosp,
                                epiyear = epiyear,
                                epiweek=epiweek,
                                key=location)
prepped_tsibble

hosp_fitfor <- ts_fit_forecast(prepped_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               covariates=TRUE)
hosp_fitfor

prepped_forecast_ts <- format_for_submission(hosp_fitfor$tsfor, method = "ts")
prepped_forecast_ts

prepped_forecast_ts_cat <- forecast_categorical(prepped_forecast_ts$ensemble, prepped_hosp)
prepped_forecast_ts_cat

forcplot <- plot_forecast(prepped_tsibble, prepped_forecast_ts$ensemble)
forcplot

save(hosp_fitfor,
     hosp_raw,
     ilidat,
     ilifor,
     prepped_forecast_ts,
     prepped_forecast_ts_cat,
     prepped_hosp,
     prepped_tsibble,
     forcplot,
     file=here::here("inst/testdata/testdata.rd"))

