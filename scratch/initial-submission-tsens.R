library(fiphde)
library(tidyverse)

# Get raw data from healthdata.gov
h_raw <- get_hdgov_hosp(limitcols=TRUE)
## save(h_raw, file="~/Downloads/h_raw.rd")
## load(file="~/Downloads/h_raw.rd")

# Prep, and make a tsibble
prepped_hosp <-
  h_raw %>%
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
formatted_list <- ts_format_for_submission(hosp_fitfor$tsfor)
formatted_list

## arima
plot_forecast(prepped_hosp, formatted_list$arima, location = "US")
plot_forecast(prepped_hosp, formatted_list$arima, location = "06")
plot_forecast(prepped_hosp, formatted_list$arima, location = "15")
plot_forecast(prepped_hosp, formatted_list$arima, location = "51")

## ets
plot_forecast(prepped_hosp, formatted_list$ets, location = "US")
plot_forecast(prepped_hosp, formatted_list$ets, location = "06")
plot_forecast(prepped_hosp, formatted_list$ets, location = "15")
plot_forecast(prepped_hosp, formatted_list$ets, location = "51")

## ensemble
plot_forecast(prepped_hosp, formatted_list$ensemble, location = "US")
plot_forecast(prepped_hosp, formatted_list$ensemble, location = "06")
plot_forecast(prepped_hosp, formatted_list$ensemble, location = "15")
plot_forecast(prepped_hosp, formatted_list$ensemble, location = "51")

