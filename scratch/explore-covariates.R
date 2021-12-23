library(tidyverse)
library(fiphde)
library(fable)
ilidat <- get_cdc_ili(region=c("national", "state"), years=2010:lubridate::year(lubridate::today()))

ilidat_us <- ilidat %>%
  filter(location=="US") %>%
  select(location, epiyear, epiweek, week_start, ili=unweighted_ili)
ilidat_us

ilidat_us %>%
  make_tsibble(epiyear=epiyear, epiweek=epiweek, key=location)

fit <- model(ilidat_us, arima=ARIMA(ili), ets=ETS(ili))
