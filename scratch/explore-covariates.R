library(tidyverse)
library(fiphde)
# library(fable)

# Get ili data from 2010 forward
i <- get_cdc_ili(region=c("national", "state"), years=2010:lubridate::year(lubridate::today()))

# US only
iu <-
  i %>%
  filter(location=="US") %>%
  select(location, epiyear, epiweek, week_start, ili=unweighted_ili)
iu %>% tail()

# Get hosp data
h <- get_hdgov_hosp()
h

# weekly data summed across states
hu <-
  h %>%
  mutate(date = date - 1) %>%
  mutate(epiweek = lubridate::epiweek(date),
         epiyear = lubridate::epiyear(date),
         .after=date) %>%
  group_by(epiyear, epiweek) %>%
  summarize(across(c(flu.admits, flu.admits.cov), sum, na.rm=TRUE), .groups="drop") %>%
  mutate(location = "US", .before = "epiyear")
hu

hu %>%
  left_join(iu, by = c("location", "epiyear", "epiweek")) %>%
  inner_join(fiphde:::historical_severity, by=c("epiweek"))


ilidat_us %>%
  make_tsibble(epiyear=epiyear, epiweek=epiweek, key=location)

fit <- model(ilidat_us, arima=ARIMA(ili), ets=ETS(ili))
