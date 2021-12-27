library(tidyverse)
library(fable)
library(fiphde)
theme_set(theme_bw())

# Get hosp data from healthdata.gov
h_raw <- get_hdgov_hosp(limitcols=TRUE)
# save(h_raw, file="~/Downloads/h_raw.rd")
# load(file="~/Downloads/h_raw.rd")
h_raw

# What's the last date you have data on? You'll need this to chop the data later on.
last_date <- max(h_raw$date)

# Fix date, summarize to epiyear, epiweek
hweek <- h_raw %>%
  rename(location=state) %>%
  mutate(date=date-1) %>%
  mutate(epiyear=lubridate::epiyear(date), epiweek=lubridate::epiweek(date), .after=date) %>%
  group_by(location, epiyear, epiweek) %>%
  summarize(across(c(flu.admits, flu.admits.cov), sum, na.rm=TRUE), .groups="drop")

# Summarize across states to get US data then bind back to states to get states+US.
h <- hweek %>%
  group_by(epiyear, epiweek) %>%
  summarize(across(c(flu.admits, flu.admits.cov), sum, na.rm=TRUE), .groups="drop") %>%
  mutate(location = "US", .before = 1) %>%
  bind_rows(hweek) %>%
  arrange(epiyear, epiweek, location)

# add back dates
h <-
  h %>%
  mutate(week_start=MMWRweek::MMWRweek2Date(epiyear, epiweek, 1),
         monday=MMWRweek::MMWRweek2Date(epiyear, epiweek, 2),
         week_end=MMWRweek::MMWRweek2Date(epiyear, epiweek, 7),
         .before=epiyear)

# Data doesn't really pick up until october 2020ish (2020:43).
# You also have incomplete weeks
# Take a look
h %>%
  filter(location=="US") %>%
  ggplot(aes(week_start, flu.admits)) + geom_point()
h %>% filter(location=="US") %>% filter(flu.admits>1)
# limit to 2020:43 and beyond
h <- h %>% filter(week_start >= MMWRweek::MMWRweek2Date(2020, 43))
# Now look again
h %>%
  filter(location=="US") %>%
  ggplot(aes(week_start, flu.admits)) + geom_point()
# You still have incomplete weeks. Get rid of those
h <- h %>% filter(week_end <= last_date)
# Now look again
h %>%
  filter(location=="US") %>%
  ggplot(aes(week_start, flu.admits)) + geom_point()


# tsibble/fable -----------------------------------------------------------

horizon <- 4

hts <-
  h %>%
  # filter(week_start<"2021-10-01") %>%
  make_tsibble(epiyear, epiweek, key=location, chop=FALSE)

# US Only
hts_us <- hts %>% filter(location=="US")

# Forecast the next four weeks
hts_us %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        arima=ARIMA(flu.admits~PDQ(0,0,0))) %>%
  mutate(ensemble=(ets+arima)/2) %>%
  forecast(h=horizon) %>%
  autoplot(hts, level=10)

# Forecast the previous four weeks
hts_us %>%
  head(nrow(hts_us)-4) %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        arima=ARIMA(flu.admits~PDQ(0,0,0))) %>%
  mutate(ensemble=(ets+arima)/2) %>%
  forecast(h=horizon) %>%
  autoplot(hts, level=10)

# Forecast the previous four weeks, with a wider confidence band
hts_us %>%
  head(nrow(hts_us)-4) %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        arima=ARIMA(flu.admits~PDQ(0,0,0))) %>%
  mutate(ensemble=(ets+arima)/2) %>%
  forecast(h=horizon) %>%
  autoplot(hts, level=c(80, 95))
