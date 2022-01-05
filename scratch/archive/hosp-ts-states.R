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

# states with small numbers
states_with_low_numbers <-
  h_raw %>%
  filter(date >= last_date-30) %>%
  group_by(state) %>%
  summarize(across(c(flu.admits, flu.admits.cov), mean, na.rm=TRUE)) %>%
  arrange(flu.admits, flu.admits.cov) %>%
  filter(flu.admits<1) %>%
  pull(state) %>%
  unique()


# Summarize to epiyear, epiweek
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

# Join in the historical data
h <- h %>%
  inner_join(fiphde:::historical_severity, by="epiweek")

# tsibble/fable -----------------------------------------------------------

# Forecast horizon in weeks
horizon <- 4

# hospitalization tsibble
hts <-
  h %>%
  filter(!(location %in% states_with_low_numbers)) %>%
  # filter(location %in% c("US", "NY", "FL", "VA")) %>%
  # filter(location=="US") %>%
  mutate(location=location %>% factor() %>% fct_relevel("US")) %>%
  make_tsibble(epiyear, epiweek, key=location)

# Function to make new data with historical epiweek severity
make_new_data <- function(.data, .horizon=4) {
  tsibble::new_data(.data, n=.horizon) %>%
    dplyr::mutate(epiweek=lubridate::epiweek(yweek)) %>%
    dplyr::inner_join(fiphde:::historical_severity, by="epiweek")
}
# example:
# make_new_data(hts)


# Model (current) for the next four weeks, no exogenous regressors
tsfit <-
  hts %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        # boxcosxets=ETS(box_cox(flu.admits, .5) ~ season(method="N")),
        # sqrtets=ETS(sqrt(flu.admits) ~ season(method="N")),
        naive=NAIVE(flu.admits),
        drift=RW(flu.admits~drift()),
        arima=ARIMA(flu.admits~PDQ(0,0,0))
  ) %>%
  mutate(ensemble=(ets+arima)/2)

# forecast
tsfor <- tsfit %>% forecast(h=horizon)

p1 <- tsfor %>% autoplot(hts, level=10) + ggtitle("No exogenous regressors")


# Model (current) for the next four weeks, with exogenous regressors
tsfit_exo <-
  hts %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        # boxcosxets=ETS(box_cox(flu.admits, .5) ~ season(method="N")),
        # sqrtets=ETS(sqrt(flu.admits) ~ season(method="N")),
        naive=NAIVE(flu.admits),
        drift=RW(flu.admits~drift()),
        arima=ARIMA(flu.admits~PDQ(0,0,0) + hosp_rank + ili_rank)
  ) %>%
  mutate(ensemble=(ets+arima)/2)

# forecast
tsfor_exo <- tsfit_exo %>% forecast(new_data=make_new_data(hts))

p2 <- tsfor_exo %>% autoplot(hts, level=10) + ggtitle("Including hospitalization rank in regression")

library(patchwork)
pcombined <- p1+p2 + plot_layout(guides="collect")
ggsave("~/Downloads/state-level-forecasts.png", width=6, height=40, plot=pcombined, scale=1.2)
