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

# Join in the historical data
h <- h %>%
  inner_join(fiphde:::historical_severity, by="epiweek")

# tsibble/fable -----------------------------------------------------------

# Forecast horizon in weeks
horizon <- 4

# hospitalization tsibble
hts <-
  h %>%
  # filter(week_start<"2021-10-01") %>%
  make_tsibble(epiyear, epiweek, key=location, chop=FALSE)


# US Only, current
htsus_c <- hts %>% filter(location=="US")

# US only, minus four weeks
htsus_4 <- htsus_c %>% head(nrow(htsus)-4)

# Model (current) for the next four weeks, no exogenous regressors
model_c <-
  htsus_c %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        arima=ARIMA(flu.admits~PDQ(0,0,0))) %>%
  mutate(ensemble=(ets+arima)/2)

# Forecast the next four weeks, no exogenous regressors
model_c %>% forecast(h=horizon) %>% autoplot(htsus_c, level=10)

# Model (minus four weeks) for the current four weeks, no exogenous regressors
model_4 <-
  htsus_4 %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        arima=ARIMA(flu.admits~PDQ(0,0,0))) %>%
  mutate(ensemble=(ets+arima)/2)

# Forecast the next four weeks, no exogenous regressors
model_4 %>% forecast(h=horizon) %>% autoplot(htsus_c, level=10)


# Function to make new data with historical epiweek severity
make_new_data <- function(.data, .horizon=4) {
  tsibble::new_data(.data, n=.horizon) %>%
    dplyr::mutate(epiweek=lubridate::epiweek(yweek)) %>%
    dplyr::inner_join(fiphde:::historical_severity)
}
# example:
# make_new_data(htsus_c)

# Model (current) for the next four weeks, with exogenous regressors
model_c_exo <-
  htsus_c %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        arima=ARIMA(flu.admits~PDQ(0,0,0) + hosp_rank)) %>%
  mutate(ensemble=(ets+arima)/2)

# Forecast the next four weeks, with exogenous regressors
model_c_exo %>% forecast(new_data=make_new_data(htsus_c)) %>% autoplot(htsus_c, level=10)

# Model (minus four weeks) for the current four weeks, with exogenous regressors
model_4_exo <-
  htsus_4 %>%
  model(ets=ETS(flu.admits ~ season(method="N")),
        arima=ARIMA(flu.admits~PDQ(0,0,0) + hosp_rank)) %>%
  mutate(ensemble=(ets+arima)/2)

# Forecast the next four weeks, with exogenous regressors
model_4_exo %>% forecast(new_data=make_new_data(htsus_4)) %>% autoplot(htsus_c, level=10)



# Loop over lots of previous weeks
plot_ts_forecast <- function(dat, weeksback=0) {
  origdat <- dat
  dat <- dat %>% head(nrow(dat)-weeksback)
  newdat <- make_new_data(dat)
  dat %>%
    model(ets=ETS(flu.admits ~ season(method="N")),
          arima=ARIMA(flu.admits~PDQ(0,0,0)+ hosp_rank)) %>%
    mutate(ensemble=(ets+arima)/2) %>%
    forecast(new_data=newdat) %>%
    autoplot(origdat, level=10) + ggtitle(max(dat$week_start))
}
# plot_ts_forecast(htsus_c, 0)
# plot_ts_forecast(htsus_c, 40)
plots <-
  tibble(weeksback=1:5) %>%
  mutate(plot=map(weeksback, ~plot_ts_forecast(htsus_c, .)))
plots <- map(0:60, ~plot_ts_forecast(htsus_c, .))
pdf("~/Downloads/tsplots.pdf", width=11.5, height=8)
print(plots)
dev.off()
