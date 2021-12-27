library(tidyverse)
library(fiphde)
theme_set(theme_bw())
# library(fable)


# Get ILI and hosp data ---------------------------------------------------

# Get ili data from 2010 forward
i <- get_cdc_ili(region=c("national", "state"), years=2010:lubridate::year(lubridate::today()))
i

# US only ILI data
iu <-
  i %>%
  filter(location=="US") %>%
  select(location, epiyear, epiweek, week_start, ili=unweighted_ili)
iu
tail(iu)

# Get hosp data from healthdata.gov
h <- get_hdgov_hosp()
h
tail(h)

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

# join hospitalization data to ili data
hi <-
  hu %>%
  left_join(iu, by = c("location", "epiyear", "epiweek")) %>%
  inner_join(fiphde:::historical_severity, by=c("epiweek")) %>%
  mutate(week_start=MMWRweek::MMWRweek2Date(epiyear, epiweek))
tail(hi)

# How many weeks of ili data are you missing?
nmiss_ili <- sum(is.na(hi$ili))
message(nmiss_ili)

# lag by nmiss_ili weeks, and ililagma is moving average of previous four weeks
hi <- hi %>%
  arrange(week_start) %>%
  mutate(ililag=lag(ili, n = nmiss_ili), .after=ili) %>%
  mutate(ililag=ifelse(is.na(ililag), first(ili), ililag)) %>%
  mutate(ililagma=slider::slide_dbl(ili, mean, na.rm=TRUE, .before=4))
tail(hi)
head(hi)

# hospitalization data doesn't really pick up until flu season 2020-2021. Limit it to after 2020:42
ggplot(hi, aes(week_start, flu.admits)) + geom_point()
h %>% filter(flu.admits>1)
hi <-
  hi %>%
  filter(epiyear>=2020 & epiweek>=42)
ggplot(hi, aes(week_start, flu.admits)) + geom_point()


# EDA: which features most related to flu.admits --------------------------

library(ggpubr)
scatter <- function(.data, .x, .y) {
  ggscatter(.data, x=.x, y=.y,
            add="reg.line",
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int=TRUE,
            cor.coef=TRUE,
            cor.coeff.args = list(method = "pearson", label.x = 1, label.sep = "\n"))
}

# ILI, lagged ILI, and moving average ILI on flu admits
hi %>%
  scatter("ili", "flu.admits")
hi %>%
  scatter("ililag", "flu.admits")
hi %>%
  scatter("ililagma", "flu.admits")

# Coverage not strongly correlated
hi %>%
  scatter("flu.admits.cov", "flu.admits")

# Most recent lags more correlated than more distant lags
hi %>%
  mutate(lag=lag(flu.admits, 1)) %>%
  scatter("lag", "flu.admits")
hi %>%
  mutate(lag=lag(flu.admits, 2)) %>%
  scatter("lag", "flu.admits")
hi %>%
  mutate(lag=lag(flu.admits, 3)) %>%
  scatter("lag", "flu.admits")
hi %>%
  mutate(lag=lag(flu.admits, 4)) %>%
  scatter("lag", "flu.admits")
hi %>%
  mutate(lag=lag(flu.admits, 5)) %>%
  scatter("lag", "flu.admits")

# take historical severity into account
hi %>%
  scatter("hosp_mean", "flu.admits")
hi %>%
  scatter("hosp_rank", "flu.admits")
hi %>%
  scatter("ili_mean", "flu.admits")
hi %>%
  scatter("ili_rank", "flu.admits")

