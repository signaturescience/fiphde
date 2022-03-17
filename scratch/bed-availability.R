library(tidyverse)
library(fiphde)
theme_set(theme_bw())

h <- get_hdgov_hosp()
h
hs <- h %>%
  filter(date>="2021-10-03") %>%
  select(state, date,
         flu.admits,
         # flu.cov=flu.admits.cov,
         cov.admits,
         # covid.cov=cov.admits.cov,
         beds=inpatient_beds, beds.used=inpatient_beds_used,
         beds.used.covid=inpatient_beds_used_covid) %>%
  mutate(epiyear=lubridate::epiyear(date), epiweek=lubridate::epiweek(date), .after=date) %>%
  select(-date) %>%
  group_by(state, epiyear, epiweek) %>%
  summarize(across(everything(), mean, na.rm=TRUE), .groups="drop") %>%
  select(-state) %>%
  group_by(epiyear, epiweek) %>%
  summarize(across(everything(), sum, na.rm=TRUE), .groups="drop") %>%
  mutate(date=MMWRweek::MMWRweek2Date(epiyear, epiweek), .before=1) %>%
  slice(1:(n()-1)) %>%
  mutate(beds.capacity=beds.used/beds) %>%
  mutate(beds.avail=beds-beds.used)

hs %>%
  select(date, beds, beds.used, beds.capacity, beds.avail) %>%
  gather(key, value, -date) %>%
  ggplot(aes(date, value)) + geom_line() + geom_point() + facet_wrap(~key, scale="free_y")

hs %>%
  select(-epiyear, -epiweek) %>%
  gather(key, value, flu.admits, beds.used.covid) %>%
  ggplot(aes(date, value)) + geom_line() + geom_point() +
  facet_wrap(~key, scale="free_y", ncol=1) +
  geom_vline(xintercept=hs$date[which(hs$flu.admits==max(hs$flu.admits))], lty=2, col="blue2") +
  geom_vline(xintercept=hs$date[which(hs$beds.used.covid==max(hs$beds.used.covid))], lty=2, col="red2")

hs %>%
  filter(date>"2022-01-01") %>%
  ggplot(aes(beds.used.covid, flu)) + geom_point() + geom_smooth(method="lm")

summary(lm(flu~beds.used.covid, data=hs %>% filter(date>"2022-01-01")))
