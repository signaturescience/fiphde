library(tidyverse)
d <- cdcfluview::hospitalizations()
d %>%
  count(age_label)
d %>%
  count(surveillance_area)
?cdcfluview::hospitalizations
d %>% count(region)

cdcfluview::surveillance_areas()
glimpse(cdcfluview::hospitalizations("ihsp", "Virginia", years=2015))

library(cdcfluview)

df <- hospitalizations("flusurv")
de <- hospitalizations("eip")
di <- hospitalizations("ihsp")

df %>% ggplot(aes(wk_start, rate)) + geom_line(aes(col=age_label, group=age_label)) + facet_wrap(~sea_description, scales="free_x")
de %>% ggplot(aes(wk_start, rate)) + geom_line(aes(col=age_label, group=age_label)) + facet_wrap(~sea_description, scales="free_x")
di %>% ggplot(aes(wk_start, rate)) + geom_line(aes(col=age_label, group=age_label)) + facet_wrap(~sea_description, scales="free_x")

df %>%
  mutate(number=rate*100000) %>%
  filter(age_label=="Overall") %>%
  ggplot(aes(wk_end, number)) +
  geom_point(aes(col=sea_description)) +
  geom_line(aes(col=sea_description))
