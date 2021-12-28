library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

# Location metadata -------------------------------------------------------

## Provenance of this file
# download.file("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv",
#               destfile=here::here("data-raw/locations.csv"))

# Read in locations data
locations <- read_csv(here::here("data-raw/locations.csv"), col_types="cccd")

## exclude DC county code because DC will be a state/territory
locations <-
  locations %>%
  dplyr::filter(location != "11001")

# If using cdcfluview::ilinet(region="hhs") You'll get hhs regions as "Region 1", "Region 2", etc.
# Bind to this tibble above 10 more rows for the 10 hhs regions so the get_cdc_ili function will work out of the box.
locations <-
  tibble::tibble(abbreviation=1:10,
                 location=NA_character_,
                 location_name=paste("Region", abbreviation),
                 population=NA_real_) %>%
  dplyr::mutate(abbreviation=sprintf("HHS%02d", abbreviation)) %>%
  dplyr::mutate(location=abbreviation) %>%
  bind_rows(locations)


# Quantiles ---------------------------------------------------------------

# quantiles needed
q <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
# # Figure out what the interval you need to get those quantiles
# qi <-
#   tibble(lower=q[q<.5], upper=rev(q[q>.5])) %>%
#   mutate(interval=as.integer(round((upper-lower)*100)))
# qi
# # The quidk (say: "quiddick") tibble: QUantile, Interval, Direction, Key
# quidk <-
#   qi %>%
#   gather(direction, quantile, lower, upper) %>%
#   mutate(key=paste0(interval, "%_", direction)) %>%
#   arrange(quantile) %>%
#   select(quantile, interval, direction, key)
# quidk



# Historical severity -----------------------------------------------------

# Get weighted and unweighted ILI (2010-2019), summarize by epiweek
ili <- cdcfluview::ilinet(region="national")
ili
ilisum <-
  ili %>%
  filter(year %>% between(2010, 2019)) %>%
  group_by(week) %>%
  summarize(ili_mean=mean(unweighted_ili)) %>%
  rename(epiweek=week) %>%
  mutate(ili_rank=rank(ili_mean) %>% as.integer())
ilisum

# Get historical hospitalization data (2010-2019), summarize by epiweek
hosp <- cdcfluview::hospitalizations(surveillance_area = "flusurv", region="all")
hosp
hospsum <-
  hosp %>%
  filter(year %>% between(2010, 2019)) %>%
  filter(age_label=="Overall") %>%
  rename(epiweek=year_wk_num) %>%
  group_by(epiweek) %>%
  summarize(hosp_mean=mean(weeklyrate)) %>%
  mutate(hosp_rank=rank(hosp_mean) %>% as.integer())
hospsum

# join ili and hosp summaries
ih <-
  left_join(ilisum, hospsum, by="epiweek") %>%
  mutate(across(everything(), replace_na, 0)) %>%
  mutate(day=(epiweek-1)*7+1, .after=epiweek)
ih

# # interpolate (see https://github.com/signaturescience/fluforce-init/blob/f8b985c0c6eb5b96bcf32f9f7a568e0265ec50c6/ilinet-epiweek.R#L47-L54)
# # first create an approximation function based on day (x) and value (y) in the joined data above.
# # run that function on the day variable in this tibble, which runs 1:365
# # left join the original data back in and add which ones are interpolated
# historical_severity <-
#   tibble(day=1:max(ih$day)) %>%
#   mutate(wili=approxfun(x=ih$day, y=ih$wili)(day)) %>%
#   mutate(uili=approxfun(x=ih$day, y=ih$uili)(day)) %>%
#   mutate(hosp=approxfun(x=ih$day, y=ih$hosp)(day)) %>%
#   left_join(ih) %>%
#   relocate(epiweek) %>%
#   mutate(interpolated=is.na(epiweek), .after=day) %>%
#   tidyr::fill(epiweek, .direction="down")
# historical_severity
#
# historical_severity_tidy <-
#   historical_severity %>%
#   gather(metric, value, wili, uili, hosp)
# ggplot(ihitidy, aes(day, value)) +
#   geom_line(lwd=.2) +
#   geom_point(size=.5) +
#   geom_point(data=filter(historical_severity_tidy, !interpolated), aes(day, value), col="red") +
#   facet_wrap(~metric, scale="free_y", ncol=1)
# rm(historical_severity_tidy)

# Skip all the interpolation stuff
historical_severity <- ih %>% select(-day)

historical_severity %>%
  gather(key, value, -epiweek) %>%
  ggplot(aes(epiweek, value)) + geom_point() + geom_line() + facet_wrap(~key, scale="free_y")

# Write package data ------------------------------------------------------

usethis::use_data(locations, q, historical_severity, internal = TRUE, overwrite = TRUE)
