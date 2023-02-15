library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(fiphde)
library(purrr)
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

# If using ilinet(region="hhs") You'll get hhs regions as "Region 1", "Region 2", etc.
# Bind to this tibble above 10 more rows for the 10 hhs regions so the get_cdc_ili function will work out of the box.
locations <-
  tibble::tibble(abbreviation=1:10,
                 location=NA_character_,
                 location_name=paste("Region", abbreviation),
                 population=NA_real_) %>%
  dplyr::mutate(abbreviation=sprintf("HHS%02d", abbreviation)) %>%
  dplyr::mutate(location=abbreviation) %>%
  bind_rows(locations)

# Locations with "rate change"  ---------------------------------------------------------------

## provenance of data file
# download.file("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv",
#               destfile=here::here("data-raw/rate-change.csv"))

## read the location crosswalk table from file downloaded from github
## select columns of interest
rate_change <-
  read_csv(here::here("data-raw/rate-change.csv"), col_types="cccddd") %>%
  select(location, count_rate1per100k, count_rate2per100k)

# Quantiles ---------------------------------------------------------------

# quantiles needed
q <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
# Figure out what the interval you need to get those quantiles
qi <-
  tibble(lower=q[q<=.5], upper=rev(q[q>=.5])) %>%
  mutate(interval=as.integer(round((upper-lower)*100)))
qi
# The quidk (say: "quiddick") tibble: QUantile, Interval, Direction, Key
quidk <-
  qi %>%
  gather(direction, quantile, lower, upper) %>%
  mutate(key=paste0(interval, "%_", direction)) %>%
  arrange(quantile) %>%
  select(quantile, interval, direction, key)
quidk



# Historical severity -----------------------------------------------------

# Get historical hospitalization data (2010-2021), convert to counts, summarize by epiweek
hosp <- fiphde:::hospitalizations(surveillance_area = "flusurv", region="all")
hosp
hospstats <-
  hosp %>%
  filter(age_label == "Overall") %>%
  filter(year %>% between(2010, 2021)) %>%
  mutate("counts" = as.numeric(weeklyrate)*3300) %>%
  dplyr::select(c("wk_start", "counts", "year_wk_num")) %>%
  rename(epiweek = year_wk_num) %>%
  group_by(epiweek) %>%
  summarise(min = min(counts),
            lowhinge = IQR(counts, 0.25),
            med = median(counts),
            uprhinge = IQR(counts, 0.75),
            max = max(counts),
            mean = mean(counts))
hospstats

# Get weighted and unweighted ILI (2010-2019), summarize by epiweek
ili <- fiphde:::ilinet(region="national")
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
#hosp <- hospitalizations(surveillance_area = "flusurv", region="all")
#hosp
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


# Create data used in vignette --------------------------------------------

# vignette data
vd <- list()
# Get data include in vignette
vd$hosp <- get_hdgov_hosp(limitcols = TRUE)
# Limit to the previous epiweek
vd$hosp <- vd$hosp %>% filter(date <= "2022-05-28")

# Prep data
vd$prepped_hosp <-
  vd$hosp %>%
  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
  dplyr::filter(abbreviation != "DC")

# Make tsibble
vd$prepped_hosp_tsibble <- make_tsibble(vd$prepped_hosp,
                                        epiyear = epiyear,
                                        epiweek=epiweek,
                                        key=location)


# Fit model
vd$hosp_fitfor <- ts_fit_forecast(vd$prepped_hosp_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               covariates=c("hosp_rank", "ili_rank"))

# Format for submission
vd$formatted_list <- format_for_submission(vd$hosp_fitfor$tsfor)

# CREG ILI data - stuff in vd$ is created here and saved
# # Original: no time limit and nowcast
# vd$ilidat <-
#   get_cdc_ili(region=c("state"), years=2019:2022) %>%
#   filter(region == "Hawaii") %>%
#   replace_ili_nowcast(., weeks_to_replace=1)
# Modified: limit dates to when this vignette was originally created
vd$ilidat <-
  get_cdc_ili(region=c("state"), years=2019:2022) %>%
  filter(region == "Hawaii") %>%
  filter(week_start<="2022-06-05")

vd$ilifor <- forecast_ili(vd$ilidat, horizon=4L, trim_date="2020-03-01")

# This stuff is done in the vignette
ilidat <- vd$ilidat
ilifor <- vd$ilifor
prepped_hosp <- vd$prepped_hosp

ilidat <- ilidat %>% mutate(weighted_ili=mnz_replace(weighted_ili))

ilifor$ilidat     <- ilifor$ilidat     %>% mutate(ili=mnz_replace(ili))
ilifor$ili_future <- ilifor$ili_future %>% mutate(ili=mnz_replace(ili))
ilifor$ili_bound  <- ilifor$ili_bound  %>% mutate(ili=mnz_replace(ili))

dat_hi <-
  prepped_hosp %>%
  filter(abbreviation=="HI") %>%
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  left_join(ilifor$ilidat, by = c("epiyear", "location", "epiweek")) %>%
  mutate(ili = log(ili))

models <-
  list(
    poisson = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "poisson"),
    quasipoisson = trending::glm_model(flu.admits ~ ili + hosp_rank + ili_rank, family = "quasipoisson"),
    negbin = trending::glm_nb_model(flu.admits ~ ili + hosp_rank + ili_rank)
  )

new_cov <-
  ilifor$ili_future %>%
  left_join(fiphde:::historical_severity, by="epiweek") %>%
  select(-epiweek,-epiyear) %>%
  mutate(ili = log(ili))


res <- glm_wrap(dat_hi,
                new_covariates = new_cov,
                .models = models,
                alpha = c(0.01, 0.025, seq(0.05, 0.5, by = 0.05)) * 2)

# Put the final res object back in vd
vd$res <- res

# Remove need for cdcfluview for dev/build
# cdcfluview:::mmwrid_map %>% write_csv(here::here("data-raw/mmwrid_map.csv"))
mmwrid_map <- readr::read_csv(here::here("data-raw/mmwrid_map.csv"), col_types="DDii")


# Get ILI Nearby data -----------------------------------------------------

# Last updated: 2023-02-15
if (!file.exists(here::here("data-raw/ilinearby.csv"))) {
  message("Archiving ilinearby data in data-raw/ilinearby.csv")
  ilinearby <-
    crossing(years=as.character(2010:2022),
             weeks=stringr::str_pad(1:53, width=2, pad="0")) %>%
    unite(eyw, years, weeks, sep="") %>%
    mutate(data=map(eyw, ~fiphde::get_nowcast_ili(epiyearweeks=., dates=NULL)))
  ilinearby <-
    ilinearby %>%
    # mutate(missing=map_lgl(data, identical, NA)) %>%
    # filter(missing) %>%
    unnest(cols=data) %>%
    filter(!is.na(weighted_ili_now)) %>%
    arrange(eyw) %>%
    select(-eyw)
  # Fix FL 2020:53
  missing_fl202053 <-
    ilinearby %>%
    filter(abbreviation=="FL") %>%
    filter(epiyear==2020 & epiweek==53) %>%
    nrow() %>%
    identical(0L)
  if (missing_fl202053) {
    message("fixing florida 2020:53")
    wilinow_fl202053 <-
      ilinearby %>%
      filter(abbreviation=="FL") %>%
      filter((epiyear==2020 & epiweek==52) | (epiyear==2021 & epiweek==1)) %>%
      pull(weighted_ili_now) %>%
      median()
    ilinearby <-
      ilinearby %>%
      add_row(location="12", abbreviation="FL", epiyear=2020L, epiweek=53L, weighted_ili_now=wilinow_fl202053) %>%
      arrange(epiyear, epiweek, location)
  }
  # write to file
  ilinearby %>% write_csv(here::here("data-raw/ilinearby.csv"))
} else {
  message("Reading in ili nearby csv previously archived in data-raw/ilinearby.csv")
  ilinearby <- read_csv(here::here("data-raw/ilinearby.csv"), col_types="cciid")
}

# Write package data ------------------------------------------------------

usethis::use_data(locations,
                  rate_change,
                  q,
                  quidk,
                  historical_severity,
                  hospstats,
                  vd,
                  mmwrid_map,
                  ilinearby,
                  internal = TRUE, overwrite = TRUE)
