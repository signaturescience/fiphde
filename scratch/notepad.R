# library(focustools)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(fiphde)

## Test input multiple data frames
# get forecast csv names
data_dir <- "C:/Users/sjessa/OneDrive - Signature Science, LLC/Documents/FIPDHE/fiphde/submission"
fps <- rev(list.files(data_dir, pattern = "*.csv$",
                      recursive = TRUE, full.names = TRUE))

# read in ground truth data
prepped_hosp <-
  get_hdgov_hosp(limitcols = TRUE) %>%
  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
  dplyr::filter(abbreviation != "DC")

# bind all dfs together with filename as key
read_forc <- function(fp) {
  tmp <- read_csv(fp)
  tmp %>%
    mutate(filename = basename(fp)) %>%
    mutate(model = basename(dirname(fp)))
}

sub_forecasts <- map_df(fps, read_forc)

dates <- unique(str_extract(fps, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))

sub_loc <- "04"
sub_date <- dates[1]
sub_forecasts <- sub_forecasts %>% filter(forecast_date == sub_date)

plot_forecast(prepped_hosp, sub_forecasts, sub_loc)

# draft summary tables
# previous_week <-
#   prepped_hosp %>%
#   dplyr::as_tibble() %>%
#   dplyr::group_by(location) %>%
#   ## restrict to appropriate epiyear/epiweek for week prior to submission
#   #dplyr::filter(epiyear == previous_ey, epiweek == previous_ew) %>%
#   ## add a column for horizon 0 so we can stack on submission data (see below)
#   dplyr::mutate(horizon = 0) %>%
#   dplyr::select(horizon, location, count = flu.admits) %>%
#   dplyr::left_join(select(fiphde:::locations, location, location_name)) %>%
#   ungroup() %>%
#   select(-location) %>%
#   select(location = location_name, week = horizon, count)
