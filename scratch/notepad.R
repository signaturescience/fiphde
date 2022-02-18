# library(focustools)
#
# ## get data at the national scale from jhu source
# usac <-  get_cases(source="jhu",  granularity = "national")
# usad <- get_deaths(source="jhu",  granularity = "national")
#
# ## use the focustools helper to prep the tsibble format
# usa <-
#   dplyr::inner_join(usac, usad, by = c("location", "epiyear", "epiweek")) %>%
#   make_tsibble(chop=TRUE)


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

# all_forecasts <- read_csv(fps, id = "filename") %>%
# #  dplyr::mutate(filename = ))
#   dplyr::mutate(filename = str_extract(filename, "([^/]+$)"))

#test_forecasts <- purrr::map_df(fps, read_csv)

#unique(all_forecasts$filename)
dates <- unique(str_extract(fps, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))

sub_loc <- "04"
sub_date <- dates[1]
sub_forecasts <- sub_forecasts %>% filter(forecast_date == sub_date)

plot_forecast(prepped_hosp, sub_forecasts, sub_loc)
