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

# inspect each cvs for debugging
# a <- read_csv(fps[1])
# b <- read_csv(fps[2])
# c <- read_csv(fps[3])
# d <- read_csv(fps[4])

# bind all dfs together with filename as key
all_forecasts <- read_csv(fps, id = "filename") %>%
#  dplyr::mutate(filename = ))
  dplyr::mutate(filename = str_extract(filename, "([^/]+$)"))

#test_forecasts <- purrr::map_df(fps, read_csv)

unique(all_forecasts$filename)
dates <- unique(str_extract(fps, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))

sub_loc <- "04"
sub_date <- dates[1]
sub_forecasts <- all_forecasts %>% filter(forecast_date == sub_date)

plot_forecast(prepped_hosp, sub_forecasts, sub_loc)
