library(focustools)

## get data at the national scale from jhu source
usac <-  get_cases(source="jhu",  granularity = "national")
usad <- get_deaths(source="jhu",  granularity = "national")

## use the focustools helper to prep the tsibble format
usa <-
  dplyr::inner_join(usac, usad, by = c("location", "epiyear", "epiweek")) %>%
  make_tsibble(chop=TRUE)
