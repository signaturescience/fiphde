library(fiphde)
library(tidyverse)

# Get raw data from healthdata.gov
h_raw <- get_hdgov_hosp(limitcols=TRUE)
## save(h_raw, file="~/Downloads/h_raw.rd")
## load(file="~/Downloads/h_raw.rd")

# Prep, and make a tsibble
prepped_hosp <-
  h_raw %>%
  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0) %>%
  dplyr::filter(abbreviation != "DC")

prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
                                     epiyear = epiyear,
                                     epiweek=epiweek,
                                     key=location)
# Fit a model
hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               constrained=TRUE,
                               covariates=c("hosp_rank", "ili_rank"))

# format for submission
formatted_list <- ts_format_for_submission(hosp_fitfor$tsfor)
formatted_list

## TODO: turn this into a fiphde fun (and probably rename)
pf2 <- function(.data, submission, location="US", pi = TRUE) {

  ## pretty sure we need to add an intermediary variable for the filter below
  ## otherwise the condition will interpret as the column name not the vector ... i think?
  loc <- location

  # Check that the specified location is in the data and submission.
  stopifnot("Specified location is not in recorded data" = loc %in% unique(.data$location))
  stopifnot("Specified location is not in forecast data" = loc %in% unique(submission$location))

  # Grab the real data
  real <-
    .data %>%
    tibble::as_tibble() %>%
    dplyr::filter(location %in% loc) %>%
    dplyr::select(location, date=monday,point=flu.admits) %>%
    dplyr::mutate(type="recorded")

  # Grab the forecasted data
  forecasted <-
    submission %>%
    dplyr::filter(type=="point" | dplyr::near(quantile, 0.025) | dplyr::near(quantile, 0.975)) %>%
    dplyr::filter(location %in% loc) %>%
    dplyr::mutate(quantile=tidyr::replace_na(quantile, "point")) %>%
    dplyr::select(-type) %>%
    tidyr::separate(target, into=c("nwk", "target"), sep=" wk ahead ") %>%
    dplyr::select(location, date=target_end_date,quantile, value) %>%
    tidyr::spread(quantile, value) %>%
    dplyr::mutate(type="forecast")

  # Bind them
  bound <-
    dplyr::bind_rows(real, forecasted) %>%
    dplyr::arrange(date, location) %>%
    dplyr::left_join(dplyr::select(fiphde:::locations, location, location_name), by = "location") %>%
    dplyr::select(-location) %>%
    dplyr::rename(location = location_name)

  # Plot
  p <-
    bound %>%
    ggplot2::ggplot(ggplot2::aes(date, point)) +
    ggplot2::geom_point(ggplot2::aes(col=type)) +
    ggplot2::geom_line(ggplot2::aes(col=type)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    ggplot2::facet_wrap(~location, scales="free", ncol = 3) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Date", y = NULL) +
    ggplot2::theme(legend.position = "Bottom", legend.title = ggplot2::element_blank())

  if(pi) {
    p <-
      p +
      ggplot2::geom_ribbon(ggplot2::aes(fill = type, ymin = `0.025`, ymax = `0.975`),
                           alpha = 0.5, color="lightpink", data=dplyr::filter(bound, type == "forecast"))
  }

  return(p)
}

## arima
pf2(prepped_hosp, formatted_list$arima, location = "US")
pf2(prepped_hosp, formatted_list$arima, location = "06")
pf2(prepped_hosp, formatted_list$arima, location = "15")
pf2(prepped_hosp, formatted_list$arima, location = "51")

## ets
pf2(prepped_hosp, formatted_list$ets, location = "US")
pf2(prepped_hosp, formatted_list$ets, location = "06")
pf2(prepped_hosp, formatted_list$ets, location = "15")
pf2(prepped_hosp, formatted_list$ets, location = "51")

## ensemble
pf2(prepped_hosp, formatted_list$ensemble, location = "US")
pf2(prepped_hosp, formatted_list$ensemble, location = "06")
pf2(prepped_hosp, formatted_list$ensemble, location = "15")
pf2(prepped_hosp, formatted_list$ensemble, location = "51")

