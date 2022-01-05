#' Get Monday
#'
#' @description
#'
#' This function is a helper to get the date for the Monday of the current week.
#'
#' @return Date for the Monday of the current week. For more details see \link[lubridate]{floor_date}.
#' @export
#' @md
#'
this_monday <- function() {
  lubridate::floor_date(lubridate::today(), "weeks", week_start = 1)
}

#' Check Monday
#'
#' @description
#'
#' This is a helper function to see if today is Monday.
#
#' @return Logical indicating whether or not today is Monday
#' @export
#' @md
is_monday <- function() {
  lubridate::wday(lubridate::today(), label=TRUE) %in% c("Mon")
}

#' Calculate WIS score
#'
#' Helper function to calculate weighted interval score (WIS) for prepped forecasts
#'
#' @param .forecasts Tibble with prepped foreacsts
#' @param .test Tibble with test data including observed value for flu admissions stored in "flu.admits" column
#'
#' @return Tibble with the WIS for each combination of epiweek and epiyear
#' @export
#'
wis_score <- function(.forecasts, .test) {
  .forecasts %>%
    dplyr::left_join(.test) %>%
    dplyr::select(epiweek,epiyear,quantile,value,flu.admits) %>%
    dplyr::group_by(epiweek, epiyear) %>%
    dplyr::summarise(wis = fiphde::weighted_interval_score(quantile = quantile, value = value, actual_value = flu.admits))
}

#' Plot forecasts against observed data
#'
#' This helper function creates a plot to visualize the forecasted point estimates (and 95% prediction interval) alongside the observed data.
#'
#' @param .forecasts Tibble with prepped forecasts
#' @param .train Tibble with data used for modeling
#' @param .test Tibble with observed data held out from modeling
#'
#' @return A `ggplot2` plot object
#' @export
#'
#'
plot_forc <- function(.forecasts, .train, .test) {

  forc_dat <-
    .forecasts %>%
    dplyr::filter(quantile %in% c(NA,0.025,0.975)) %>%
    tidyr::spread(quantile,value) %>%
    dplyr::rename(lower = `0.025`, upper = `0.975`, mean = `<NA>`)

  .test %>%
    dplyr::bind_rows(.train) %>%
    dplyr::select(epiweek,epiyear, truth = flu.admits, location) %>%
    dplyr::left_join(forc_dat) %>%
    dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(date,truth), lwd = 2, col = "black") +
    ggplot2::geom_line(ggplot2::aes(date,mean), lwd = 2, alpha = 0.5, lty = "solid", col = "firebrick") +
    ggplot2::geom_ribbon(ggplot2::aes(date, ymin = lower, ymax = upper), alpha = 0.25, fill = "firebrick") +
    ## get an upper limit from whatever the max of observed or forcasted hospitalizations is
    ggplot2::scale_y_continuous(limits = c(0,max(c(.test$flu.admits, .train$flu.admits, forc_dat$upper)))) +
    ggplot2::scale_x_date(date_labels = "%Y-%m", date_breaks = "month") +
    ggplot2::labs(x = "Date", y = "Count", title = "Influenza hospitalizations") +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~ location)

}


#' @title Replace ILInet with nowcast data
#' @description Replaces `weighted_ili` from [get_cdc_ili] with nowcast data from [get_nowcast_ili] for the number of specified `weeks_to_replace`.
#' @param ilidat Data from [get_cdc_ili].
#' @param weeks_to_replace Number of weeks of `ilidat` to replace. Defaults to 2.
#' @return The same as the `ilidat` input, but with `weeks_to_replace` weeks replaced with nowcasted data.
#' @export
#' @examples
#' \dontrun{
#' ilidat <- get_cdc_ili(years=2021)
#' ilidat <-
#'   ilidat %>%
#'   dplyr::filter(location=="US" | abbreviation=="VA") %>%
#'   dplyr::group_by(location) %>%
#'   dplyr::slice_max(week_start, n=4) %>%
#'   dplyr::select(location:weighted_ili)
#' ilidat
#' iliaug <- replace_ili_nowcast(ilidat, weeks_to_replace=1)
#' iliaug
#'
#' # arrange for comparison
#' ilidat <- ilidat %>% dplyr::arrange(location, week_start)
#' iliaug <- iliaug %>% dplyr::arrange(location, week_start)
#' # Compare US
#' waldo::compare(ilidat %>% dplyr::filter(location=="US"),
#'                iliaug %>% dplyr::filter(location=="US"))
#' # Compare VA
#' waldo::compare(ilidat %>% dplyr::filter(location=="51"),
#'                iliaug %>% dplyr::filter(location=="51"))
#' }
replace_ili_nowcast <- function(ilidat, weeks_to_replace=1) {
  # How many days back do you need to go? 1 to weeks+1, *7
  days_back <- (1:(weeks_to_replace+1))*7
  # What are those dates?
  dates_back <- lubridate::today() - days_back
  ilinow <- get_nowcast_ili(dates=dates_back)
  ilinow <- ilinow %>% dplyr::filter(location %in% ilidat$location)
  message(paste0("Replacing weighted_ili with nowcast weighted_ili on dates: ", paste(dates_back, collapse=", ")))
  res <-
    ilidat %>%
    dplyr::full_join(ilinow, by = c("location", "abbreviation", "epiyear", "epiweek")) %>%
    dplyr::mutate(week_start=MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
    dplyr::arrange(location, week_start) %>%
    tidyr::fill(region_type, region, .direction="down") %>%
    dplyr::mutate(weighted_ili=ifelse(!is.na(weighted_ili_now), weighted_ili_now, weighted_ili)) %>%
    dplyr::select(-weighted_ili_now)
  # We expect to have one extra row per location in the result compared to the input.
  if (nrow(ilidat)!=nrow(res)-length(unique(res$location))) warning("Unexpected number of rows returned in result.")
  return(res)
}

#' Plot forecasts
#'
#' This function serves as a plotting mechanism for prepped forecast submission data. Using truth data supplied, the plots show the historical trajectory of weekly flu hospitalizations along with the point estimates for forecasts. Optionally, the user can include 95% prediction interval as well. Plots include trajectories of weekly flu hospitalizations faceted by location.
#'
#' @param .data 	Historical truth data for all locations and outcomes in submission targets
#' @param submission Formatted submission
#' @param location  Vector specifying locations to filter to; 'US' by default.
#' @param pi Logical as to whether or not the plot should include 95% prediction interval; default is `TRUE`
#'
#' @return A `ggplot2` plot object with line plots for outcome trajectories faceted by location
#' @export
#'
#' @md
#'
#'
plot_forecast <- function(.data, submission, location="US", pi = TRUE) {

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
    dplyr::left_join(dplyr::select(locations, location, location_name), by = "location") %>%
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
