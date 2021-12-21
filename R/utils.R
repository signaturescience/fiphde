#' Make `tsibble`
#'
#' @description
#'
#' This function converts an input `tibble` with columns for \link[lubridate]{epiyear} and \link[lubridate]{epiweek} into a \link[tsibble]{tsibble} object. The `tsibble` has columns specifying indices for the time series as well as a date for the Monday of the epiyear/epiweek combination at each row. Users can optionally ignore the current week when generating the `tsibble` via the "chop" argument.
#'
#' @param df A `tibble` containing columns `epiyear` and `epiweek`.
#' @param epiyear Unquoted variable name containing the MMWR epiyear.
#' @param epiweek Unquoted variable name containing the MMWR epiweek.
#' @param chop Logical indicating whether or not to remove the most current week (default `TRUE`).
#' @return A `tsibble` containing additional columns `monday` indicating the date
#'   for the Monday of that epiweek, and `yweek` (a yearweek vctr class object)
#'   that indexes the `tsibble` in 1 week increments.
#' @export
#' @md
make_tsibble <- function(df, epiyear, epiweek, chop=TRUE) {
  out <- df %>%
    # get the monday that starts the MMWRweek
    dplyr::mutate(monday=MMWRweek::MMWRweek2Date(MMWRyear={{epiyear}},
                                                 MMWRweek={{epiweek}},
                                                 MMWRday=2),
                  .after={{epiweek}}) %>%
    # convert represent as yearweek (see package?tsibble)
    dplyr::mutate(yweek=tsibble::yearweek(monday), .after="monday") %>%
    # convert to tsibble
    tsibble::as_tsibble(index=yweek, key=location)
  # Remove the incomplete week
  if (chop) out <- utils::head(out, -1)
  return(out)
}

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


