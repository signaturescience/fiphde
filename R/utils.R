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
