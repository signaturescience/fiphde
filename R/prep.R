#' @title Prep hospitalization data
#' @description Prep healthdata.gov hospitalization data retrieved using [get_hdgov_hosp] for downstream forecasting. Limits to states only, trims to where you have data, removes the incomplete week, and removes locations with little reporting over the last month.
#' @param h_raw Raw hospitalization data from [get_hdgov_hosp]
#' @param statesonly Limit to US+DC+States only (i.e., drop territories)? Defaults to `TRUE`.
#' @param trim list with epiyear and epiweek to trim on. Defaults to October 25, 2020 (2020:43). There isn't any data before this.
#' @param remove_incomplete Remove the last week if incomplete? Defaults to `TRUE`.
#' @param min_per_week The minimum number of flu.admits per week needed to retain that state. Default removes states with less than 1 flu admission per week over the last 30 days.
#' @return A tibble; hospitalization data ready for downstream forecasting.
#' @export
#' @examples
#' \dontrun{
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#' h <- prep_hdgov_hosp(h_raw)
#' h
#' hts <- make_tsibble(h, epiyear=epiyear, epiweek=epiweek, key=location)
#' hts
#' }
prep_hdgov_hosp <- function(h_raw,
                            statesonly=TRUE,
                            trim=list(epiyear=2020, epiweek=43),
                            remove_incomplete=TRUE,
                            min_per_week=1) {
  # What's the last date you have data on? You'll need this to chop the data later on.
  last_date <- max(h_raw$date)

  # Summarize to epiyear, epiweek
  message("Summarizing to epiyear/epiweek")
  hweek <- h_raw %>%
    dplyr::rename(location=state) %>%
    dplyr::mutate(epiyear=lubridate::epiyear(date), epiweek=lubridate::epiweek(date), .after=date) %>%
    dplyr::group_by(location, epiyear, epiweek) %>%
    dplyr::summarize(dplyr::across(c(flu.admits, flu.admits.cov), sum, na.rm=TRUE), .groups="drop")

  # Summarize across states to get US data then bind back to states to get states+US.
  h <- hweek %>%
    dplyr::group_by(epiyear, epiweek) %>%
    dplyr::summarize(dplyr::across(c(flu.admits, flu.admits.cov), sum, na.rm=TRUE), .groups="drop") %>%
    dplyr::mutate(location = "US", .before = 1) %>%
    dplyr::bind_rows(hweek) %>%
    dplyr::arrange(epiyear, epiweek, location)

  # add back dates
  h <-
    h %>%
    dplyr::mutate(week_start=MMWRweek::MMWRweek2Date(epiyear, epiweek, 1),
                  monday=MMWRweek::MMWRweek2Date(epiyear, epiweek, 2),
                  week_end=MMWRweek::MMWRweek2Date(epiyear, epiweek, 7),
                  .before=epiyear)

  # Trim to when you actually have data
  if (!is.null(trim) && is.list(trim)) {
    message(sprintf("Trimming to %s", MMWRweek::MMWRweek2Date(trim$epiyear, trim$epiweek)))
    h <- h %>% dplyr::filter(week_start >= MMWRweek::MMWRweek2Date(trim$epiyear, trim$epiweek))
  }

  # Remove incomplete weeks
  if (remove_incomplete) {
    message(sprintf("Removing incomplete weeks (data prior to %s)", last_date))
    h <- h %>% dplyr::filter(week_end <= last_date)
  }

  # join in location codes
  h <- h %>%
    dplyr::rename(abbreviation=location) %>%
    dplyr::inner_join(locations, ., by="abbreviation") %>%
    dplyr::select(-location_name, -population)

  # Trim to US+states+DC only
  if (statesonly) {
    message("Filtering to US+DC+States only")
    h <-
      h %>%
      dplyr::filter(location %in% c("US", stringr::str_pad(1:56, width=2, pad="0")))
  }

  # Drop states without minimum flu.admits per week
  if (is.numeric(min_per_week)) {
    message(sprintf("Removing states with < %d flu.admits per week on average over the last month", min_per_week))
    dropstates <-
      h %>%
      dplyr::filter(week_end >= last_date-30) %>%
      dplyr::group_by(abbreviation) %>%
      dplyr::summarize(dplyr::across(c(flu.admits, flu.admits.cov), mean, na.rm=TRUE)) %>%
      dplyr::arrange(flu.admits, flu.admits.cov) %>%
      dplyr::filter(flu.admits<min_per_week) %>%
      dplyr::pull(abbreviation) %>%
      unique()
    h <- h %>%
      dplyr::filter(!(abbreviation %in% dropstates))
    message(sprintf("Removed %d states: %s", length(dropstates), paste(dropstates, collapse=" ")))
  }

  # Join in the historical data
  h <-
    h %>%
    dplyr::inner_join(historical_severity, by="epiweek")

  return(h)
}

