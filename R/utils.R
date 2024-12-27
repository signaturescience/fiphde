#' @title Get Saturday
#'
#' @description
#'
#' This function is a helper to get the date for the Saturday of the current week. The function determines the current week based on epidemiological week orientation (i.e., week begins with Sunday).
#'
#' @return Date for the Saturday of the current week.
#' @export
#' @examples
#' this_saturday()
this_saturday <- function() {
  tmp <- MMWRweek::MMWRweek(lubridate::today())
  MMWRweek::MMWRweek2Date(tmp$MMWRyear, tmp$MMWRweek, 7)
}
#' @title Get Monday
#'
#' @description
#'
#' This function is a helper to get the date for the Monday of the current week. The function determines the current week based on epidemiological week orientation (i.e., week begins with Sunday).
#'
#' @return Date for the Monday of the current week.
#' @export
#' @examples
#' this_monday()
this_monday <- function() {
  tmp <- MMWRweek::MMWRweek(lubridate::today())
  MMWRweek::MMWRweek2Date(tmp$MMWRyear, tmp$MMWRweek, 2)
}
#' @title Check Monday
#'
#' @description
#'
#' This is a helper function to see if today is Monday.
#
#' @return Logical indicating whether or not today is Monday
#' @export
#' @examples
#' is_monday()
is_monday <- function() {
  lubridate::wday(lubridate::today(), label=TRUE) %in% c("Mon")
}


#' @title Replace ILINet data with nowcast
#'
#' @description
#'
#' This function replaces the weighted ILI retrieved from [get_cdc_ili] with nowcast data for each of the locations in the original data. The function will first attempt to use ILI Nearby nowcasts pulled using [get_nowcast_ili]. If the ILI Nearby nowcasts are unavailable, the function will optionally fallback to a pseudo nowcast method that averages the observed ILI for the 4 most recent weeks. The nowcast data will be used to add 1 additional week to the observed ILI data and (optionally) replace the number of weeks specified in the "weeks_to_replace" argument.
#'
#' @param ilidat ILI data retrieved via [get_cdc_ili]
#' @param start_date Date from which to start nowcasting; default is [lubridate::today]
#' @param weeks_to_replace Number of weeks of `ilidat` to replace; default is `1`
#' @param try_api Logical as to whether or not the function should try the ILI Nearby nowcast API; default is `TRUE`; if `FALSE` then the function will not attempt to query the API at all
#' @param fallback Logical as to whether or not to fall back to pseudo nowcast (average of last 4 ILI weeks in the given location) if nowcast data is unavailable; default is `TRUE`
#' @return A `tibble` with the following columns:
#'
#' - **location**: FIPS code for the location
#' - **region_type**: The type of location
#' - **abbreviation**: Abbreviation for the location
#' - **region**: Name of the region
#' - **epiyear**: Year of reporting (in epidemiological week calendar)
#' - **epiweek**: Week of reporting (in epidemiological week calendar)
#' - **week_start**: Date of beginning (Sunday) of the given epidemiological week
#' - **weighted_ili**: Population-weighted percentage of ILI outpatient visits
#'
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
#' }
replace_ili_nowcast <- function(ilidat, start_date = NULL, weeks_to_replace=1, fallback=TRUE, try_api=TRUE) {
  if (is.null(start_date)) start_date <- lubridate::today()
  # How many days back do you need to go? 1 to weeks+1, *7
  days_back <- (1:(weeks_to_replace+1))*7
  # What are those dates?
  dates_back <- start_date - days_back
  if(try_api) {
    ilinow <- get_nowcast_ili(dates=dates_back)
  } else {
    ilinow <- NA
  }
  ## handle case when delphi ili nowcast api doesnt return all of the nowcast data
  if(all(is.na(ilinow))) {
    if(fallback) {

      message("There was an issue retrieving the ILI nowcast data from the API. The fallback option is set to 'TRUE'. Using 4 most recent weeks of available data to generate pseudo nowcast for each location.")

      ilinow <-
        ilidat %>%
        dplyr::group_by(location) %>%
        dplyr::arrange(location,week_start) %>%
        ## get last 4 rows for ilidat for each location
        dplyr::filter(dplyr::row_number() > dplyr::n()-4) %>%
        ## compute weighted ili "nowcast" as average of last 4
        dplyr::summarise(weighted_ili_now = mean(weighted_ili, na.rm = TRUE)) %>%
        ## join to locations object to get abbreviation
        dplyr::left_join(dplyr::select(locations, abbreviation, location), by = "location") %>%
        ## create combinations of pseudo nowcast value and all dates / locations
        ## pseudo nowcast value will be same for all weeks specfied
        tidyr::crossing(dates_back, .) %>%
        ## get epiyear and epiweek
        dplyr::mutate(epiweek = lubridate::epiweek(dates_back),
                      epiyear = lubridate::epiyear(dates_back)) %>%
        dplyr::select(-dates_back)

    } else {
      stop("There was an issue retrieving the ILI nowcast data from the API. The fallback option is set to 'FALSE'. Cannot proceed.")
    }
  }
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
  # if (nrow(ilidat)!=nrow(res)-length(unique(res$location))) warning("Unexpected number of rows returned in result.")
  return(res)
}

#' @title Plot forecasts
#'
#' @description
#'
#' This function serves as a plotting mechanism for prepped forecast submission data. The plots show the historical trajectory of the truth data supplied along with the forecasted point estimates and (optionally) the prediction interval. All plots are faceted by location.
#'
#' Note that the ".data" and "submission" arguments to this function expect incoming data prepared in a certain format. See the argument documentation and "Details" for more information.
#'
#' @param .data A data frame with historical truth data for all locations and outcomes in submission targets
#' @param submission Formatted submission (e.g., a `tibble` containing forecasts prepped with [format_for_submission])
#' @param location  Vector specifying locations to filter to; `'US'` by default.
#' @param pi Width of prediction interval to plot; default is `0.95` for 95% PI; if set to `NULL` the PI will not be plotted
#' @param .model Name of the model used to generate forecasts; default is `NULL` and the name of the model will be assumed to be stored in a column called "model" in formatted submission file
#' @param .outcome The name of the outcome variable you're plotting in the historical data; defaults to `"flu.admits"`
#' @param format The submission format to be used; must be one of `"hubverse"` or `"legacy"`; default is `"legacy"`
#'
#' @details
#'
#' To plot the forecasted output alongside the observed historical data, both the ".data" and "submission" data must be prepared at the same geographic and temporal resolutions. The data frame passed to ".data" must include the column specified in the ".outcome" argument as well as the following columns:
#'
#' - **location**: FIPS location code
#' - **week_end**: Date of the last day (Saturday) in the given epidemiological week
#'
#' If format is "legacy" the "submission" data should be a probabilistic forecast prepared as a `tibble` with at minimum the following columns:
#'
#' - **forecast_date**: Date of forecast
#' - **target**: Horizon and name of forecasted target
#' - **target_end_date**: Last date of the forecasted target (e.g., Saturday of the given epidemiological week)
#' - **location**: FIPS code for location
#' - **type**: One of either "point" or "quantile" for the forecasted value
#' - **quantile**: The quantile for the forecasted value; `NA` if "type" is `"point"`
#' - **value**: The forecasted value
#'
#' If format is "hubverse" the "submission" data should be a probabilistic forecast prepared as a `tibble` with at minimum the following columns:
#'
#' - **reference_date**: Date of reference for forecast submission
#' - **horizon**: Horizon for the given forecast
#' - **target**: Name of forecasted target
#' - **target_end_date**: Last date of the forecasted target (e.g., Saturday of the given epidemiological week)
#' - **location**: Name or geographic identifier (e.g., FIPS code) for location for the given forecast
#' - **output_type**: Type of forecasted value (e.g., "quantile")
#' - **output_type_id**: The quantile for the forecasted value if output_type is "quantile"
#' - **value**: The forecasted value
#'
#' The "submission" data may optionally include a column with the name of the model used, such that multiple models can be visualized in the same plot.
#'
#' @return A `ggplot2` plot object with line plots for outcome trajectories faceted by location
#' @export
#'
#' @examples
#' \dontrun{
#' # Get some data
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#'
#' # Prep all the data
#' prepped_hosp_all <- prep_hdgov_hosp(h_raw)
#'
#' # What are the last four weeks of recorded data?
#' last4 <-
#'   prepped_hosp_all %>%
#'   dplyr::distinct(week_start) %>%
#'   dplyr::arrange(week_start) %>%
#'   tail(4)
#'
#' # Remove those
#' prepped_hosp <-
#'   prepped_hosp_all %>%
#'   dplyr::anti_join(last4, by="week_start")
#'
#' # Make a tsibble
#' prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
#'                                      epiyear = epiyear,
#'                                      epiweek=epiweek,
#'                                      key=location)
#' # Limit to just one state and US
#' prepped_hosp_tsibble <-
#'   prepped_hosp_tsibble %>%
#'   dplyr::filter(location %in% c("US", "51"))
#'
#' # Fit models and forecasts
#' hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
#'                                horizon=4L,
#'                                outcome="flu.admits",
#'                                trim_date=NULL,
#'                                covariates=TRUE)
#'
#' # Format for submission
#' hosp_formatted <- ts_format_for_submission(hosp_fitfor$tsfor)
#'
#' # Plot with current and all data
#' plot_forecast(prepped_hosp, hosp_formatted$ensemble)
#' plot_forecast(prepped_hosp_all, hosp_formatted$ensemble)
#' plot_forecast(prepped_hosp, hosp_formatted$ensemble, location=c("US", "51"))
#' plot_forecast(prepped_hosp_all, hosp_formatted$ensemble, location=c("US", "51"))
#' plot_forecast(prepped_hosp, hosp_formatted$ets)
#' plot_forecast(prepped_hosp_all, hosp_formatted$ets)
#' plot_forecast(prepped_hosp, hosp_formatted$arima)
#' plot_forecast(prepped_hosp_all, hosp_formatted$arima)
#'
#' # Demonstrating multiple models
#' prepped_hosp <-
#'   h_raw %>%
#'   prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
#'   dplyr::filter(abbreviation != "DC") %>%
#'   dplyr::filter(week_start < as.Date("2022-01-08", format = "%Y-%m-%d"))
#'
#' tsens_20220110 <-
#'   system.file("extdata/2022-01-10-SigSci-TSENS.csv", package="fiphde") %>%
#'   readr::read_csv(show_col_types = FALSE)
#' creg_20220110 <-
#'   system.file("extdata/2022-01-10-SigSci-CREG.csv", package="fiphde") %>%
#'   readr::read_csv(show_col_types = FALSE)
#' combo_20220110 <- dplyr::bind_rows(
#'   dplyr::mutate(tsens_20220110, model = "SigSci-TSENS"),
#'   dplyr::mutate(creg_20220110, model = "SigSci-CREG")
#' )
#' plot_forecast(prepped_hosp, combo_20220110, location = "24")
#' plot_forecast(prepped_hosp, tsens_20220110, location = "24")
#' plot_forecast(prepped_hosp, combo_20220110, location = c("34","36"))
#' plot_forecast(prepped_hosp, creg_20220110, location = "US", .model = "SigSci-CREG")
#' plot_forecast(prepped_hosp, creg_20220110, location = "US", .model = "SigSci-CREG")
#'
#' ## demonstrating different prediction interval widths
#' plot_forecast(prepped_hosp, combo_20220110, location = "24", pi = 0.5)
#' plot_forecast(prepped_hosp, combo_20220110, location = "24", pi = 0.9)
#' plot_forecast(prepped_hosp, combo_20220110, location = "24", pi = 0.95)
#' plot_forecast(prepped_hosp, combo_20220110, location = "24", pi = NULL)
#' }
plot_forecast <- function(.data, submission, location="US", pi = 0.95, .model = NULL, .outcome="flu.admits", format = "legacy") {

  if(!is.null(.model)) {
    submission$model <- .model
  }

  validpi <- sort(unique(round(abs(2*(q-0.5)),2)))[-1]
  if (!is.null(pi) && !(pi %in% validpi)) {
    stop(paste("pi must be NULL or one of:", paste(validpi, collapse=" ")))
  }

  if (!("model" %in% colnames(submission))) submission$model <- "Forecast"

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
    dplyr::select(location, date=week_end,point={{.outcome}}) %>%
    dplyr::mutate(model="Observed")

  if(format == "hubverse") {
    submission <-
      submission %>%
      dplyr::filter(output_type == "quantile") %>%
      dplyr::mutate(target = paste0(horizon, " ", target)) %>%
      dplyr::mutate(target = gsub("wk", "wk ahead", target)) %>%
      dplyr::select(model, forecast_date = reference_date, target, target_end_date, location, type = output_type, quantile = output_type_id, value)

    ## need to prep point estimates for hubverse format
    ## will use q0.5 as point estimates for plotting below
    point_estimates <-
      submission %>%
      dplyr::filter(quantile == 0.5) %>%
      dplyr::mutate(type = "point") %>%
      dplyr::mutate(quantile = NA)

    submission <-
      submission %>%
      dplyr::filter(quantile != 0.5) %>%
      dplyr::bind_rows(., point_estimates)
  }

  ## get appropriate boundaries based on specified width of PI
  ## default is 0.95 ...
  ## which would estrict to q0.025 (lower) and q0.975 (upper)
  if(!is.null(pi)) {
    lower_bound <-
      round(0.5 - (pi/2),3) %>%
      as.character(.) %>%
      stringr::str_pad(.,width = 5, pad = "0", side = "right")

    upper_bound <-
      round(0.5 + (pi/2),3) %>%
      as.character(.) %>%
      stringr::str_pad(.,width = 5, pad = "0", side = "right")

    tmp_forecasted <-
      submission %>%
      ## force all quantile representations to be 0.150 vs 0.15 (for example) formatting
      dplyr::mutate(quantile = stringr::str_pad(quantile,width = 5, pad = "0", side = "right")) %>%
      dplyr::group_by(model) %>%
      dplyr::filter(type=="point" | quantile == lower_bound | quantile == upper_bound)

    # return(tmp_forecasted)
    # Grab the forecasted data
    forecasted <-
      tmp_forecasted %>%
      dplyr::filter(location %in% loc) %>%
      dplyr::mutate(quantile=tidyr::replace_na(quantile, "point")) %>%
      dplyr::select(-type) %>%
      tidyr::separate(target, into=c("nwk", "target"), sep=" wk ahead ") %>%
      dplyr::select(location, date=target_end_date,quantile, value, model) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      dplyr::mutate(quantile = ifelse(quantile == lower_bound, "lower",
                                      ifelse(quantile == upper_bound, "upper",
                                             quantile))) %>%
      tidyr::spread(quantile, value)

    ## get number of models to control alpha for PI below
    n_models <- dplyr::n_groups(tmp_forecasted)
  } else {

    # Grab the forecasted data
    forecasted <-
      submission %>%
      dplyr::group_by(model) %>%
      dplyr::filter(type == "point") %>%
      dplyr::filter(location %in% loc) %>%
      dplyr::mutate(quantile=quantile %>% as.character() %>% tidyr::replace_na("point")) %>%
      dplyr::select(-type) %>%
      tidyr::separate(target, into=c("nwk", "target"), sep=" wk ahead ") %>%
      dplyr::select(location, date=target_end_date,quantile, value, model) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      tidyr::spread(quantile, value)
  }

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
    ggplot2::geom_point(ggplot2::aes(col=model)) +
    ggplot2::geom_line(ggplot2::aes(col=model)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    ggplot2::facet_wrap(~location, scales="free", ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Date", y = NULL) +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())

  if(!is.null(pi)) {

    ## NOTE: alpha is controlled by the number of models plotted
    ## if only 1 model is plotted set to 0.5 so the ribbon isnt solid
    pi_alpha <- ifelse(n_models > 1, 1/n_models, 0.5)
    p <-
      p +
      ggplot2::geom_ribbon(ggplot2::aes(fill = model, ymin = lower, ymax = upper),
                           alpha = pi_alpha)
  }

  return(p)
}

#' @title Plot categorical forecasts
#' @description This function creates a bar plot for categorical forecasts. See examples for demonstration of usage.
#' @param categorical_forecast Either a `tibble` with categorical forecasts created with [forecast_categorical] or prepared forecast submission in "hubverse" format (see Details)
#' @param format Either "hubverse" or "legacy"; the "hubverse" format will require an input forecast that includes output for "pmf" (see Details); default is "hubverse"
#' @return A `ggplot2` object with categorical forecasts shown as a stacked bar plot.
#'
#' @details
#' The categorical plotting function works both with "legacy" formatting (i.e., format used in the 2022-23 FluSight season) and the "hubverse" formatting (i.e., format used in the 2023-24 FluSight season). Unlike the "legacy" format, the "hubverse" format allows for quantile and categorical forecasts to be co-mingled in the same submission object. If the format is specified as "hubverse", then the `plot_forecast_categorical()` function will interally look for the "pmf" forecasts.
#'
#' @export
#' @examples
#' \dontrun{
#' # Retrieve hospitalization data
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#' # Prepare and summarize hospitalization data to weekly resolution
#' prepped_hosp <- prep_hdgov_hosp(h_raw)
#' # Create a keyed time series tibble with only locations of interest
#' prepped_tsibble <- make_tsibble(prepped_hosp,
#'                                      epiyear = epiyear,
#'                                      epiweek=epiweek,
#'                                      key=location)
#' # Run with default constrained ARIMA, nonseasonal ETS, no NNETAR
#' hosp_fitfor <- ts_fit_forecast(prepped_tsibble,
#'                                horizon=4L,
#'                                outcome="flu.admits")
#' # Prepare forecast for quantile submission format
#' prepped_forecast <- format_for_submission(hosp_fitfor$tsfor, method = "ts", format = "hubverse")
#' # Run categorical summary of quantiles for the time series ensemble
#' categorical_forecast <- forecast_categorical(prepped_forecast$ensemble, prepped_hosp, format = "hubverse")
#' # Plot the categorical forecast
#' plot_forecast_categorical(categorical_forecast, format = "hubverse")
#' }
plot_forecast_categorical <- function(categorical_forecast, format = "hubverse") {
  if(format == "legacy") {
    categorical_forecast %>%
      dplyr::inner_join(locations, by="location") %>%
      dplyr::select(loc=abbreviation, type_id, value) %>%
      tidyr::spread(type_id, value) %>%
      dplyr::mutate(score=(2*large_increase + increase + (-1)*decrease + (-2)*large_decrease)) %>%
      dplyr::mutate(loc=factor(loc) %>% stats::reorder(score)) %>%
      dplyr::mutate(loc=stats::relevel(loc, ref="US")) %>%
      dplyr::select(-score) %>%
      tidyr::pivot_longer(-loc, names_to="type_id", values_to="value") %>%
      dplyr::mutate(type_id=factor(type_id,
                                   levels=c("large_decrease", "decrease", "stable", "increase", "large_increase"),
                                   labels=c("Large decrease", "Decrease", "Stable", "Increase", "Large increase"))) %>%
      ggplot2::ggplot(ggplot2::aes(loc, value)) + ggplot2::geom_col(ggplot2::aes(fill=type_id)) +
      ggplot2::scale_fill_manual(values=c("darkorchid", "cornflowerblue", "gray80", "orange", "red2")) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position="bottom", legend.title=ggplot2::element_blank()) +
      ggplot2::labs(x=NULL, y=NULL)
  } else if (format == "hubverse") {
    categorical_forecast %>%
      dplyr::filter(output_type == "pmf") %>%
      dplyr::inner_join(locations, by="location") %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      dplyr::select(loc=abbreviation, output_type_id, value, horizon) %>%
      dplyr::mutate(output_type_id=factor(output_type_id,
                                          levels=c("large_decrease", "decrease", "stable", "increase", "large_increase"),
                                          labels=c("Large decrease", "Decrease", "Stable", "Increase", "Large increase"))) %>%
      ggplot2::ggplot(ggplot2::aes(loc, value)) + ggplot2::geom_col(ggplot2::aes(fill=output_type_id)) +
      ggplot2::scale_fill_manual(values=c("darkorchid", "cornflowerblue", "gray80", "orange", "red2")) +
      ggplot2::facet_wrap(~horizon, ncol = 1) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position="bottom", legend.title=ggplot2::element_blank()) +
      ggplot2::labs(x=NULL, y=NULL)
  }
}

#' @title Minimum non-zero
#' @description Helper function to get the minimum non-zero positive value from a vector. Used internally in [mnz_replace].
#' @param x A numeric vector
#' @return The minimum non-zero positive value from `x`
#' @export
#' @examples
#' x <- c(.1, 0, -.2, NA, .3, .4, .0001, -.3, NA, 999)
#' x
#' mnz(x)
mnz <- function(x) {
  if (!is.numeric(x)) stop("x must be a numeric vector")
  return(min(x[which(x>0)]))
}

#' @title Minimum non-zero replacement
#' @description Replace zeros and negative values with the minimum non-zero positive value from a vector.
#' @param x A numeric vector
#' @return A vector of the same length with negatives and zeros replaced with the minimum nonzero value of that vector.
#' @examples
#' x <- c(.1, 0, -.2, NA, .3, .4, .0001, -.3, NA, 999)
#' x
#' mnz(x)
#' mnz_replace(x)
#' tibble::tibble(x) %>% dplyr::mutate(x2=mnz_replace(x))
#' @export
mnz_replace <- function(x) {
  x[which(x<=0)] <- mnz(x)
  return(x)
}

#' @title Convert MMWR format to date
#'
#' @description
#'
#' Adapted from `cdcfluview::mmwr_week_to_date`.
#'
#' This function transforms MMWR epidemiological year+week (or year+week+day) to a date object. This was implemented based on the `cdcfluview::mmwr_week_to_date` function, which adapted similar functionality from the `MMWRweek` package.
#'
#' @param year Vector of epidemiological year(s); must be same length as "week" and "day" (unless "day" is `NULL`)
#' @param week Vector of epidemiological week(s); must be same length as "year" and "day" (unless "day" is `NULL`)
#' @param day Vector of day(s); must be same length as "week" and "year" (unless set to is `NULL`); default is `NULL` and the day returned will be the first day of the epidemiological week (i.e., Sunday)
#' @return Vector of date objects as with as many elements as input year(s), week(s), day(s)
#' @references
#' - [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)
#' @export
#' @examples
#' mmwr_week_to_date(2020,1)
#' mmwr_week_to_date(2020,1,5)
#' mmwr_week_to_date(c(2020,2021,2022),c(1,2,8), c(1,1,7))
mmwr_week_to_date <- function(year, week, day=NULL) {

  year <- as.numeric(year)
  week <- as.numeric(week)
  day <- if (!is.null(day)) as.numeric(day) else rep(1, length(week))

  week <- ifelse(0 < week & week < 54, week, NA)

  as.Date(ifelse(is.na(week), NA, MMWRweek::MMWRweek2Date(year, week, day)),
          origin="1970-01-01")

}

#' @title Make clean column names
#'
#' @description
#'
#' This helper is used in [ilinet] and [who_nrevss] functions to clean column names of values returned from the APIs.
#'
#' @param tbl Input `tibble` with columns to rename
#'
#' @return A `tibble` with clean column names
#'
#'
.mcga <- function(tbl) {
  x <- colnames(tbl)
  x <- tolower(x)
  x <- gsub("[[:punct:][:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^_|_$)", "", x)
  x <- gsub("^x_", "", x)
  x <- make.unique(x, sep = "_")
  colnames(tbl) <- x
  tbl
}

#' @title Clean numeric values
#'
#' @description
#'
#' This unexported helper is used in the [ilinet] function to strip special characters and empty space and convert a character vector to numeric.
#'
#' @param x Input character vector for which special characters should be stripped and converted
#'
#' @return Numeric vector
#'
to_num <- function(x) {
  x <- gsub("%", "", x, fixed=TRUE)
  x <- gsub(">", "", x, fixed=TRUE)
  x <- gsub("<", "", x, fixed=TRUE)
  x <- gsub(",", "", x, fixed=TRUE)
  x <- gsub(" ", "", x, fixed=TRUE)
  suppressWarnings(as.numeric(x))
}

#' @title Calculate smoothed and weighted averages of previous observations
#'
#' @description
#'
#' This helper function calculates a weighted average of the last n observations.
#'
#' @param x Incoming vector of observations
#' @param n Number of recent observations to smooth; default is `4`
#' @param weights Vector of weights to be applied to last n observations during averaging
#'
#' @return Vector of length 1 with the weighted average of last n observations.
#' @export
#'
#' @examples
#' \dontrun{
#' ## pull and prep weekly US flu hospitalization data
#' hosp_us <-
#'   get_hdgov_hosp() %>%
#'   prep_hdgov_hosp() %>%
#'   dplyr::filter(location == "US")
#'
#' ## what do the last 4 observations look like?
#' tail(hosp_us$flu.admits, 4)
#'
#' ## smooth over last 4 with default weights
#' smoothie(hosp_us$flu.admits, n=4, weights=c(1,2,3,4))
#'
#' ## try smoothing over last 4 with different weights (exponential this time)
#' smoothie(hosp_us$flu.admits, n=4, weights=exp(1:4))
#' }
smoothie <- function(x, n = 4, weights = c(1,2,3,4)) {

  ## check that n = length(weights)
  stopifnot(n == length(weights))

  ## get the last 4 items of vector
  lastn <- utils::tail(x,n)

  ## get "weights" for each item
  ## the weights correspond to number of times each element is repeated
  tmp <- dplyr::tibble(last_n=lastn, reps=weights)

  ## repeat the elements as many times as weighted
  ## and average
  purrr::map2(tmp$last_n, tmp$reps, .f = function(x,y) rep(x,y)) %>%
    unlist(.) %>%
    mean(.)

}

#' Round and preserve vector
#'
#' This unexported helper is used to ensure that categorical forecasts are rounded to sum to 1.
#'
#' @param x Numeric vector with values to round
#' @param digits The number of digits to use in precision; defalut is `0`
#'
#' @return Vector of same length as "x" with values rounded
#'
round_preserve <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

#' @title Non-seasonal flu hospitalization imputation
#'
#' @description
#'
#' This unexported helper function is used to create a "non-seasonal", location-specific imputation estimate for weekly NHSN flu hospitalization counts. The imputation approach was motivated by the change in reporting requirements for the NHSN hospital respiratory disease metrics, which became optional from April 2024 to November 2024. This function includes four different approaches (see 'Details' for more) for adjusting and/or filling the gap in state-level flu hospitalization reporting.
#'
#' @param dat A `tibble` with hospitalization data prepared either by [prep_hdgov_hosp] or [prep_nhsn_weekly]
#' @param location FIPS code for location to impute
#' @param method Imputation method to use; must be one of `"val"`, `"diff"`, `"median"`, or `"partial"` (see 'Details' for more); default is `"val"`
#' @param begin_date Start date for imputation in YYYY-MM-DD format; default is `"2024-04-28"`
#' @param end_date End date for imputation in YYYY-MM-DD; default is `"2024-11-02"`
#'
#' @details
#' There are four possible methods for imputing non-seasonal weeks implemented in this function:
#'
#' - "val": Random sampling from a vector of values including all flu hospitalizations reported weeks between June-October 2022 and June-October 2023 for the given location; first and last values are defined as median of the random sample and the most recent un-imputed value (i.e., the week before imputation begins and the week after imputation ends)
#' - "diff": Random sampling from a vector of week-to-week differences in flu hospitalizations reported in weeks between June-October 2022 and June-October 2023 for the given location
#' - "median": Median of 2022 and 2023 values reported for the given epiweek
#' - "partial": Uses the `adjust_partial=TRUE` flag for the [prep_nhsn_weekly] function to fill the weeks in the date range specified
#'
#' @return A `tibble` with the same structure as the input for the "dat" argument, but with weeks between "begin_date" and "end_date" imputed.
#'
#' @export
#'
#'
ns_impute <- function(dat, location, method = "val", begin_date = "2024-04-28", end_date = "2024-11-02") {

  ## enforce date-ness for date math below
  begin_date <- as.Date(begin_date)
  end_date <- as.Date(end_date)

  ## correct for the beginning date being the *week start* (i.e., sunday)
  ## all of the dates will be oriented towards *week end* (i.e., saturday)
  true_begin <- begin_date + 6

  ## get a sequence of the beginning and end weeks for imputation
  imputed_weeks <- seq(true_begin, end_date, by = 7)

  ## restrict data to the location of interest
  tmp_dat <-
    dat %>%
    dplyr::filter(.data$location == .env$location) %>%
    dplyr::filter(!dplyr::between(.data$week_end, true_begin, end_date))

  if(method %in% c("diff","val")) {
    ## pull non-seasonal values for 2022/2023
    ## NOTE: for now this is hardcoded as anything between start of June and start of October)
    nonseasonal_vals <-
      tmp_dat %>%
      dplyr::filter(dplyr::between(.data$week_start, as.Date("2022-06-01"), as.Date("2022-10-01")) | dplyr::between(.data$week_start, as.Date("2023-06-01"), as.Date("2023-10-01"))) %>%
      dplyr::pull("flu.admits")

    ## use observed non-seasonal values to find observed point-to-point differences
    nonseasonal_diffs <- diff(nonseasonal_vals)

    ## get the last value to use with the imputed differences and smoothing below
    last_left_val <-
      tmp_dat %>%
      dplyr::filter(.data$week_end == as.Date(begin_date - 1)) %>%
      dplyr::pull(flu.admits)

    ## get the first value of restarted reporting to use with smoothing below
    first_right_val <-
      tmp_dat %>%
      dplyr::filter(.data$week_end == as.Date(end_date + 7)) %>%
      dplyr::pull(flu.admits)

    if(method == "diff") {
      ## randomly sample from non-seasonal differences for the length of the sequence of weeks to impute
      imputed_diffs <- sample(nonseasonal_diffs, length(imputed_weeks), replace = TRUE)

      ## get the cumulative sum of the differences added to the last value
      ## the -1 index removes the last value so we dont repeat that in the time series when we stitch together
      imputed_ts <- cumsum(c(last_left_val,imputed_diffs))[-1]
    } else if (method == "val") {
      imputed_ts <- sample(nonseasonal_vals, length(imputed_weeks), replace = TRUE)

      ## smooth left edge by taking mean of non seasonal impute and the "last left value" ...
      ## ... i.e., the last reported week before non xseasonal impute begins
      imputed_ts[1] <- stats::median(c(imputed_ts[1],last_left_val))
      ## smooth right edge by taking mean of non seasonal impute and the "last right value" ...
      ## ... i.e., the first reported week after non seasonal impute ends
      imputed_ts[length(imputed_ts)] <- stats::median(c(imputed_ts[length(imputed_ts)],first_right_val))
    }
  } else if (method == "median") {

    ## get the epiweeks for the week end dates to impute
    ews <- lubridate::epiweek(imputed_weeks)

    ## use a median imputation approach for equivalent weeks in 2022 and 2023
    imputed_ts <-
      tmp_dat %>%
      ## make sure we dont use data prior to NHSN flu hosps being required fields
      dplyr::filter(week_end >= as.Date("2022-04-01")) %>%
      dplyr::filter(epiweek %in% ews) %>%
      dplyr::group_by(epiweek) %>%
      ## just take the median at each epiweek
      dplyr::summarise(flu.admits = stats::median(flu.admits, na.rm = TRUE)) %>%
      dplyr::pull(flu.admits)
  } else if (method == "partial") {

    ## get partially reported data from NHSN weekly aggregates
    ## prep to format as imputed ts
    partial_dat <-
      get_nhsn_weekly() %>%
      prep_nhsn_weekly(adjust_partial = TRUE) %>%
      ## the data has other weeks in it so we need to filter for the date range of interest
      dplyr::filter(dplyr::between(week_end, begin_date,end_date)) %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::select(abbreviation, flu.admits, week_end)

    imputed_ts <-
      partial_dat %>%
      dplyr::pull(flu.admits)

  }


  ## truncate at zero so there are no negative counts
  imputed_ts <- ifelse(imputed_ts < 0, 0, imputed_ts)

  res <-
    dplyr::tibble(
      location = .env$location,
      abbreviation = unique(tmp_dat$abbreviation),
      flu.admits = imputed_ts,
      week_end = imputed_weeks
    ) %>%
    dplyr::mutate(week_start = week_end - 6,
                  epiyear = lubridate::epiyear(week_end),
                  epiweek = lubridate::epiweek(week_end),
                  monday = week_start + 1) %>%
    dplyr::bind_rows(., tmp_dat) %>%
    dplyr::arrange(week_end)

  return(res)
}

