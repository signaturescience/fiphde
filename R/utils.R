#' Get Monday
#'
#' @description
#'
#' This function is a helper to get the date for the Monday of the current week. The function determines the current week based on epidemiogical week orientation (i.e., week begins with Sunday).
#'
#' @return Date for the Monday of the current week.
#' @export
#' @examples
#' this_monday()
this_monday <- function() {
  tmp <- MMWRweek::MMWRweek(lubridate::today())
  MMWRweek::MMWRweek2Date(tmp$MMWRyear, tmp$MMWRweek, 2)
}
#' Check Monday
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

#' @title Replace ILINet data with nowcast entirely for a state
#'
#' @description
#'
#' This function replaces the weighted ILI retrieved from [get_cdc_ili] with nowcast data for each of the locations in the original data. The function will first attempt to use ILI Nearby nowcasts pulled using [get_nowcast_ili]. If the ILI Nearby nowcasts are unavailable, the function will optionally fallback to a pseudo nowcast method that averages the observed ILI for the 4 most recent weeks. Unlike [replace_ili_nowcast], the ILI will be replaced with nowcast for *all* dates for a specified location. This is useful for getting data for states where most or all ILI data is missing (e.g., Florida).
#'
#' Note that this only replaces weighted ILI in the specified state where the value is `NA`. _Most_ ILI data from Florida is missing, but not all.
#'
#' @param ilidat ILI data retrieved via [get_cdc_ili]
#' @param state Two-letter state abbreviation to replace completely
#' @param impute Logical as to whether or not to try to mean impute missing values using the immediately preceding and following values; default is `TRUE`
#' @param fallback Logical as to whether or not to fall back to pseudo nowcast (average of last 4 ILI weeks in the given location) if nowcast data is unavailable; default is `TRUE`
#' @param ... Other arguments passed to [get_nowcast_ili] (e.g., `boundatzero`, which is `TRUE` by default)
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
#' @examples
#' \dontrun{
#'
#' ilidat <- get_cdc_ili(years=2020)
#'
#' ilidat <-
#'   ilidat %>%
#'   dplyr::filter(location=="US" | abbreviation=="VA" | abbreviation=="FL") %>%
#'   dplyr::group_by(location) %>%
#'   dplyr::slice_max(week_start, n=4) %>%
#'   dplyr::select(location:weighted_ili) %>%
#'   dplyr::arrange(location, epiyear, epiweek)
#' ilidat
#' state_replace_ili_nowcast_all(ilidat, state="FL")
#' # Example with Florida, which has a negative value for nowcasted ILI
#' ilidat <- get_cdc_ili(years=2019)
#' ilidat <- ilidat %>%
#'   dplyr::filter(location=="US" | abbreviation=="VA" | abbreviation=="FL") %>%
#'   dplyr::filter(epiyear==2020 & epiweek %in% c(20, 21, 22)) %>%
#'   dplyr::select(location:weighted_ili) %>%
#'   dplyr::arrange(location, epiyear, epiweek)
#' ilidat
#' # defaults to bound at zero
#' state_replace_ili_nowcast_all(ilidat, state="FL")
#' # show results when you don't bound at zero
#' state_replace_ili_nowcast_all(ilidat, state="FL", boundatzero=FALSE)
#' # example with missing data in florida
#' ilidat <- get_cdc_ili(region=c("national","state"), years=2019:lubridate::year(lubridate::today()))
#' ilidat <- ilidat %>%
#'   dplyr::filter(abbreviation=="FL") %>%
#'   dplyr::filter(week_start>="2020-12-13" & week_start<="2021-01-10")
#' ilidat
#' state_replace_ili_nowcast_all(ilidat, state="FL")
#' state_replace_ili_nowcast_all(ilidat, state="FL", impute=FALSE)
#' }
#' @export
state_replace_ili_nowcast_all <- function(ilidat, state, impute=TRUE, fallback=TRUE, ...) {
  dates <- sort(unique(ilidat$week_start))
  ilinow <- get_nowcast_ili(dates=dates, state=state, ...)

  ## handle case when delphi ili nowcast api doesnt return all of the nowcast data
  if(is.na(ilinow)) {
    if(fallback) {

      message("There was an issue retrieving the ILI nowcast data from the API. The fallback option is set to 'TRUE'. Using 4 most recent weeks of available data to generate pseudo nowcast for each location.")
      ilinow <-
        ilidat %>%
        dplyr::filter(abbreviation == state) %>%
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
        tidyr::crossing(dates, .) %>%
        ## get epiyear and epiweek
        dplyr::mutate(epiweek = lubridate::epiweek(dates),
                      epiyear = lubridate::epiyear(dates)) %>%
        dplyr::select(-dates)
    } else {
      stop("There was an issue retrieving the ILI nowcast data from the API. The fallback option is set to 'FALSE'. Cannot proceed.")
    }
  }

  res <- ilidat %>%
    dplyr::left_join(ilinow, by = c("location", "abbreviation", "epiyear", "epiweek")) %>%
    dplyr::mutate(weighted_ili=ifelse(is.na(weighted_ili) & abbreviation==state, weighted_ili_now, weighted_ili)) %>%
    dplyr::select(-weighted_ili_now)
  # Quick fix for FL 2020:53: mean impute a missing value using the immediately preceding and following values
  if (impute) {
    res <- res %>%
      dplyr::mutate(weighted_ili=ifelse(is.na(weighted_ili),
                                        yes = (dplyr::lead(weighted_ili) + dplyr::lag(weighted_ili))/2,
                                        no  = weighted_ili))
  }
  return(res)
}

#' @title Replace ILINet data with nowcast
#'
#' @description
#'
#' This function replaces the weighted ILI retrieved from [get_cdc_ili] with nowcast data for each of the locations in the original data. The function will first attempt to use ILI Nearby nowcasts pulled using [get_nowcast_ili]. If the ILI Nearby nowcasts are unavailable, the function will optionally fallback to a pseudo nowcast method that averages the observed ILI for the 4 most recent weeks. The nowcast data will be used to add 1 additional week to the observed ILI data and (optionally) replace the number of weeks specified in the "weeks_to_replace" argument.
#'
#' @param ilidat ILI data retrieved via [get_cdc_ili]
#' @param start_date Date from which to start nowcasting; default is [lubridate::today]
#' @param weeks_to_replace Number of weeks of `ilidat` to replace; default is `2`
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
replace_ili_nowcast <- function(ilidat, start_date = NULL, weeks_to_replace=1, fallback=TRUE) {
  if (is.null(start_date)) start_date <- lubridate::today()
  # How many days back do you need to go? 1 to weeks+1, *7
  days_back <- (1:(weeks_to_replace+1))*7
  # What are those dates?
  dates_back <- start_date - days_back
  ilinow <- get_nowcast_ili(dates=dates_back)
  ## handle case when delphi ili nowcast api doesnt return all of the nowcast data
  if(is.na(ilinow)) {
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

#' Plot forecasts
#'
#'
#' @description
#'
#' This function serves as a plotting mechanism for prepped forecast submission data. The plots how the historical trajectory of the truth data supplied along with the forecasted point estimates and (optionally) the prediction interval. All plots are faceted by location.
#'
#' Note that the ".data" and "submission" arguments to this function expect incoming data prepared in a certain format. See the argument documentation and "Details" for more information requirements for these parameters.
#'
#' @param .data A data frame with historical truth data for all locations and outcomes in submission targets
#' @param submission Formatted submission (e.g., a `tibble` containing forecasts prepped with [format_for_submission])
#' @param location  Vector specifying locations to filter to; `'US'` by default.
#' @param pi Width of prediction interval to plot; default is `0.95` for 95% PI; if set to `NULL` the PI will not be plotted
#' @param .model Name of the model used to generate forecasts; default is `NULL` and the name of the model will be assumed to be stored in a column called "model" in formatted submission file
#' @param .outcome The name of the outcome variable you're plotting in the historical data; defaults to `"flu.admits"`
#'
#' @details
#'
#' To plot the forecasted output alongside the observed historical data, both the ".data" and "submission" data must be prepared at the same geographic and temporal resolutions. The data frame passed to ".data" must include the column specified in the ".outcome" argument as well as the following columns:
#'
#' - **location**: FIPS location code
#' - **week_end**: Date of the last day (Saturday) in the given epidemiological week
#'
#' The "submission" data should be a probabilistic forecast prepared as a `tibble` with at minimum following columns:
#'
#' - **forecast_date**: Date of forecast
#' - **target**: Horizon and name of forecasted target
#' - **target_end_date**: Last date of the forecasted target (e.g., Saturday of the given epidemiological week)
#' - **location**: FIPS code for location
#' - **type**: One of either "point" or "quantile" for the forecasted value
#' - **quantile**: The quantile for the forecasted value; `NA` if "type" is `"point"`
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
#' #remove those
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
#'                                covariates=c("hosp_rank", "ili_rank"))
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
#'   dplyr::filter(abbreviation != "DC")
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
plot_forecast <- function(.data, submission, location="US", pi = 0.95, .model = NULL, .outcome="flu.admits") {

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

#' Convert MMWR format to date
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

#' Make clean column names
#'
#' This helper is used in [ilinet] and [who_nrevss] functions to clean column names of values returned from the APIs.
#'
#' @param tbl Input tibble with columns to rename
#'
#' @return Tibble with clean column names
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

#' Clean numeric values
#'
#' This unexported helper is used in the [ilinet] function to strip special characters and empty space and convert a character vector to numeric.
#'
#' @param x Input character vector for which special characters should be stripped and converted
#'
#' @return Numeric vector
#'
#' @examples
#' x <- c(",1", 4, "11", "19  ", "25>")
#' fiphde:::to_num(x)
to_num <- function(x) {
  x <- gsub("%", "", x, fixed=TRUE)
  x <- gsub(">", "", x, fixed=TRUE)
  x <- gsub("<", "", x, fixed=TRUE)
  x <- gsub(",", "", x, fixed=TRUE)
  x <- gsub(" ", "", x, fixed=TRUE)
  suppressWarnings(as.numeric(x))
}

#' Calculate smoothed and weighted averages of previous observations
#'
#' @description This helper function calculates a weighted average of the last n observations.
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
#' get_hdgov_hosp() %>%
#' prep_hdgov_hosp() %>%
#' dplyr::filter(location == "US")
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

