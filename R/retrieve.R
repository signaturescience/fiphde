#' @title Get hospitalization data
#' @name Get flu and COVID-19 hospitalization data from healthdata.gov endpoint
#' @description Retrieves hospitalization data from the healthdata.gov endpoint with optional filtering on fields, and return the results into a nice tibble.
#' @param endpoint URL to healthdata.gov endpoint (see references)..
#' @param state A two-letter state abbreviation.
#' @param limitrows Limit API query to at most this number of results. Default is `NULL` (no limit).
#' @param mindate Minimum date of results returned (ISO 8601 format: YYYY-MM-DD). See examples.
#' @param maxdate Maximum date of results returned (ISO 8601 format: YYYY-MM-DD). See examples.
#' @param limitcols Limit the columns returned to the subjectively defined important ones?
#' @param app_token App token from healthdata.gov. If `NULL` you might get rate limited. Add an entry to your `~/.Renviron` with `HEALTHDATA_APP_TOKEN="tokenhere"` that you got from <https://healthdata.gov/profile/edit/developer_settings>.
#' @return A tibble
#' @references API documentation: <http://dev.socrata.com/foundry/healthdata.gov/g62h-syeh>.
#' @examples
#' \dontrun{
#' get_hdgov_flu_hosp(mindate="2021-11-01", limitrows=10)
#' get_hdgov_flu_hosp(state="VA")
#' get_hdgov_flu_hosp(state="VA", mindate="2021-10-01")
#' get_hdgov_flu_hosp(state="VA", mindate="2021-10-01", maxdate="2021-10-31")
#' get_hdgov_flu_hosp(state="VA", mindate="2021-10-01", maxdate="2021-11-21", limitrows=5)
#' get_hdgov_flu_hosp(state="VA", mindate="2021-10-01", limitrows=5, limitcols=FALSE)
#' }
#' @export
get_hdgov_hosp <- function(endpoint="https://healthdata.gov/resource/g62h-syeh.json",
                               state=NULL, limitrows=NULL, mindate=NULL, maxdate=NULL,
                               limitcols=TRUE, app_token=Sys.getenv("HEALTHDATA_APP_TOKEN")) {

  # If limiting to a state, construct the state limit query string
  state <- if (!is.null(state) && is.character(state)) state <- paste0("state=", state)

  # Construct the "where" filter for dates depending on what's defined in the call
  datequery <-
    dplyr::case_when(
      # both min and max are defined, use between
      !is.null(mindate) & !is.null(maxdate) ~ paste0("$where=date between '", mindate, "T00:00:00.000' and '", maxdate, "T00:00:00.000'"),
      # Only min is defined, use >=
      !is.null(mindate) &  is.null(maxdate) ~ paste0("$where=date>='", mindate, "T00:00:00.000'"),
      # Only max is defined, use <=
      is.null(mindate)  & !is.null(maxdate) ~ paste0("$where=date<='", maxdate, "T00:00:00.000'"),
      # Neither defined, use NA_character (can't return NULL)
      is.null(mindate)  &  is.null(maxdate) ~ NA_character_
    )
  # Replace NA with NULL
  if (is.na(datequery)) datequery <- NULL

  # Limit the rows of the output?
  limitrows <- if (!is.null(limitrows) && is.numeric(limitrows)) paste0("$limit=", round(limitrows))

  # Construct the API url filter string
  filterstring <- paste(state, datequery, limitrows, sep="&")
  # Multiple NULLs would result in multiple &s in the string. Turn >=2 &s to a single &
  filterstring <- gsub("&+", "&", filterstring)
  # Remove trailing &
  filterstring <- gsub("&+$", "", filterstring)
  # Remove leading &
  filterstring <- gsub("^&+", "", filterstring)

  # Construct API URL
  api_url <- paste(endpoint, filterstring, sep="?")

  # Get the data and make it a tibble, and fix the date
  d <- suppressWarnings(RSocrata::read.socrata(api_url, app_token)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(date=lubridate::as_date(date)) %>%
    dplyr::arrange(date)

  # Just return the columns you care about, as numeric
  if (limitcols) {
    d <- d %>%
      dplyr::select(state,
                    date,
                    flu.admits     = previous_day_admission_influenza_confirmed,
                    flu.admits.cov = previous_day_admission_influenza_confirmed_coverage,
                    flu.deaths     = previous_day_deaths_influenza,
                    flu.deaths.cov = previous_day_deaths_influenza_coverage,
                    flu.icu        = icu_patients_confirmed_influenza,
                    flu.icu.cov    = icu_patients_confirmed_influenza_coverage,
                    flu.tot        = total_patients_hospitalized_confirmed_influenza,
                    flu.tot.cov    = total_patients_hospitalized_confirmed_influenza_coverage,
                    cov.admits     = previous_day_admission_adult_covid_confirmed,
                    cov.admits.cov = previous_day_admission_adult_covid_confirmed_coverage,
                    cov.deaths     = deaths_covid,
                    cov.deaths.cov = deaths_covid_coverage
      ) %>%
      dplyr::mutate(dplyr::across(c(-state, -date), as.integer))
  }

  message(paste0(nrow(d), " rows retrieved from:\n", api_url))
  return(d)

}



#' @title Get vaccination data
#' @description Get vaccination data from cdc.gov endpoint.
#' @param endpoint URL to cdc.gov endpoint. See references.
#' @param limitrows Limit API query to at most this number of results. Default is `NULL` (no limit).
#' @param season Limit to a season. Format: 20xx-20yy. E.g., season="2017-2018" or season="2020-2021"
#' @param app_token App token from healthdata.gov. If it's `NULL` you might get rate limited. Add an entry to your `~/.Renviron` with `HEALTHDATA_APP_TOKEN="tokenhere"` that you got from <https://healthdata.gov/profile/edit/developer_settings>.
#' @return A tibble
#' @references API documentation: <https://dev.socrata.com/foundry/data.cdc.gov/k87d-gv3u>.
#' @examples
#' \dontrun{
#' d <- get_cdc_vax_data()
#' d
#' library(ggplot2)
#' d %>%
#'   ggplot(aes(date, cumulative_flu_doses)) +
#'   geom_line() +
#'   facet_wrap(~season, scale="free_x") +
#'   theme_bw()
#' rm(d)
#' }
#' @export
get_cdc_vax <- function(endpoint="https://data.cdc.gov/resource/k87d-gv3u.json",
                             season=NULL, limitrows=NULL, app_token=Sys.getenv("HEALTHDATA_APP_TOKEN")) {

  # If limiting to a state, construct the state limit query string
  season <- if (!is.null(season) && is.character(season)) season <- paste0("season=", season)

  # Limit the rows of the output?
  limitrows <- if (!is.null(limitrows) && is.numeric(limitrows)) paste0("$limit=", round(limitrows))

  # Construct the API url filter string
  filterstring <- paste(season, limitrows, sep="&")
  # Multiple NULLs would result in multiple &s in the string. Turn >=2 &s to a single &
  filterstring <- gsub("&+", "&", filterstring)
  # Remove trailing &
  filterstring <- gsub("&+$", "", filterstring)
  # Remove leading &
  filterstring <- gsub("^&+", "", filterstring)

  # Construct API URL
  api_url <- paste(endpoint, filterstring, sep="?")

  # # Get the data and make it a tibble, and fix the date
  d <- suppressWarnings(RSocrata::read.socrata(api_url, app_token)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(current_through=lubridate::as_date(current_through)) %>%
    dplyr::mutate(week=as.numeric(week)) %>%
    dplyr::mutate(cumulative_flu_doses=as.numeric(cumulative_flu_doses)) %>%
    # Separate the season (20xx-20yy) into two different years
    tidyr::separate(season, into=c("y1", "y2"), sep="-", remove=FALSE) %>%
    # If it's July-December, it's 20xx, else it's 20yy
    dplyr::mutate(year=ifelse(month %in% c("July", "August", "September", "October", "November", "December"), y1, y2), .before=1L) %>%
    dplyr::select(-y1, -y2) %>%
    # Figure out January, February, ..., November, December is month 1, 2, ..., 11, 12
    dplyr::inner_join(tibble::tibble(month=month.name, month_calendar_number=1:12), by="month") %>%
    # Figure out the date. Start with the year and the month number ie 20XX-MM, then start with the first day of that month, 01,
    # then add 7 days for each week elapsed in that month.
    dplyr::mutate(date = lubridate::as_date(paste(year, month_calendar_number, "01", sep="-"))  + lubridate::weeks(week-1), .before=1L) %>%
    # Get the epiweek and year
    dplyr::mutate(epiyear=lubridate::epiyear(date), epiweek=lubridate::epiweek(date)) %>%
    # Reorder columns keeping only the useful ones
    dplyr::select(season, year, month, month_number, month_calendar_number, week, epiyear, epiweek, date, cumulative_flu_doses, imputed_value)

  message(paste0(nrow(d), " rows retrieved from:\n", api_url))
  return(d)

}

