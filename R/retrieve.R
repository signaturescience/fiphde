#' @title Get hospitalization data
#' @description Retrieves hospitalization data from the healthdata.gov endpoint with optional filtering on fields, and return the results into a nice tibble.
#' @param endpoint URL to healthdata.gov endpoint (see references)..
#' @param limitcols Limit the columns returned to the subjectively defined important ones?
#' @param app_token App token from healthdata.gov. If `NULL` you might get rate limited. Add an entry to your `~/.Renviron` with `HEALTHDATA_APP_TOKEN="tokenhere"` that you got from <https://healthdata.gov/profile/edit/developer_settings>.
#' @return A tibble
#' @references API documentation: <http://dev.socrata.com/foundry/healthdata.gov/g62h-syeh>.
#' @examples
#' \dontrun{
#' get_hdgov_hosp(limitcols=TRUE)
#' get_hdgov_hosp(mindate="2021-11-01", limitrows=10, limitcols=FALSE)
#' get_hdgov_hosp(state="VA")
#' get_hdgov_hosp(state="VA", mindate="2021-10-01")
#' get_hdgov_hosp(state="VA", mindate="2021-10-01", maxdate="2021-10-31")
#' get_hdgov_hosp(state="VA", mindate="2021-10-01", maxdate="2021-11-21", limitrows=5)
#' }
#' @export
get_hdgov_hosp <- function(endpoint="https://healthdata.gov/api/views/g62h-syeh/rows.csv",
                           app_token=Sys.getenv("HEALTHDATA_APP_TOKEN"),
                           limitcols=FALSE) {

  api_url <- endpoint

  if (!is.null(app_token) && is.character(app_token) && app_token!="") {
    api_url <- paste0(api_url, "?$$app_token=", app_token)
  }

  # Get the data
  d <- readr::read_csv(api_url, progress=FALSE, show_col_types = FALSE)

  # Make everything except state and date numeric
  d <-
    d %>%
    tibble::as_tibble() %>%
    dplyr::mutate(date=lubridate::as_date(date)) %>%
    dplyr::arrange(date, state) %>%
    dplyr::mutate(dplyr::across(.cols=-c(state, date), as.numeric))

  # Simplify column names for columns you care most about, and relocate to the front of the tibble
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
                  cov.deaths.cov = deaths_covid_coverage,
                  dplyr::everything())

  # Limit to a few columns you care about
  if (limitcols) {
    d <- d %>% dplyr::select(state:cov.deaths.cov)
  }

  # Data is previous day's stats. Shift dates back one day to deal with this.
  d$date <- d$date-1

  message(paste0(nrow(d), " rows retrieved from:\n", api_url))
  return(d)

}



#' @title Get vaccination data
#' @description Get vaccination data from cdc.gov endpoint.
#' @param endpoint URL to cdc.gov endpoint. See references.
#' @param app_token App token from healthdata.gov. If it's `NULL` you might get rate limited. Add an entry to your `~/.Renviron` with `HEALTHDATA_APP_TOKEN="tokenhere"` that you got from <https://healthdata.gov/profile/edit/developer_settings>.
#' @return A tibble
#' @references API documentation: <https://dev.socrata.com/foundry/data.cdc.gov/k87d-gv3u>.
#' @examples
#' \dontrun{
#' d <- get_cdc_vax()
#' d
#' library(ggplot2)
#' d %>%
#'   ggplot(aes(date, cumulative_flu_doses_distributed)) +
#'   geom_line() +
#'   facet_wrap(~season, scale="free_x") +
#'   theme_bw()
#' rm(d)
#' }
#' @export
get_cdc_vax <- function(endpoint="https://data.cdc.gov/api/views/k87d-gv3u/rows.csv",
                        app_token=Sys.getenv("HEALTHDATA_APP_TOKEN")) {

  api_url <- endpoint

  if (!is.null(app_token) && is.character(app_token) && app_token!="") {
    api_url <- paste0(api_url, "?$$app_token=", app_token)
  }

  # Get the data
  d <- readr::read_csv(api_url, progress=FALSE, show_col_types = FALSE)

  # Make it a tibble, and fix the date
  d <-
    d %>%
    tibble::as_tibble() %>%
    purrr::set_names(tolower) %>%
    dplyr::mutate(week=as.numeric(week)) %>%
    dplyr::mutate(cumulative_flu_doses_distributed=as.numeric(cumulative_flu_doses_distributed)) %>%
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
    dplyr::select(season, year, month, month_number, month_calendar_number, week, epiyear, epiweek, date, cumulative_flu_doses_distributed, imputed_value)

  message(paste0(nrow(d), " rows retrieved from:\n", api_url))
  return(d)

}


#' @title Get ILI data from CDC FluView
#' @description Get ILI data from CDC FluView. See [cdcfluview::ilinet].
#' @param region Either "state", "national", or "hhs". Defaults to `c("national", "state", "hhs") for all three`.
#' @param years A vector of years to retrieve data for. CDC has data going back to 1997. Default value (`NULL`) retrieves **all** years.
#' @return A tibble
#' @references cdcfluview documentation: <https://hrbrmstr.github.io/cdcfluview/index.html#retrieve-ilinet-surveillance-data>.
#' @examples
#' \dontrun{
#' get_cdc_ili(region="national", years=2021)
#' get_cdc_ili(region="hhs", years=2021)
#' get_cdc_ili(region="state", years=2021) %>% dplyr::filter(abbreviation=="VA")
#' get_cdc_ili(region=c("national", "state"), years=2021)
#' }
#' @export
get_cdc_ili <- function(region=c("national", "state", "hhs"), years=NULL) {
  # Check that you aren't specifying unsupported regions
  if (!all(region %in% c("national", "state", "hhs"))) stop("Invalid region. See ?get_cdc_ili.")
  # Map over regions calling cdcfluview::ilinet for that region and specified years
  d <- purrr::map_dfr(region, ~cdcfluview::ilinet(., years=years))
  # Get only relevant columns (drop age group distributions)
  # Join to internal package data to get state abbreviations and FIPS codes
  d <- d %>%
    dplyr::select(region_type, region, epiyear=year, epiweek=week, week_start, dplyr::contains("ili"), ilitotal:total_patients) %>%
    dplyr::mutate(region=gsub("National", "US", region)) %>%
    dplyr::inner_join(locations, by=c("region"="location_name")) %>%
    dplyr::select(location, region_type, abbreviation, region, dplyr::everything())
  # if region_type is States, make weighted_ili = unweighted_ili
  d <- d %>%
    dplyr::mutate(weighted_ili=ifelse(region_type=="States", unweighted_ili/1, weighted_ili))
  # Let the user know which weeks you have data for
  message(sprintf("Latest week_start / year / epiweek available:\n%s / %d / %d",
                  max(d$week_start),
                  unique(d$epiyear[d$week_start==max(d$week_start)]),
                  unique(d$epiweek[d$week_start==max(d$week_start)])))
  return(d)
}

#' @title Get hospitalization data from CDC FluView
#' @description Get hospitalization data from CDC FluView. See [cdcfluview::hospitalizations].
#' @param years A vector of years to retrieve data for (i.e. 2014 for CDC flu season 2014-2015). CDC has data going back to 2009 and up until the _previous_ flu season. Default value (`NULL`) retrieves **all** years.
#' @return A tibble
#' @references cdcfluview documentation: <https://hrbrmstr.github.io/cdcfluview/index.html#retrieve-ilinet-surveillance-data>.
#' @examples
#' \dontrun{
#' get_cdc_hosp(years=2019)
#' }
get_cdc_hosp <- function(years=NULL) {
  warning("CDC hospitalization should only be used for historical analysis. Use get_hdgov_hosp() for flusight forecasting.")
  d <- cdcfluview::hospitalizations(surveillance_area="flusurv", region="all", years=years)
  d <- d %>%
    dplyr::filter(age_label=="Overall") %>%
    dplyr::transmute(location="US",
                     abbreviation="US",
                     region="US",
                     epiyear=year,
                     epiweek=year_wk_num,
                     week_start=wk_start,
                     week_end=wk_end,
                     rate,
                     weeklyrate,
                     season=sea_label)
  message(sprintf("Latest week_start / year / epiweek available:\n%s / %d / %d",
                  max(d$week_start),
                  unique(d$epiyear[d$week_start==max(d$week_start)]),
                  unique(d$epiweek[d$week_start==max(d$week_start)])))
  return(d)
}


#' @title Get ILI nowcast
#' @description Get ILI nowcast from CMU Delphi ILI Nearby. See examples.
#' @param epiyearweeks A vector of epiyear-epiweeks to retrieve data for, e.g., 202150, 202151, etc. Exclusive with dates
#' @param dates A vector of dates to retrieve data for, e.g., ""2021-12-12" or "2021-12-19". Exclusive with epiyearweek. Defaults to two weeks prior.
#' @param location A vector of locations to retrieve. Default `NULL` retrieves all states, national, and hhs regions. See examples.
#' @return A tibble
#' @references <https://delphi.cmu.edu/nowcast/>
#' @examples
#' \dontrun{
#' # Defaults to the previous two weeks for all states
#' get_nowcast_ili()
#'
#' # Otherwise specify one or the other, not both
#' get_nowcast_ili(epiyearweeks=c("202150", "202151"), dates=NULL)
#' get_nowcast_ili(epiyearweeks=NULL, dates=c("2021-12-12", "2021-12-19"))
#'
#' # Get just one state for the last years worth of data (back 52 weeks to 1 week)
#' get_nowcast_ili(epiyearweeks=NULL,
#'                 dates=lubridate::today()-seq(52*7, 7, -7),
#'                 location="FL")
#'
#' # Compare to ilinet
#' library(dplyr)
#' library(ggplot2)
#' ilidat <- get_cdc_ili(years=2021)
#' ilinow <- get_nowcast_ili()
#' ilijoined <-
#'   inner_join(ilidat, ilinow, by = c("location", "abbreviation", "epiyear", "epiweek")) %>%
#'   select(abbreviation, epiyear, epiweek, weighted_ili, weighted_ili_now)
#' ggplot(ilijoined, aes(weighted_ili, weighted_ili_now)) + geom_point()
#' ilijoined %>%
#'   mutate(diff=weighted_ili_now-weighted_ili) %>%
#'   arrange(desc(abs(diff)))
#' }
#' @export
get_nowcast_ili <- function(epiyearweeks=NULL, dates=lubridate::today()-c(14,7), location=NULL) {

  # Check that you're not supplying both. If you are, error out.
  if (!xor(is.null(epiyearweeks), is.null(dates))) {
    stop("Either epiyearweeks or dates must be defined, but not both. Set one to NULL.")
  }

  # Base url to nowcast API
  base_url <- "https://delphi.cmu.edu/epidata/nowcast/"

  # If location is NULL, use all states, national, and hhs regions by default
  if (is.null(location)) {
    # Get the names of the states in lowercase
    states <-
      locations %>%
      dplyr::filter(location %in% stringr::str_pad(1:56, width = 2, pad = 0)) %>%
      dplyr::pull(abbreviation) %>%
      tolower()
    # paste together "nat", hhs regions, and states
    locs <- c("nat", paste0("hhs",1:10), states)
  } else {
    # if location is specified, use them
    locs <- location
  }
  # Make them all lowercase
  locs <- tolower(locs)
  # put that into a character vector separated by ,
  locs <- paste(locs, collapse=",")

  # If you supply dates instead of epiyearweeks, this will convert dates to epiyearweeks
  if (!is.null(dates)) {
    epiyearweeks <-
      MMWRweek::MMWRweek(dates) %>%
      dplyr::mutate(epiyearweeks=paste0(MMWRyear, stringr::str_pad(MMWRweek, width = 2, pad = "0", side = "left")) )%>%
      dplyr::pull(epiyearweeks)
  }
  epiyearweeks <- paste(epiyearweeks, collapse=",")

  # Construct the API URL. E.g.,
  # https://delphi.cmu.edu/epidata/nowcast/?locations=nat,hhs1,tx,va&epiweeks=201242,202151
  api_url <- paste0(base_url, "?", "locations=", locs, "&epiweeks=", epiyearweeks)

  # message(api_url)

  # Read in the raw json
  resjson <- jsonlite::read_json(api_url, simplifyVector = TRUE)

  # Parse the json. separate epiweek into epiyear/epiweek, then line up the names with our location data.
  res <-
    resjson$epidata %>%
    tibble::as_tibble() %>%
    tidyr::separate(epiweek, into=c("epiyear", "epiweek"), sep=4, convert = TRUE) %>%
    dplyr::mutate(location=toupper(location)) %>%
    dplyr::mutate(location=gsub("NAT", "US", location)) %>%
    dplyr::mutate(location=ifelse(grepl("HHS", location),
                                  location %>% stringr::str_extract("\\d+") %>% stringr::str_pad(width=2, pad=0) %>% paste0("HHS", .),
                                  location)) %>%
    dplyr::rename(abbreviation=location) %>%
    dplyr::inner_join(locations, by="abbreviation") %>%
    dplyr::select(location, abbreviation, epiyear, epiweek, weighted_ili_now=value) %>%
    dplyr::arrange(epiyear, epiweek, location)

  return(res)
}


