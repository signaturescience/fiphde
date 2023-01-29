#' @title Get hospitalization data
#' @description Retrieves hospitalization data from the healthdata.gov endpoint with optional filtering on fields, and return the results into a nice tibble.
#' @param endpoint URL to healthdata.gov endpoint (see references).
#' @param limitcols Limit the columns returned to the subjectively defined important ones? Default `FALSE`.
#' @param app_token App token from healthdata.gov. If `NULL` you might get rate limited. Add an entry to your `~/.Renviron` with `HEALTHDATA_APP_TOKEN="tokenhere"` that you got from <https://healthdata.gov/profile/edit/developer_settings>.
#' @return A tibble
#' @references API documentation: <http://dev.socrata.com/foundry/healthdata.gov/g62h-syeh>.
#' @examples
#' \dontrun{
#' get_hdgov_hosp()
#' get_hdgov_hosp(limitcols=TRUE)
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
#' @export
get_cdc_vax <- function(endpoint="https://data.cdc.gov/api/views/k87d-gv3u/rows.csv",
                        app_token=Sys.getenv("HEALTHDATA_APP_TOKEN")) {

  # This function hasn't worked in some time. Deprecating and removing.
  .Deprecated("Download and parse data at https://data.cdc.gov/api/views/k87d-gv3u/rows.csv.")
  return(NA)

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
#' @description Get ILI data from CDC FluView. See [ilinet].
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
  # Map over regions calling ilinet for that region and specified years
  d <- purrr::map_dfr(region, ~ilinet(., years=years))
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
#' @description Get hospitalization data from CDC FluView. See [hospitalizations].
#' @param years A vector of years to retrieve data for (i.e. 2014 for CDC flu season 2014-2015). CDC has data going back to 2009 and up until the _previous_ flu season. Default value (`NULL`) retrieves **all** years.
#' @return A tibble
#' @references cdcfluview documentation: <https://hrbrmstr.github.io/cdcfluview/index.html#retrieve-ilinet-surveillance-data>.
#' @examples
#' \dontrun{
#' get_cdc_hosp(years=2019)
#' }
get_cdc_hosp <- function(years=NULL) {
  warning("CDC hospitalization should only be used for historical analysis. Use get_hdgov_hosp() for flusight forecasting.")
  d <- hospitalizations(surveillance_area="flusurv", region="all", years=years)
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
#' @param state A vector of states to retrieve (two-letter abbreviation). Default `NULL` retrieves all states, national, and hhs regions. See examples.
#' @param boundatzero Bound nowcasts at zero? defaults to TRUE.
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
#'                 state="FL")
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
get_nowcast_ili <- function(epiyearweeks=NULL, dates=lubridate::today()-c(14,7), state=NULL, boundatzero=TRUE) {

  # Check that you're not supplying both. If you are, error out.
  if (!xor(is.null(epiyearweeks), is.null(dates))) {
    stop("Either epiyearweeks or dates must be defined, but not both. Set one to NULL.")
  }

  # Base url to nowcast API
  base_url <- "https://delphi.cmu.edu/epidata/nowcast/"

  # If location is NULL, use all states, national, and hhs regions by default
  if (is.null(state)) {
    # Get the names of the states in lowercase
    states <-
      locations %>%
      dplyr::filter(location %in% stringr::str_pad(1:56, width = 2, pad = 0)) %>%
      dplyr::pull(abbreviation) %>%
      tolower()
    # paste together "nat", hhs regions, and states
    locs <- c("nat", paste0("hhs",1:10), states)
  } else {
    # if state is specified, use it
    locs <- state
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

  ## check that epidata returns nowcast values for *all* weeks
  if(purrr::is_empty(resjson$epidata) | !all(epiyearweeks %in% resjson$epidata$epiweek)) {
    message("The nowcast data returned is missing some or all of the query weeks. Returning NA.")
    return(NA)
  } else {
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

    if (boundatzero) {
      res$weighted_ili_now[res$weighted_ili_now<0] <- 0
    }

    return(res)
  }
}

#' Retrieve and prep clinical laboratory percent positive flu data
#'
#' @description This function returns weekly state and/or national clinical laboratory percent positivity data from the NREVSS reporting instrument.
#'
#' @param region Either "state", "national", or "both". Defaults to "both" to return state and national data combined.
#' @param years A vector of years to retrieve data for. CDC has data going back to 1997. Default value (`NULL`) retrieves **all** years.
#'
#' @return Tibble with prepared data pulled from NREVSS, including columns for total number of positives, percent positive, epiweek, epiyear
#' @export
#'
#' @examples
#' \dontrun{
#'
#'## get all clinical lab flu positivity data
#'all_clin <- get_cdc_clin()
#'
#'## or look at a specific location and time
#'## note: the year "start" will begin at the first epiweek of the season
#'## this example 2021 will weekly data ...
#'## ... starting at beginning of 2021/22 season
#'## ... ending the week before start of 2022/23 season
#'va_clin <-
#'get_cdc_clin(region = "state", years = 2021) %>%
#'dplyr::filter(location == "51")
#'
#' }
#'
#'
get_cdc_clin <- function(region = "both", years = NULL) {

  ## handle incoming arg
  tmp_region <- tolower(region)

  tmp_years <- years

  ## if both state and national then first get state data
  ## then get natinoal
  if(tmp_region == "both") {
    state_clin <-
      who_nrevss("state", years = tmp_years)$clinical_labs %>%
      dplyr::mutate(percent_positive = as.numeric(percent_positive)) %>%
      dplyr::mutate(total_specimens = as.numeric(total_specimens)) %>%
      dplyr::mutate(total_a = as.numeric(total_a)) %>%
      dplyr::mutate(total_b = as.numeric(total_b)) %>%
      dplyr::rename(location_name = region) %>%
      dplyr::left_join(locations, by = c("location_name")) %>%
      dplyr::mutate(total_positive = total_a + total_b) %>%
      dplyr::select(abbreviation, location, epiyear = year, epiweek = week, week_start = wk_date, p_positive = percent_positive, n_positive = total_positive, total = total_specimens)

    nat_clin <-
      who_nrevss("national", years = tmp_years)$clinical_labs %>%
      dplyr::mutate(percent_positive = as.numeric(percent_positive)) %>%
      dplyr::mutate(total_specimens = as.numeric(total_specimens)) %>%
      dplyr::mutate(total_a = as.numeric(total_a)) %>%
      dplyr::mutate(total_b = as.numeric(total_b)) %>%
      dplyr::rename(location_name = region) %>%
      dplyr::mutate(abbreviation = "US",
                    location = "US") %>%
      dplyr::mutate(total_positive = total_a + total_b) %>%
      dplyr::select(abbreviation, location, epiyear = year, epiweek = week, week_start = wk_date, p_positive = percent_positive, n_positive = total_positive, total = total_specimens)

    clin <- dplyr::bind_rows(state_clin, nat_clin)
  } else {
    ## otherwise just get national or state
    clin <-
      who_nrevss(tmp_region, years = tmp_years)$clinical_labs %>%
      dplyr::mutate(percent_positive = as.numeric(percent_positive)) %>%
      dplyr::mutate(total_specimens = as.numeric(total_specimens)) %>%
      dplyr::mutate(total_a = as.numeric(total_a)) %>%
      dplyr::mutate(total_b = as.numeric(total_b)) %>%
      dplyr::rename(location_name = region) %>%
      dplyr::left_join(locations, by = c("location_name")) %>%
      dplyr::mutate(location = dplyr::if_else(region_type == "national", "US", location),
                    abbreviation = dplyr::if_else(region_type == "national", "US", abbreviation)) %>%
      dplyr::mutate(total_positive = total_a + total_b) %>%
      dplyr::select(abbreviation, location, epiyear = year, epiweek = week, week_start = wk_date, p_positive = percent_positive, n_positive = total_positive, total = total_specimens)
  }

  return(clin)

}

#' Retrieve ILINet Surveillance Data
#'
#' Adapted from cdcfluview::ilinet.
#'
#' The CDC FluView Portal provides in-season and past seasons' national, regional,
#' and state-level outpatient illness and viral surveillance data from both
#' ILINet (Influenza-like Illness Surveillance Network) and WHO/NREVSS
#' (National Respiratory and Enteric Virus Surveillance System).
#'
#' This function retrieves current and historical ILINet surveillance data for
#' the identified region. The function is used internally in `get_cdc_ili()` but is not exported.
#'
#' @param region one of "`national`", "`hhs`", "`census`", or "`state`"
#' @param years a vector of years to retrieve data for (i.e. `2014` for CDC
#'        flu season 2014-2015). CDC has data for this API going back to 1997.
#'        Default value (`NULL`) means retrieve **all** years. NOTE: if you
#'        happen to specify a 2-digit season value (i.e. `57` == 2017-2018)
#'        the function is smart enough to retrieve by season ID vs convert that
#'        to a year.
#' @references
#' - [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)
#' - [CDC FluView Portal](https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html)
#' - [ILINet Portal](https://wwwn.cdc.gov/ilinet/) (Login required)
#' - [WHO/NREVSS](https://www.cdc.gov/surveillance/nrevss/index.html)
#' @examples
#' \dontrun{
#' national_ili <- ilinet("national", years = 2017)
#' hhs_ili <- ilinet("hhs")
#' census_ili <- ilinet("census")
#' state_ili <- ilinet("state")
#'
#' all_ili <- suppressWarnings(
#'   suppressMessages(purrr::map_df(c("national", "hhs", "census", "state"), ilinet)))
#' }
ilinet <- function(region = c("national", "hhs", "census", "state"), years = NULL) {

  ## handle region argument
  region <- match.arg(tolower(region), c("national", "hhs", "census", "state"))

  ## get metadata from CDC fluview api
  meta <- jsonlite::fromJSON("https://gis.cdc.gov/grasp/flu2/GetPhase02InitApp?appVersion=Public")
  ## use metadata to determine available season ids
  available_seasons <- sort(meta$seasons$seasonid)

  ## add params for query
  params <- list(
    AppVersion = "Public",
    DatasourceDT = list(list(ID = 1, Name = "ILINet")),
    RegionTypeId = c(national = 3, hhs = 1, census = 2, state = 5)[region]
  )

  ## add another param for subregion switched on region entered in function arg
  params$SubRegionsDT <- switch(region,
                                national = {
                                  list(list(ID = 0, Name = ""))
                                },
                                hhs = {
                                  lapply(1:10, function(i) list(ID = i, Name = as.character(i)))
                                },
                                census = {
                                  lapply(1:9, function(i) list(ID = i, Name = as.character(i)))
                                },
                                state = {
                                  lapply(1:59, function(i) list(ID = i, Name = as.character(i)))
                                }
  )

  ## default to all years if years is NULL
  ## otherwise parse out years based given season ids and available seasons
  if (is.null(years)) {
    years <- available_seasons
  } else {

    years <- as.numeric(years)
    years <- ifelse(years > 1996, years - 1960, years)
    years <- sort(unique(years))
    years <- years[years %in% available_seasons]

    if (length(years) == 0) {
      years <- rev(sort(meta$seasons$seasonid))[1]
      curr_season_descr <- meta$seasons[meta$seasons$seasonid == years, "description"]
      message(sprintf(
        "No valid years specified, defaulting to this flu season => ID: %s [%s]",
        years, curr_season_descr
      ))
    }
  }

  ## add another parameter for the query for the seasons to return
  params$SeasonsDT <- lapply(years, function(i) list(ID = i, Name = as.character(i)))

  ## path to tempfile for downloading query results
  tf <- tempfile(fileext = ".zip")

  on.exit(unlink(tf), TRUE)

  ## establish user agent for query
  ua <- "Mozilla/5.0 (compatible; R-fiphde Bot/1.0; https://github.com/signaturescience/fiphde)"

  ## http POST to download the params specified
  tmp_q <-
    httr::POST(
      url = "https://gis.cdc.gov/grasp/flu2/PostPhase02DataDownload",
      httr::user_agent(ua),
      httr::add_headers(
        Origin = "https://gis.cdc.gov",
        Accept = "application/json, text/plain, */*",
        Referer = "https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html"
      ),
      encode = "json",
      body = params,
      httr::write_disk(tf)
    )

  httr::stop_for_status(tmp_q)

  nm <- utils::unzip(tf, overwrite = TRUE, exdir = tempdir())

  ## read in downloaded data and clean up
  xdf <- utils::read.csv(nm, skip = 1, stringsAsFactors = FALSE)
  ## first clean up column names
  xdf <- .mcga(xdf)

  ## then convert all of these columns to numeric (after cleaning up the special chars)
  xdf$weighted_ili <- to_num(xdf$weighted_ili)
  xdf$unweighted_ili <- to_num(xdf$unweighted_ili)
  xdf$age_0_4 <- to_num(xdf$age_0_4)
  xdf$age_25_49 <- to_num(xdf$age_25_49)
  xdf$age_25_64 <- to_num(xdf$age_25_64)
  xdf$age_5_24 <- to_num(xdf$age_5_24)
  xdf$age_50_64 <- to_num(xdf$age_50_64)
  xdf$age_65 <- to_num(xdf$age_65)
  xdf$ilitotal <- to_num(xdf$ilitotal)
  xdf$num_of_providers <- to_num(xdf$num_of_providers)
  xdf$total_patients <- to_num(xdf$total_patients)
  xdf$week_start <- MMWRweek::MMWRweek2Date(xdf$year, xdf$week)

  ## set region column value in tabuladated data
  if (region == "national") xdf$region <- "National"
  if (region == "hhs") xdf$region <- factor(xdf$region, levels = sprintf("Region %s", 1:10))

  ## make sure fucntion returns a tibble
  class(xdf) <- c("tbl_df", "tbl", "data.frame")

  dplyr::arrange(suppressMessages(readr::type_convert(xdf)), week_start)

}

#' Laboratory-Confirmed Influenza Hospitalizations
#'
#' Adapted from cdcfluview::ilinet.
#'
#' @param surveillance_area one of "`flusurv`", "`eip`", or "`ihsp`"
#' @param region Using "`all`" mimics selecting "Entire Network" from the
#'        CDC FluView application drop down. Individual regions for each
#'        surveillance area can also be selected. Use [surveillance_areas()] to
#'        see a list of valid sub-regions for each surveillance area.
#' @param years a vector of years to retrieve data for (i.e. `2014` for CDC
#'        flu season 2014-2015). CDC has data for this API going back to 2009
#'        and up until the _previous_ flu season.
#'        Default value (`NULL`) means retrieve **all** years. NOTE: if you
#'        happen to specify a 2-digit season value (i.e. `56` == 2016-2017)
#'        the function is smart enough to retrieve by season ID vs convert that
#'        to a year.
#' @references
#' - [Hospital Portal](https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html)
#' - [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)
#' @examples
#' \dontrun{
#' hosp_fs <- hospitalizations("flusurv", years=2015)
#' hosp_eip <- hospitalizations("eip")
#' hosp_ihsp <- hospitalizations("ihsp")
#' }
hospitalizations <- function(surveillance_area=c("flusurv", "eip", "ihsp"),
                             region="all", years=NULL) {

  ## handle surveillance area argument
  sarea <- match.arg(tolower(surveillance_area), choices = c("flusurv", "eip", "ihsp"))
  sarea <- c(flusurv = "FluSurv-NET", eip = "EIP", ihsp = "IHSP")[sarea]

  ## get metadata from CDC fluview API
  meta <- jsonlite::fromJSON("https://gis.cdc.gov/GRASP/Flu3/GetPhase03InitApp?appVersion=Public")
  ## use metadata to establish catchment data
  areas <- stats::setNames(meta$catchments[,c("networkid", "name", "area", "catchmentid")],
                           c("networkid", "surveillance_area", "region", "id"))

  ## handle region argument
  reg <- region
  if (reg == "all") reg <- "Entire Network"

  tgt <- dplyr::filter(areas, (surveillance_area == sarea) & (region == reg))

  if (nrow(tgt) == 0) {
    stop("Region not found. Use `surveillance_areas()` to see a list of valid inputs.",
         call.=FALSE)
  }

  ## establish user agent for query
  ua <- "Mozilla/5.0 (compatible; R-fiphde Bot/1.0; https://github.com/signaturescience/fiphde)"

  ## submit query to API
  tmp_q <-
    httr::POST(
      url = "https://gis.cdc.gov/GRASP/Flu3/PostPhase03GetData",
      httr::user_agent(ua),
      httr::add_headers(
        Origin = "https://gis.cdc.gov",
        Accept = "application/json, text/plain, */*",
        Referer = "https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html"
      ),
      encode = "json",
      body = list(
        appversion = "Public",
        networkid = tgt$networkid,
        cacthmentid = tgt$id
      ),
      httr::timeout(120)
    )

  httr::stop_for_status(tmp_q)

  ## parse query
  res <- httr::content(tmp_q)

  ## create a list object with metadata returned and the http query response
  hosp <- list(res = res, meta = meta)

  ## create a tibble with age metadata and labels
  age_df <- stats::setNames(hosp$meta$ages, c("age_label", "age", "color"))
  age_df <- age_df[,c("age", "age_label")]

  ## create a tibble with season metadata
  sea_df <- stats::setNames(
    hosp$meta$seasons,
    c("sea_description", "sea_endweek", "sea_label", "seasonid", "sea_startweek", "color", "color_hexvalue")
  )
  sea_df <- sea_df[,c("seasonid", "sea_label", "sea_description", "sea_startweek", "sea_endweek")]

  ## variable names for hospital data
  ser_names <- unlist(hosp$res$busdata$datafields, use.names = FALSE)

  ## create tibble with hospital data
  suppressWarnings(
    suppressMessages(
      mmwr_df <- dplyr::bind_rows(hosp$res$mmwr)
    )
  )

  ## create tibble that maps mmwr week to season
  mmwr_df <- mmwr_df[,c("mmwrid", "weekend", "weeknumber", "weekstart", "year",
                        "yearweek", "seasonid", "weekendlabel", "weekendlabel2")]

  ## start binding actual hospitalization data to information about season and age groups queried
  suppressMessages(
    suppressWarnings(

      xdf <-
        dplyr::bind_rows(
          lapply(hosp$res$busdata$dataseries, function(.x) {
            tdf <-
              dplyr::bind_rows(
                lapply(.x$data, function(.x) stats::setNames(.x, ser_names))
              )

            tdf$age <- .x$age
            tdf$season <- .x$season
            tdf
          })
        )

    )
  )

  ## create age labels as factors
  if (length(unique(xdf$age)) > 9) {
    age_df <-
      data.frame(
        age = 1:12,
        age_label = c("0-4 yr", "5-17 yr", "18-49 yr", "50-64 yr", "65+ yr", "Overall",
                      "65-74 yr", "75-84 yr", "85+", "18-29 yr", "30-39 yr", "40-49 yr"
        )
      )
    age_df$age_label <- factor(age_df$age_label, levels = age_df$age_label)
  }

  ## join results data to information about mmwr week / season / age group and labels
  ## add a column for the surveillance area based on choice of "flusurv", "eip", or "ihsp"
  ## and add a column for region based on region argument value
  ## lastly join to mmwrid_map to get week start and week end
  xdf <-
    dplyr::left_join(xdf, mmwr_df, c("mmwrid", "weeknumber")) %>%
    dplyr::left_join(age_df, "age") %>%
    dplyr::left_join(sea_df, "seasonid") %>%
    dplyr::mutate(
      surveillance_area = sarea,
      region = reg
    ) %>%
    dplyr::left_join(mmwrid_map, "mmwrid")

  ## subset to columns of interest
  xdf <- xdf[,c("surveillance_area", "region", "year", "season", "wk_start", "wk_end",
                "year_wk_num", "rate", "weeklyrate", "age", "age_label", "sea_label",
                "sea_description", "mmwrid")]

  ## make sure that the data returned is for years selected
  ## based on available seasons
  available_seasons <- sort(unique(xdf$season))

  if (!is.null(years)) { # specified years or seasons or a mix

    years <- as.numeric(years)
    years <- ifelse(years > 1996, years - 1960, years)
    years <- sort(unique(years))
    years <- years[years %in% available_seasons]

    if (length(years) == 0) {
      years <- rev(available_seasons)[1]
      curr_season_descr <- xdf[xdf$season == years,]$sea_description[1]
      message(
        sprintf(
          "No valid years specified, defaulting to the last available flu season => ID: %s [%s]",
          years, curr_season_descr
        )
      )
    }

    xdf <- dplyr::filter(xdf, season %in% years)

  }

  xdf

}

#' WHO/NREVSS surveillance data
#'
#' Adapted from cdcfluview::who_nrevss.
#'
#' @param region one of "`national`", "`hhs`", "`census`", or "`state`"
#' @param years a vector of years to retrieve data for (i.e. `2014` for CDC
#'        flu season 2014-2015). CDC has data for this API going back to 1997.
#'        Default value (`NULL`) means retrieve **all** years. NOTE: if you
#'        happen to specify a 2-digit season value (i.e. `57` == 2017-2018)
#'        the function is smart enough to retrieve by season ID vs convert that
#'        to a year.
#' @references
#' - [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)
#' @examples
#' \dontrun{
#' national_nrevss <- who_nrevss("national")
#' hhs_who <- who_nrevss("hhs")
#' census_who <- who_nrevss("census")
#' state_who <- who_nrevss("state")
#' }
who_nrevss <- function(region = c("national", "hhs", "census", "state"), years = NULL) {

  ## handle region arg
  region <- match.arg(tolower(region), c("national", "hhs", "census", "state"))

  ## get metadata from CDC fluview api
  meta <- jsonlite::fromJSON("https://gis.cdc.gov/grasp/flu2/GetPhase02InitApp?appVersion=Public")
  ## use metadata to determine available season ids
  available_seasons <- sort(meta$seasons$seasonid)

  ## add params for query
  params <-
    list(
      AppVersion = "Public",
      DatasourceDT = list(list(ID = 1, Name = "WHO_NREVSS")),
      RegionTypeId = c(national = 3, hhs = 1, census = 2, state = 5)[region]
    )

  params$SubRegionsDT <- switch(
    region,
    national = { list(list(ID=0, Name="")) },
    hhs = { lapply(1:10, function(i) list(ID=i, Name=as.character(i))) },
    census = { lapply(1:9, function(i) list(ID=i, Name=as.character(i))) },
    state = { lapply(1:59, function(i) list(ID=i, Name=as.character(i))) }
  )


  ## default to all years if years is NULL
  ## otherwise parse out years based given season ids and available seasons
  if (is.null(years)) {
    years <- available_seasons
  } else {

    years <- as.numeric(years)
    years <- ifelse(years > 1996, years - 1960, years)
    years <- sort(unique(years))
    years <- years[years %in% available_seasons]

    if (length(years) == 0) {
      years <- rev(sort(meta$seasons$seasonid))[1]
      curr_season_descr <- meta$seasons[meta$seasons$seasonid == years, "description"]
      message(sprintf("No valid years specified, defaulting to this flu season => ID: %s [%s]",
                      years, curr_season_descr))
    }

  }

  ## add another parameter for the query for the seasons to return
  params$SeasonsDT <- lapply(years, function(i) list(ID=i, Name=as.character(i)))

  ## path to tempfile for downloading query results
  tf <- tempfile(fileext = ".zip")
  td <- tempdir()

  on.exit(unlink(tf), TRUE)

  ua <- "Mozilla/5.0 (compatible; R-fiphde Bot/1.0; https://github.com/signaturescience/fiphde)"

  tmp_q <-
    httr::POST(
    url = "https://gis.cdc.gov/grasp/flu2/PostPhase02DataDownload",
    httr::user_agent(ua),
    httr::add_headers(
      Origin = "https://gis.cdc.gov",
      Accept = "application/json, text/plain, */*",
      Referer = "https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html"
    ),
    encode = "json",
    body = params,
    httr::timeout(120),
    httr::write_disk(tf)
  )

  httr::stop_for_status(tmp_q)

  ## unzip the downloaded tempfile to the tempdir() defined above
  nm <- utils::unzip(tf, overwrite = TRUE, exdir = td)

  xdf <-
    lapply(nm, function(x) {

    tdf <- utils::read.csv(x, skip = 1, stringsAsFactors=FALSE)
    tdf <- .mcga(tdf)
    class(tdf) <- c("tbl_df", "tbl", "data.frame")

    tdf[tdf=="X"] <- NA
    tdf[tdf=="XX"] <- NA

    tdf

  })

  xdf <- stats::setNames(xdf, sub("who_nrevss_", "", tools::file_path_sans_ext(tolower(basename(nm)))))

  ## apply a clean up function to
  ## 1) get "wk_date" for epiyear and epiweek
  ## 2) title case "National" if the region is "national"
  xdf <- lapply(xdf, function(.x) {
    x_cols <- colnames(.x)
    if ((("year" %in% x_cols) & ("week" %in% x_cols))) {
      .x$wk_date <- suppressWarnings(mmwr_week_to_date(.x$year, .x$week))
    } else {
      .x$wk_date <- as.Date(NA)
    }
    if (region == "national") .x$region <- "National"
    .x
  })

  xdf

}

