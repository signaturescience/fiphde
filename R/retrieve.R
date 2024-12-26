#' @title Retrieve hospitalization data from HHS
#'
#' @description
#'
#' This function retrieves hospital utilization time series data distributed through healthdata.gov. Data are aggregated to the state granularity from facility level reports via HHS TeleTracking, HHS Protect, and the National Healthcare Safety Network (historically). Users can optionally filter to include all fields or restrict to a prespecified set of fields relevant to COVID and influenza hospital utilization. The results are returned as a `tibble`.
#'
#' @param endpoint URL to healthdata.gov endpoint
#' @param limitcols Logical as to whether or not to limit to prespecified set of columns (see "Value" section for more details); default `FALSE`
#' @param app_token App token from healthdata.gov; default is to look for environment variable called `"HEALTHDATA_APP_TOKEN"` and if a token is not supplied to proceed with possibility of rate limitation (see "Details" for more information)
#' @param shift_back Logical as to whether or not the dates for the retrieved data should be shifted back to reflect previous day; default is `TRUE`
#' @details The data retrieval will proceed whether or not an API token has been supplied via the `app_token` argument. However, to avoid possible rate limits it is recommended to retrieve a token for the healthdata.gov API (<https://healthdata.gov/profile/edit/developer_settings>), and add that token as an entry to `.Renviron` with `HEALTHDATA_APP_TOKEN="yourtokenhere"`.
#' @return A `tibble` with at least the following columns:
#'
#' - **state**: Abbreviation of the state
#' - **date**: Date of report
#' - **flu.admits**: Count of flu cases among admitted patients on previous day
#' - **flu.admits.cov**: Coverage (number of hospitals reporting) for incident flu cases
#' - **flu.deaths**: Count of flu deaths on previous day
#' - **flu.deaths.cov**: Coverage (number of hospitals reporting) for flu deaths
#' - **flu.icu**: Count of flu cases among ICU patients on previous day
#' - **flu.icu.cov**: Coverage (number of hospitals reporting) for flu ICU cases
#' - **flu.tot**: Count of total flu cases among admitted patients
#' - **flu.tot.cov**: Coverage (number of hospitals reporting) for total flu cases
#' - **cov.admits**: Count of COVID cases among admitted patients on previous day
#' - **cov.admits.cov**: Coverage (number of hospitals reporting) for incident COVID cases
#' - **cov.deaths**: Count of COVID deaths on previous day
#' - **cov.deaths.cov**: Coverage (number of hospitals reporting) for COVID deaths
#'
#' If `limitcols=TRUE` then the only columns returned will be those listed above. However, if `limitcols=FALSE` then the function will additionally return *all* other fields in the state-aggregated hospitalization data.
#'
#' @references <https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh>
#' @references <https://dev.socrata.com/foundry/healthdata.gov/g62h-syeh>
#'
#' @examples
#' \dontrun{
#' # Retrieve hospitalization data (all columns)
#' get_hdgov_hosp()
#' # Retrieve hospitalization data (limited columns)
#' get_hdgov_hosp(limitcols=TRUE)
#' }
#' @export
get_hdgov_hosp <- function(endpoint="https://healthdata.gov/api/views/g62h-syeh/rows.csv",
                           app_token=Sys.getenv("HEALTHDATA_APP_TOKEN"),
                           limitcols=FALSE,
                           shift_back = TRUE) {

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
  if(shift_back) {
    d$date <- d$date-1
  }

  message(paste0(nrow(d), " rows retrieved from:\n", api_url))
  return(d)

}

#' @title Retrieve ILI data from ILINet
#'
#' @description
#'
#' This function pulls ILINet data from the CDC FluView API. Data are available historically and can be pulled at the state, national, or HHS region level.
#'
#' @param region Either "state", "national", or "hhs"; defaults to `c("national", "state", "hhs")` for all three.
#' @param years A vector of years to retrieve data for. CDC has data going back to 1997. Default value (`NULL`) retrieves **all** years.
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
#' - **unweighted_ili**: Unweighted percentage of ILI outpatient visits
#' - **ilitotal**: Total number of ILI outpatient visits reported
#' - **num_providers**: Number of providers reporting
#' - **total_patients**: Total number of outpatient visits reported
#' - **population**: Total population for the given location
#'
#' @references <https://gis.cdc.gov/grasp/fluview/FluViewPhase1QuickReferenceGuide.pdf>
#' @examples
#' \dontrun{
#' # Retrieve ILI data for specific years and regions
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

#' @title Retrieve hospitalization data from FluSurv-NET
#'
#' @description
#'
#' This function retrieves historical FluSurv-NET hospitalization data via either RESP-NET or the CDC FluView API (see 'Details' section).
#'
#' @param source The source for the hospitalization data; must be one of `"fluview"` or `"resp-net"`; default is `"fluview"`
#' @param years A vector of years to retrieve data for (i.e. 2014 for CDC flu season 2014-2015). CDC has data going back to 2009 and up until the _previous_ flu season. Default value (`NULL`) retrieves **all** years. Only used if `source="fluview"`.
#'
#' @return A `tibble` with the following columns:
#'
#' - **location**: FIPS code for the location
#' - **abbreviation**: Abbreviation for the location
#' - **region**: Name of the region
#' - **epiyear**: Year of reporting (in epidemiological week calendar)
#' - **epiweek**: Week of reporting (in epidemiological week calendar)
#' - **week_start**: Date of beginning (Sunday) of the given epidemiological week
#' - **week_end**: Date of end (Saturday) of the given epidemiological week
#' - **rate**: The cumulative rate per 100k
#' - **weekly_rate**: The weekly rate per 100k
#' - **season**: The flu season to which the given epidemiological week belongs
#'
#' @details
#' The data retrieval from FluView and and RESP-NET pulls FluSurv-NET lab-confirmed hospitalization data as overall cumulative rates and weekly incident rates across reporting sites, age groups, sex, and race/ethnicity categories. Note that as of October 2024, the FluSurv-NET hospitalizations from RESP-NET begin in the 2018-19 season, while the FluView data goes back to 2009-10.
#'
#' @references
#' - [Hospital Portal](https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html)
#' - [cdcfluview package](https://github.com/hrbrmstr/cdcfluview)
#' - [RESP-NET Data Portal](https://data.cdc.gov/Public-Health-Surveillance/Rates-of-Laboratory-Confirmed-RSV-COVID-19-and-Flu/kvib-3txy/about_data)
#' - [RESP-NET Dashboard](https://www.cdc.gov/resp-net/dashboard/)
#' @export
#' @examples
#' \dontrun{
#' # Retrieve FluSurv-Net hospitalization data for specific year(s)
#' get_cdc_hosp()
#' }
get_cdc_hosp <- function(source = "fluview", years=NULL) {

  if(tolower(source) == "fluview") {
    d <- hospitalizations(surveillance_area="flusurv", region="all", years=years)
    d <- d %>%
      dplyr::filter(.data$age_label=="Overall") %>%
      dplyr::filter(.data$race_label=="Overall") %>%
      dplyr::filter(.data$sexid == 0) %>%
      dplyr::filter(.data$name == "FluSurv-NET") %>%
      ## get the overall for influenza a/b
      dplyr::filter(.data$flutype == 0) %>%
      dplyr::filter(!is.na(.data$weeklyrate) & !is.na(.data$rate)) %>%
      dplyr::transmute(location="US",
                       abbreviation="US",
                       region="US",
                       epiyear=year,
                       epiweek=weeknumber,
                       week_end=weekend,
                       week_start=weekstart,
                       rate,
                       weekly_rate = weeklyrate,
                       season=season_label) %>%
      ## coerce to tibble
      dplyr::as_tibble(.)
  } else if (tolower(source) == "resp-net") {

    d <- get_respnet()

    d <-
      d %>%
      dplyr::filter(.data$race_ethnicity == "Overall") %>%
      dplyr::filter(.data$age_group == "Overall") %>%
      dplyr::filter(.data$sex == "Overall") %>%
      dplyr::filter(.data$site == "Overall") %>%
      dplyr::mutate(location = "US", abbreviation = "US", region = "US") %>%
      dplyr::select(-c("age_group","sex","race_ethnicity","site","type","surveillance_network")) %>%
      dplyr::rename("rate" = "cumulative_rate") %>%
      dplyr::select(c("location","abbreviation","region","epiyear","epiweek","week_start","week_end","rate","weekly_rate","season"))

  }
  message(sprintf("Latest week_start / year / epiweek available:\n%s / %d / %d",
                  max(d$week_start),
                  unique(d$epiyear[d$week_start==max(d$week_start)]),
                  unique(d$epiweek[d$week_start==max(d$week_start)])))
  return(d)
}


#' @title Retrieve ILI nowcast
#'
#' @description
#'
#' This function pulls the ILI nowcast from CMU Delphi's ILI Nearby API. Observed ILINet data is typically reported with a lag, and the ILI nowcast can be used to augment the ILI data stream. The functionality here depends on availability of the ILI Nearby API (see 'Details' section).
#'
#' @param epiyearweeks A vector of epiyear-epiweeks to retrieve data for, e.g., 202150, 202151, etc. Exclusive with dates
#' @param dates A vector of dates to retrieve data for, e.g., ""2021-12-12" or "2021-12-19". Exclusive with epiyearweek. Defaults to two weeks prior.
#' @param state A vector of states to retrieve (two-letter abbreviation). Default `NULL` retrieves all states, national, and hhs regions. See examples.
#' @param boundatzero Logical as to whether or not the values should be truncated at 0 (i.e., non-negative); default is `TRUE`
#' @return Either `NA` (if the API can't be reached) or a `tibble` with the following columns:
#'
#' - **location**: FIPS code for the location
#' - **abbreviation**: Abbreviation for the location
#' - **epiyear**: Year of reporting (in epidemiological week calendar)
#' - **epiweek**: Week of reporting (in epidemiological week calendar)
#' - **weighted_ili_now**: Nowcasted ILI value
#'
#' @details
#'
#' As of October 2022 ILInearby was no longer being updated. As such, the `get_nowcast_ili()` will likely return 'NA'. See <https://github.com/cmu-delphi/delphi-epidata/issues/993>.
#'
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
#' }
#' @export
get_nowcast_ili <- function(epiyearweeks=NULL, dates=lubridate::today()-c(14,7), state=NULL, boundatzero=TRUE) {

  warning("As of October 2022 ILInearby was no longer being updated. This will likely return 'NA'. See https://github.com/cmu-delphi/delphi-epidata/issues/993")

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
  if(purrr::is_empty(resjson$epidata) | !all(strsplit(epiyearweeks, split=",")[[1]] %in% resjson$epidata$epiweek)) {
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

#' @title Retrieve clinical laboratory percent positive flu data
#'
#' @description
#'
#' This function returns weekly state and/or national clinical laboratory percent positivity data from the NREVSS reporting instrument via the CDC FluView API.
#'
#' @param region Either "state", "national", or "both". Defaults to `"both"` to return state and national data combined.
#' @param years A vector of years to retrieve data for. CDC has data going back to 1997. Default value (`NULL`) retrieves **all** years.
#'
#' @return A `tibble` with the following columns:
#'
#' - **abbreviation**: Abbreviation for the location
#' - **location**: FIPS code for the location
#' - **epiyear**: Year of reporting (in epidemiological week calendar)
#' - **epiweek**: Week of reporting (in epidemiological week calendar)
#' - **week_start**: Date of beginning (Sunday) of the given epidemiological week
#' - **p_positive**: Percentage of positive specimens
#' - **n_positive**: Total number of positive specimens
#' - **total**: Total number of specimens tested
#'
#' @references <https://gis.cdc.gov/grasp/fluview/Phase_6_Cleared_Help.pdf>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all clinical lab flu positivity data
#' all_clin <- get_cdc_clin()
#' all_clin
#' # Alternatively look at a specific location and time
#' # This 2021 will return weekly data
#' # Starting at beginning of 2021/22 season
#' # Ending the week before start of 2022/23 season
#' va_clin <-
#'   get_cdc_clin(region = "state", years = 2021) %>%
#'   dplyr::filter(location == "51")
#' va_clin
#' }
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

#' @title Retrieve ILINet surveillance data
#'
#' @description
#'
#' Adapted from `cdcfluview::ilinet`.
#'
#' This unexported helper function retrieves current and historical ILINet surveillance data for
#' the identified region via the CDC FluView API. The function is used internally in [get_cdc_ili]. Data returned include weighted and unweighted ILI percentage, as well as age-specific ILI outpatient visit counts for each location / epidemiological week.
#'
#' @param region One of "`national`", "`hhs`", "`census`", or "`state`"
#' @param years A vector of years to retrieve data for (i.e. `2014` for CDC
#'        flu season 2014-2015). CDC has data for this API going back to 1997.
#'        Default value (`NULL`) means retrieve **all** years. NOTE: if you
#'        happen to specify a 2-digit season value (i.e. `57` == 2017-2018)
#'        the function is smart enough to retrieve by season ID vs convert that
#'        to a year.
#' @references
#' - [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)
#' - [CDC FluView Portal](https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html)
#'
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

#' @title Laboratory-confirmed influenza hospitalizations
#'
#' @description
#'
#' Adapted from `cdcfluview::hospitalizations`.
#'
#' This unexported helper function leverages the CDC FluView API to pull influenza hospitalizations collected by surveillance instruments (including FluSurv-NET). The data retrieved can be parameterized by geographic granularity and/or flu season, and includes hospitalization rates by age group. The function is used internally by [get_cdc_hosp].
#'
#' @param surveillance_area One of `"flusurv"`, `"eip"`, or `"ihsp"`
#' @param region Individual region within the surveillance area selected; default `"all"` mimics selecting "Entire Network" from the CDC FluView application drop down; see "Details" for list of valid region values for each surveillance area
#' @param years A vector of years to retrieve data for (i.e. `2014` for CDC
#'        flu season 2014-2015). CDC has data for this API going back to 2009
#'        and up until the _previous_ flu season.
#'        Default value (`NULL`) means retrieve **all** years. NOTE: if you
#'        happen to specify a 2-digit season value (i.e. `56` == 2016-2017)
#'        the function is smart enough to retrieve by season ID vs convert that
#'        to a year.
#'
#' @details
#'
#' **NOTE**: The list of regions was compiled in February 2023 by querying the CDC FluView API. Individual regions may not be accessible in all cases. As of late 2023, the query was only returning results for the "Entire Network" selection.
#'
#' Each possible value "surveillance_area" (`"flusurv"`, `"eip"`, or `"ihsp"`) can be further queried by region. The following is a list of valid regions:
#'
#' - **flusurv**: "Entire Network"
#' - **eip**: "Entire Network", "California", "Colorado", "Connecticut", "Georgia", "Maryland", "Minnesota", "New Mexico", "New York - Albany", "New York - Rochester", "Oregon, "Tennessee"
#' - **ihsp**: "Entire Network", "Idaho", "Iowa", "Michigan", "Ohio", "Oklahoma", "Rhode Island", "South Dakota", "Utah"
#'
#'
#' @references
#' - [Hospital Portal](https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html)
#' - [cdcfluview package](https://github.com/hrbrmstr/cdcfluview)
#'
hospitalizations <- function(surveillance_area=c("flusurv", "eip", "ihsp"),
                             region="all", years=NULL) {

  ## handle surveillance area argument
  sarea <- match.arg(tolower(surveillance_area), choices = c("flusurv", "eip", "ihsp"))
  sarea <- c(flusurv = "FluSurv-NET", eip = "EIP", ihsp = "IHSP")[sarea]

  ## establish user agent for query
  ua <- "Mozilla/5.0 (compatible; R-fiphde Bot/1.0; https://github.com/signaturescience/fiphde)"

  body <-
    list(
      appversion = jsonlite::unbox("Public"),
      key = jsonlite::unbox(""),
      injson = I(list())
    )

  ## submit query to API
  tmp_q <-
    httr::POST(
      httr::user_agent(ua),
      url = "https://gis.cdc.gov/GRASP/Flu3/PostPhase03DataTool",
      body = jsonlite::toJSON(body),
      encode = "raw",
      httr::accept_json(),
      httr::add_headers(
        `content-type` = "application/json;charset=UTF-8",
        origin = "https://gis.cdc.gov",
        referer = "https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html"
      ),
      httr::timeout(120)
    )

  httr::stop_for_status(tmp_q)

  meta <- jsonlite::fromJSON(httr::content(tmp_q, as = "text"))

  ## use metadata to establish catchment data
  areas <- stats::setNames(meta$catchments[,c("networkid", "name", "area", "catchmentid")],
                           c("networkid", "surveillance_area", "region", "id"))

  ## handle region argument
  reg <- region
  if (reg == "all") reg <- "Entire Network"

  tgt <- dplyr::filter(areas, (surveillance_area == sarea) & (region == reg))

  if (nrow(tgt) == 0) {
    stop("Region not found. See ?hospitalizations to see a list of valid inputs.",
         call.=FALSE)
  }

  ## create a list object with metadata returned and the http query response
  hosp <- list(res = meta$default_data, meta = meta)

  ## create a tibble with age metadata and labels
  age_df <- stats::setNames(hosp$meta$master_lookup, c("variable", "value_id", "parent_id", "label", "color", "enabled"))
  age_df <- age_df[(age_df$variable == "Age" | age_df$value_id == 0) & !is.na(age_df$value_id),]
  age_df <- stats::setNames(age_df[,c("value_id", "label")], c("ageid", "age_label"))
  age_df <- age_df[order(age_df$ageid),]

  ## cerate a tibble with race metadata
  race_df <- stats::setNames(hosp$meta$master_lookup, c("variable", "value_id", "parent_id", "label", "color", "enabled"))
  race_df <- race_df[(race_df$variable == "Race" | race_df$value_id == 0) & !is.na(race_df$value_id),]
  race_df <- stats::setNames(race_df[,c("value_id", "label")], c("raceid", "race_label"))

  ## create a tibble with season metadata
  season_df <-
    hosp$meta$seasons %>%
    purrr::set_names(c("season_description", "season_enabled", "season_endweek", "season_label", "seasonid", "season_startweek", "include_weekly"))

  season_df <- season_df[,c("seasonid", "season_label", "season_description", "season_startweek", "season_endweek")]

  ## create tibble that maps mmwr week to season
  mmwr_df <- hosp$meta$mmwr
  mmwr_df <- mmwr_df[,c("mmwrid", "weekend", "weeknumber", "weekstart", "year",
                        "yearweek", "seasonid", "weekendlabel", "weekendlabel2")]
  mmwr_df$seasonid <- NULL

  ## create tibble with catchment metadata
  catchments_df <- hosp$meta$catchments[,c("catchmentid", "beginseasonid", "endseasonid", "networkid", "name", "area")]
  catchments_df$catchmentid <- as.character(catchments_df$catchmentid)
  catchments_df$networkid <- NULL

  ## join results data to information about mmwr week / season / age group / race and labels
  ## add a column for the surveillance area based on choice of "flusurv", "eip", or "ihsp"
  ## and add a column for region based on region argument value
  ## lastly join to mmwrid_map to get week start and week end
  xdf <-
    hosp$res %>%
    dplyr::left_join(mmwr_df, "mmwrid") %>%
    dplyr::left_join(age_df, "ageid") %>%
    dplyr::left_join(race_df, "raceid") %>%
    dplyr::left_join(season_df, "seasonid") %>%
    dplyr::mutate(catchmentid = as.character(catchmentid)) %>%
    dplyr::left_join(catchments_df, "catchmentid") %>%
    dplyr::mutate(
      surveillance_area = sarea,
      region = reg
    )


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

#' @title Retrieve data from RESP-NET
#'
#' @description
#'
#' This unexported helper function retrieves data from the RESP-NET API for respiratory hospitalizations. The data retrieved can be filtered to specific network(s) reported. The function is used internally by [get_cdc_hosp].
#'
#' @param network The name of the RESP-NET network to query; must be one of `"FluSurv-NET"`, `"COVID-NET"`, `"RSV-NET"`, or `"Combined"`; default is `"FluSurv-NET"`
#'
#' @references
#' - [RESP-NET Data Portal](https://data.cdc.gov/Public-Health-Surveillance/Rates-of-Laboratory-Confirmed-RSV-COVID-19-and-Flu/kvib-3txy/about_data)
#' - [RESP-NET Dashboard](https://www.cdc.gov/resp-net/dashboard/)
#'
get_respnet <- function(network="FluSurv-NET") {

  dat <- readr::read_csv("https://data.cdc.gov/api/views/kvib-3txy/rows.csv")

  dat %>%
    dplyr::rename(
      "surveillance_network" = "Surveillance Network",
      "season" = "Season",
      "epiyear" = "MMWR Year",
      "epiweek" = "MMWR Week",
      "age_group" = "Age group",
      "sex" = "Sex",
      "race_ethnicity" = "Race/Ethnicity",
      "site" = "Site",
      "weekly_rate" = "Weekly Rate",
      "cumulative_rate" = "Cumulative Rate",
      "week_end" = "Week Ending Date",
      "type" = "Type"
    ) %>%
    dplyr::filter(
      .data$surveillance_network == .env$network
    ) %>%
    dplyr::mutate(week_end = as.Date(.data$week_end)) %>%
    dplyr::mutate(week_start = .data$week_end - 6)

}


#' @title WHO/NREVSS clinical lab surveillance data
#'
#' @description
#'
#' Adapted from `cdcfluview::who_nrevss`.
#'
#' This unexported helper function leverages the CDC FluView API to pull flu surveillance data collected from U.S. World Health Organization (WHO) Collaborating Laboratories and National Respiratory and Enteric Virus Surveillance System (NREVSS) laboratories. The data retrieved can be parameterized by geographic granularity and/or flu season. The function is used internally by [get_cdc_clin].
#'
#' @param region One of "`national`", "`hhs`", "`census`", or "`state`"
#' @param years A vector of years to retrieve data for (i.e. `2014` for CDC
#'        flu season 2014-2015). CDC has data for this API going back to 1997.
#'        Default value (`NULL`) means retrieve **all** years. NOTE: if you
#'        specify a 2-digit season value the function will convert that to the corresponding season identifier (i.e. `57` == 2017-2018).
#' @references
#' - [cdcfluview package](https://github.dev/hrbrmstr/cdcfluview)
#'
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

#' Retrieve weekly NHSN flu hospitalization data
#'
#' This function retrieves weekly aggregated NHSN hospital respiratory data API. The function was written to use the default API endpoint (see description of "endpoint" argument and link in references). Note one the available endpoints includes data flagged as "preliminary". All reported weekly aggregates include the number of facilities reporting. In the weeks between April 28, 2024 and November 02, 2024 the NHSN flu hospitalization signal was not required to be reported.
#'
#' @param endpoint URL to data.cdc.gov endpoint; default is `"https://data.cdc.gov/api/views/mpgq-jmmr/rows.csv"` for the preliminary reporting signal
#'
#' @return A `tibble` with the following columns:
#'
#' - **abbreviation**: Abbreviation of the state or US aggregate
#' - **week_end**: End date for the epiweek/epiyear being reported
#' - **flu.admits**: Count of incident flu cases among hospitalized patients
#' - **flu.admits.cov**: Coverage (number of hospitals reporting) for incident flu cases
#' - **flu.admits.cov.perc**: Coverage (percentage of hospitals reporting) for incident flu cases
#'
#' @export
#'
#' @references <https://data.cdc.gov/Public-Health-Surveillance/Weekly-Hospital-Respiratory-Data-HRD-Metrics-by-Ju/mpgq-jmmr/about_data>
#' @references <https://data.cdc.gov/Public-Health-Surveillance/Weekly-Hospital-Respiratory-Data-HRD-Metrics-by-Ju/ua7e-t2fy/about_data>
#'
#'
get_nhsn_weekly <- function(endpoint = "https://data.cdc.gov/api/views/mpgq-jmmr/rows.csv") {

  ## conditionally handle the preliminary endpoint
  if(grepl(pattern = "mpgq-jmmr", endpoint)) {
    dat <-
      ## read from endpoint
      readr::read_csv(endpoint) %>%
      ## NOTE: the mpgq-jmmr has issues with spaces in the names for two columns
      dplyr::rename(`Percent Hospitals Reporting Influenza Admissions` = `Percent Hospitals Reporting  Influenza Admissions`) %>%
      dplyr::rename(`Number Hospitals Reporting Influenza Admissions` = `Number Hospitals Reporting  Influenza Admissions`) %>%
      ## downselect to just a few columns
      dplyr::select(abbreviation = `Geographic aggregation`, week_end = `Week Ending Date`, flu.admits = `Total Influenza Admissions`, flu.admits.cov = `Number Hospitals Reporting Influenza Admissions`, flu.admits.cov.perc = `Percent Hospitals Reporting Influenza Admissions`)
  } else {
    dat <-
      readr::read_csv(endpoint) %>%
      dplyr::select(abbreviation = `Geographic aggregation`, week_end = `Week Ending Date`, flu.admits = `Total Influenza Admissions`, flu.admits.cov = `Number Hospitals Reporting Influenza Admissions`, flu.admits.cov.perc = `Percent Hospitals Reporting Influenza Admissions`)
  }

  dat

}
