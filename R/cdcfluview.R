#' Convert an MMWR year+week or year+week+day to a Date object
#'
#' Copied from [cdcfluview::mmwr_week_to_date].
#' This is a reformat and re-export of a function in the `MMWRweek` package.
#' It provides a snake case version of its counterpart and produces a vector
#' of `Date` objects that corresponds to the input MMWR year+week or year+week+day
#' vectors. This also adds some parameter checking and cleanup to avoid exceptions.
#'
#' @author Bob Rudis
#' @param year,week,day Year, week and month vectors. All must be the same length
#'        unless `day` is `NULL`.
#' @return vector of `Date` objects
#' @export
#' @examples
#' mwd <- mmwr_week_to_date(2016,10,3)
mmwr_week_to_date <- function(year, week, day=NULL) {

  year <- as.numeric(year)
  week <- as.numeric(week)
  day <- if (!is.null(day)) as.numeric(day) else rep(1, length(week))

  week <- ifelse(0 < week & week < 54, week, NA)

  as.Date(ifelse(is.na(week), NA, MMWRweek::MMWRweek2Date(year, week, day)),
          origin="1970-01-01")

}


#' Retrieve ILINet Surveillance Data
#'
#' Copied from [cdcfluview::ilinet].
#' The CDC FluView Portal provides in-season and past seasons' national, regional,
#' and state-level outpatient illness and viral surveillance data from both
#' ILINet (Influenza-like Illness Surveillance Network) and WHO/NREVSS
#' (National Respiratory and Enteric Virus Surveillance System).
#'
#' This function retrieves current and historical ILINet surveillance data for
#' the identified region.
#'
#' @author Bob Rudis
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
#' @export
#' @examples
#' national_ili <- ilinet("national", years = 2017)
#' \dontrun{
#' hhs_ili <- ilinet("hhs")
#' census_ili <- ilinet("census")
#' state_ili <- ilinet("state")
#'
#' all_ili <- suppressWarnings(
#'   suppressMessages(purrr::map_df(c("national", "hhs", "census", "state"), ilinet)))
#' }
ilinet <- function(region = c("national", "hhs", "census", "state"), years = NULL) {

  #region="national"; years=1997:2018

  region <- match.arg(tolower(region), c("national", "hhs", "census", "state"))

  meta <- jsonlite::fromJSON("https://gis.cdc.gov/grasp/flu2/GetPhase02InitApp?appVersion=Public")

  list(
    AppVersion = "Public",
    DatasourceDT = list(list(ID = 1, Name = "ILINet")),
    RegionTypeId = .region_map[region]
  ) -> params

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

  available_seasons <- sort(meta$seasons$seasonid)

  if (is.null(years)) { # ALL YEARS
    years <- available_seasons
  } else { # specified years or seasons or a mix

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

  params$SeasonsDT <- lapply(years, function(i) list(ID = i, Name = as.character(i)))

  tf <- tempfile(fileext = ".zip")
  td <- tempdir()

  on.exit(unlink(tf), TRUE)

  httr::POST(
    url = "https://gis.cdc.gov/grasp/flu2/PostPhase02DataDownload",
    httr::user_agent(.cdcfluview_ua),
    httr::add_headers(
      Origin = "https://gis.cdc.gov",
      Accept = "application/json, text/plain, */*",
      Referer = "https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html"
    ),
    encode = "json",
    body = params,
    # httr::verbose(),
    httr::write_disk(tf)
  ) -> res

  httr::stop_for_status(res)

  nm <- utils::unzip(tf, overwrite = TRUE, exdir = td)

  xdf <- utils::read.csv(nm, skip = 1, stringsAsFactors = FALSE)
  xdf <- .mcga(xdf)

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

  if (region == "national") xdf$region <- "National"
  if (region == "hhs") xdf$region <- factor(xdf$region, levels = sprintf("Region %s", 1:10))

  class(xdf) <- c("tbl_df", "tbl", "data.frame")

  dplyr::arrange(suppressMessages(readr::type_convert(xdf)), week_start)

}
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
to_num <- function(x) {
  x <- gsub("%", "", x, fixed=TRUE)
  x <- gsub(">", "", x, fixed=TRUE)
  x <- gsub("<", "", x, fixed=TRUE)
  x <- gsub(",", "", x, fixed=TRUE)
  x <- gsub(" ", "", x, fixed=TRUE)
  suppressWarnings(as.numeric(x))
}


#' Laboratory-Confirmed Influenza Hospitalizations
#'
#' Copied from [cdcfluview::ilinet].
#'
#' @author Bob Rudis
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
#' @export
#' @examples
#' \dontrun{
#' hosp_fs <- hospitalizations("flusurv", years=2015)
#' hosp_eip <- hospitalizations("eip")
#' hosp_ihsp <- hospitalizations("ihsp")
#' }
hospitalizations <- function(surveillance_area=c("flusurv", "eip", "ihsp"),
                             region="all", years=NULL) {

  sarea <- match.arg(tolower(surveillance_area), choices = c("flusurv", "eip", "ihsp"))
  sarea <- .surv_rev_map[sarea]

  meta <- jsonlite::fromJSON("https://gis.cdc.gov/GRASP/Flu3/GetPhase03InitApp?appVersion=Public")
  areas <- stats::setNames(meta$catchments[,c("networkid", "name", "area", "catchmentid")],
                           c("networkid", "surveillance_area", "region", "id"))

  reg <- region
  if (reg == "all") reg <- "Entire Network"

  tgt <- dplyr::filter(areas, (surveillance_area == sarea) & (region == reg))

  if (nrow(tgt) == 0) {
    stop("Region not found. Use `surveillance_areas()` to see a list of valid inputs.",
         call.=FALSE)
  }

  httr::POST(
    url = "https://gis.cdc.gov/GRASP/Flu3/PostPhase03GetData",
    httr::user_agent(.cdcfluview_ua),
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
    # httr::verbose(),
    httr::timeout(.httr_timeout)
  ) -> res

  httr::stop_for_status(res)

  res <- httr::content(res)

  hosp <- list(res = res, meta = meta)

  age_df <- stats::setNames(hosp$meta$ages, c("age_label", "age", "color"))
  age_df <- age_df[,c("age", "age_label")]

  sea_df <- stats::setNames(
    hosp$meta$seasons,
    c("sea_description", "sea_endweek", "sea_label", "seasonid", "sea_startweek", "color", "color_hexvalue")
  )
  sea_df <- sea_df[,c("seasonid", "sea_label", "sea_description", "sea_startweek", "sea_endweek")]

  ser_names <- unlist(hosp$res$busdata$datafields, use.names = FALSE)

  suppressWarnings(
    suppressMessages(
      mmwr_df <- dplyr::bind_rows(hosp$res$mmwr)
    )
  )

  mmwr_df <- mmwr_df[,c("mmwrid", "weekend", "weeknumber", "weekstart", "year",
                        "yearweek", "seasonid", "weekendlabel", "weekendlabel2")]

  suppressMessages(
    suppressWarnings(

      dplyr::bind_rows(
        lapply(hosp$res$busdata$dataseries, function(.x) {

          dplyr::bind_rows(
            lapply(.x$data, function(.x) stats::setNames(.x, ser_names))
          ) -> tdf

          tdf$age <- .x$age
          tdf$season <- .x$season

          tdf

        })
      ) -> xdf

    )
  )

  if (length(unique(xdf$age)) > 9) {
    data.frame(
      age = 1:12,
      age_label = c("0-4 yr", "5-17 yr", "18-49 yr", "50-64 yr", "65+ yr", "Overall",
                    "65-74 yr", "75-84 yr", "85+", "18-29 yr", "30-39 yr", "40-49 yr"
      )
    ) -> age_df
    age_df$age_label <- factor(age_df$age_label, levels = age_df$age_label)
  }

  dplyr::left_join(xdf, mmwr_df, c("mmwrid", "weeknumber")) %>%
    dplyr::left_join(age_df, "age") %>%
    dplyr::left_join(sea_df, "seasonid") %>%
    dplyr::mutate(
      surveillance_area = sarea,
      region = reg
    ) %>%
    dplyr::left_join(mmwrid_map, "mmwrid") -> xdf

  xdf <- xdf[,c("surveillance_area", "region", "year", "season", "wk_start", "wk_end",
                "year_wk_num", "rate", "weeklyrate", "age", "age_label", "sea_label",
                "sea_description", "mmwrid")]

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

#' Retrieve a list of valid sub-regions for each surveillance area.
#'
#' @md
#' @export
#' @examples
#' sa <- surveillance_areas()
surveillance_areas <- function() {
  meta <- jsonlite::fromJSON("https://gis.cdc.gov/GRASP/Flu3/GetPhase03InitApp?appVersion=Public")
  xdf <- stats::setNames(meta$catchments[,c("name", "area")], c("surveillance_area", "region"))
  xdf$surveillance_area <- .surv_map[xdf$surveillance_area]
  xdf
}

