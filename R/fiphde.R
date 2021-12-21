#' \code{fiphde} package
#'
#' Forecasting Influenza in Support of Public Health Decision Making
#'
#' @docType package
#' @name fiphde
NULL

## quiets concerns of R CMD check re: the non-bound global variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "previous_day_admission_influenza_confirmed",
                                                        "previous_day_admission_influenza_confirmed_coverage",
                                                        "previous_day_deaths_influenza",
                                                        "previous_day_deaths_influenza_coverage",
                                                        "icu_patients_confirmed_influenza",
                                                        "icu_patients_confirmed_influenza_coverage",
                                                        "total_patients_hospitalized_confirmed_influenza",
                                                        "total_patients_hospitalized_confirmed_influenza_coverage",
                                                        "previous_day_admission_adult_covid_confirmed",
                                                        "previous_day_admission_adult_covid_confirmed_coverage",
                                                        "deaths_covid",
                                                        "deaths_covid_coverage",
                                                        "cov.deaths.cov",
                                                        "current_through",
                                                        "week",
                                                        "cumulative_flu_doses",
                                                        "month",
                                                        "y1",
                                                        "y2",
                                                        "year",
                                                        "month_calendar_number",
                                                        "month_number",
                                                        "epiyear",
                                                        "epiweek",
                                                        "imputed_value",
                                                        "region_type",
                                                        "week_start",
                                                        "ilitotal",
                                                        "total_patients",
                                                        "location",
                                                        "abbreviation",
                                                        "age_label",
                                                        "year_wk_num",
                                                        "wk_start",
                                                        "wk_end",
                                                        "rate",
                                                        "weeklyrate",
                                                        "sea_label",
                                                        "monday",
                                                        "yweek",
                                                        "."))

