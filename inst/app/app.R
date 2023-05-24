library(shiny)
library(shinyWidgets)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)
library(fiphde)
library(plotly)
library(waiter)
library(bslib)

source("~/fiphde/inst/app/global_functions.R")
module_sources = list.files(full.names = TRUE, path = "~/fiphde/inst/app/modules")
sapply(module_sources, source)

## data dir
## list files in data dir
data_dir <- .GlobalEnv$submission_dir
#data_dir <- .GlobalEnv$.submission_dir
prepped_hosp = prepped_hosp
#prepped_hosp <- .GlobalEnv$.data

## note that fps are reversed so that most recent *should* appear first
fps <- rev(list.files(data_dir, pattern = "candidate\\.csv$", recursive = TRUE, full.names = TRUE))
## ignore params csv if present
fps <- fps[!grepl("params", fps)]

## find dates
dates <- unique(stringr::str_extract(fps, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))

# app part ####
ui <- fluidPage(
  includeCSS("~/fiphde/inst/app/style.css"),
  theme = bs_theme(bootswatch = "lux"),
  useWaiter(),
  waiterOnBusy(html = spin_hexdots(), color = transparent(.5)),
  page_navbar(title = "FIPHDE Explorer"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("forecast", "Select forecast date", choices = dates),
      uiOutput("loc_checkbox"),
      uiOutput("model_checkbox"),
      htmlOutput("valid"),
      tags$br(),
      conditionalPanel(condition = "input.model.length == 1",
                       downloadButton("download")),
      tags$br(),
      conditionalPanel(condition = "input.model.length == 1",
                       downloadButton("download_cat", label = "Download (categorical)"))),
    mainPanel(
      tabsetPanel(type = "pills",
        tabPanel("Visualization", PlotUI("first_tab")),
        tabPanel("Table", TableUI("second_tab")),
        tabPanel("Summary", SummaryUI("third_tab"))
        ))
      )
)


server <- function(input, output, session) {
  waiter_hide()

  ## reactive to read in the original submission file
  ## this is reactive because the data will change depending on which input$forecast is supplied
  submission_raw <- reactive({
    ## get the path to the forecast file from the input basename
    tmp_fp <- grep(input$forecast, fps, value = TRUE)
    data <- map_df(tmp_fp, read_forc)
    return(list(data = data))
  })

  ## reactive engine that drives the bus here ...
  submission <- reactive({

    req(!is.null(submission_raw()))

    ## get the *names* (not codes) for locations
    locs <-
      fiphde:::locations %>%
      filter(abbreviation %in% c("US", state.abb, "DC")) %>%
      filter(location %in% unique(submission_raw()$data$location))

    tmp_loc <-
      locs %>%
      filter(location_name %in% input$location) %>%
      pull(location) %>%
      unique(.)

    data <-
      submission_raw()$data %>%
      filter(location %in% tmp_loc) %>%
      filter(model %in% input$model)

    formatted_data <-
      data %>%
      select(-filename,-model)

    ## NOTE: added "selected_loc" because we need to have translated location name ...
    ## otherwise could avoid returning that altogether and just called input$location below
    return(list(data = data, selected_loc = tmp_loc, formatted_data = formatted_data))

  })

  ## reactive engine that drives the bus here ...
  summary_dat <- reactive({
    req(!is.null(submission()))
    req(nrow(submission()$data) > 0)
    req(length(input$model) == 1)
    submission_summary(.data = prepped_hosp, submission = submission()$data, location = submission()$selected_loc)
  })

  ## checkbox to select locations
  ## this is a renderUI option
  output$loc_checkbox <- renderUI({

    ## requires that the original submission file has been read in ...
    req(!is.null(submission_raw()))

    if(length(input$model) == 1) {
      model_locs <-
        submission_raw()$data %>%
        filter(model == input$model) %>%
        pull(location) %>%
        unique(.)

      ## get the *names* (not codes) for locations
      locs <-
        fiphde:::locations %>%
        filter(abbreviation %in% c("US", state.abb, "DC")) %>%
        filter(location %in% model_locs)

    } else {
      ## get the *names* (not codes) for locations
      locs <-
        fiphde:::locations %>%
        filter(abbreviation %in% c("US", state.abb, "DC")) %>%
        filter(location %in% unique(submission_raw()$data$location))
    }
    ## checkbox choices are *names* (not codes) ... see above
    pickerInput("location","Select location", choices = locs$location_name, selected = locs$location_name, options = list(`actions-box` = TRUE), multiple = T)
  })

  output$model_checkbox <- renderUI({

    ## requires that the original submission file has been read in ...
    req(!is.null(submission_raw()))

    ## get the model names
    mods <-
      unique(submission_raw()$data$model)

    ## checkbox choices
    pickerInput("model","Select models", choices = mods, selected = mods, options = list(`actions-box` = TRUE),multiple = T)
  })

  # First tab ####

  PlotServer("first_tab", submission = submission)

  # Second tab ####

  TableServer("second_tab", submission = submission)

  # Third tab ####
  count_in <- reactive(input$model) # to pass the user selected models into the summary_module

  SummaryServer("third_tab", submission = submission, summary_dat = summary_dat, action = count_in)

  ## handler to download the selected data
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$forecast, "-", input$model, ".csv")
    },
    content = function(file) {
      submission()$formatted_data %>%
        dplyr::mutate_all(., as.character) %>%
        readr::write_csv(., file)
    }
  )

  ## handler to download the selected data (categorical targets)
  output$download_cat <- downloadHandler(
    filename = function() {
      paste0(input$forecast, "-", input$model, ".experimental.csv")
    },
    content = function(file) {
      submission()$formatted_data %>%
        forecast_categorical(., prepped_hosp) %>%
        dplyr::mutate_all(., as.character) %>%
        readr::write_csv(., file)
    }
  )
}

shinyApp(ui, server)
