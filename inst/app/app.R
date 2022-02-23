library(shiny)
library(shinyWidgets)
library(tidyverse)
library(fiphde)

## data dir
## list files in data dir
#data_dir <- .GlobalEnv$.submission_dir
#data_dir <- "C:/Users/sjessa/OneDrive - Signature Science, LLC/Documents/FIPDHE/fiphde/submission"
## note that fps are reversed so that most recent *should* appear first
fps <- rev(list.files(data_dir, pattern = "*.csv$", recursive = TRUE, full.names = TRUE))
## ignore params csv if present
fps <- fps[!grepl("params", fps)]
## find dates
dates <- unique(str_extract(fps, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
#dates <- unique(as.Date(str_extract(fps, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
#                format="%Y-%m-%d"))

# read in ground truth data
# prepped_hosp <-
#   get_hdgov_hosp(limitcols = TRUE) %>%
#   prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
#   dplyr::filter(abbreviation != "DC")
#prepped_hosp <- .GlobalEnv$.data


get_plots <- function(n, ...) {

  plot_output_object <- renderPlot({
    plot_forecast(...)
  },
  height = n*250)

  list(plot_output_object)
}

## helper function to bind all dfs together with filename as key
read_forc <- function(fp) {
  tmp <- read_csv(fp)
  tmp %>%
    mutate(filename = basename(fp)) %>%
    mutate(model = basename(dirname(fp)))
}

ui <- fluidPage(
  titlePanel("FIPHDE Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("forecast", "Select forecast date", choices = dates),
      uiOutput("loc_checkbox"),
      uiOutput("model_checkbox"),
      htmlOutput("valid"),
      tags$br(),
      conditionalPanel(condition = "input.model.length == 1",
                       downloadButton("download")),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", uiOutput("plots")),
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Summary",
                 tabsetPanel(
                   tabPanel("Hospitizations",
                            fluidRow(
                              column(
                                tags$h3("Something"),
                                width = 6),
                              column(
                                tags$h3("Something Else"),
                                width = 6)
                            )
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output) {

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

    submission_summary(.data = prepped_hosp, submission = submission()$data, location = submission()$selected_loc)

  })

  ## reactive engine that drives the bus here ...
  validate_dat <- reactive({

    req(!is.null(submission()))
    req(length(input$model) == 1)

    ## should NOT be valid to have no locations selected
    if(nrow(submission()$data) == 0) {
      "<br><font color=\"#b22222\"><b>FORECAST FILE IS NOT VALID</b></font><br>"
    } else if(validate_forecast(submission()$formatted_data)$valid) {
      "<br><font color=\"#228B22\"><b>FORECAST FILE IS VALID</b></font><br>"
    } else {
      "<br><font color=\"#b22222\"><b>FORECAST FILE IS NOT VALID</b></font><br>"
    }

  })

  output$valid <- renderText({
    req(!is.null(validate_dat()))
    validate_dat()
  })

  ## checkbox to select locations
  ## this is a renderUI option
  output$loc_checkbox <- renderUI({

    ## requires that the original submission file has been read in ...
    req(!is.null(submission_raw()))

    ## get the *names* (not codes) for locations
    locs <-
      fiphde:::locations %>%
      filter(abbreviation %in% c("US", state.abb, "DC")) %>%
      filter(location %in% unique(submission_raw()$data$location))

    ## checkbox choices are *names* (not codes) ... see above
    pickerInput("location","Select location", choices = locs$location_name, selected = locs$location_name, options = list(`actions-box` = TRUE),multiple = T)
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

  ## renders all of the plots (individual renderPlot calls generated as a list by get_plots)
  output$plots <- renderUI({

    ## before trying to render plots make sure that locations are selected
    if(nrow(submission()$data) == 0) {
      HTML("<em>No locations selected.</em>")
    } else {
      ## call get_plots
      ## defined above
      ## effectively wraps fiphde::plot_forecast() ...
      ## submission is reactive data from submission() reactive ...
      ## as is the location
      get_plots(n = length(unique(submission()$data$location)),
                .data = prepped_hosp,
                submission = submission()$data,
                location = submission()$selected_loc)
    }

  })

  ## tabular output
  output$table <- DT::renderDataTable({
    submission()$formatted_data
  })

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

}

# Run the application
shinyApp(ui = ui, server = server)
