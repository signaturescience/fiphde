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
library(rplanes)

## data dir
## list files in data dir
data_dir <- .GlobalEnv$.submission_dir
prepped_hosp <- .GlobalEnv$.data

## note that fps are reversed so that most recent *should* appear first
fps <- rev(list.files(data_dir, pattern = "candidate\\.csv$", recursive = TRUE, full.names = TRUE))
## ignore params csv if present
fps <- fps[!grepl("params", fps)]

## find dates
dates <- unique(stringr::str_extract(fps, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))

get_plots <- function(n, ...) {

  # create plotly object
  plot_output_object <- renderPlotly({
    gp <- ggplotly(plot_forecast(...), height = n*250)

    ## Unify legend names
    # Get the names of the legend entries
    leg_names <- data.frame(id = seq_along(gp$x$data), legend_entries = unlist(lapply(gp$x$data, `[[`, "name")))
    # Extract the group identifier (ie. "observed", "SigSci-CREG", etc.)
    leg_names$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", leg_names$legend_entries)
    leg_names$first <- !duplicated(leg_names$legend_group)

    for (i in leg_names$id) {
      first <- leg_names$first[[i]]
      # Assign the group identifier to the name and legend_group arguments
      gp$x$data[[i]]$name <- leg_names$legend_group[[i]]
      gp$x$data[[i]]$legendgroup <- gp$x$data[[i]]$name
      # Show the legend only once
      if (!first) gp$x$data[[i]]$showlegend <- FALSE
    }
    gp
  })

  list(plot_output_object)
}

## helper function to bind all dfs together with filename as key
read_forc <- function(fp, format) {

  if(format == "legacy") {
    tmp <- readr::read_csv(fp, col_types = "DcDcccc")
  } else if (format == "hubverse") {
    tmp <- readr::read_csv(fp, col_types = "DccDccc")
  }
  tmp %>%
    mutate(filename = basename(fp)) %>%
    mutate(model = basename(dirname(fp)))
}

## helpers for submission summary
spread_value <- function(.data, ...) {

  ## quietly ...
  suppressMessages({
    tmp <-
      ## spread the data
      tidyr::spread(.data, ...) %>%
      ## then get the location names
      dplyr::left_join(dplyr::select(fiphde:::locations, location, location_name)) %>%
      dplyr::select(-location)
  })

  ## one more piece of logic to get "Previous" column before w ahead columns if need be
  if("Previous" %in% names(tmp)) {
    tmp <-
      tmp %>%
      dplyr::select(location = location_name, Previous, dplyr::everything())
  } else {
    tmp <-
      tmp %>%
      dplyr::select(location = location_name, dplyr::everything())
  }
}

submission_summary <- function(.data, submission, location = NULL, format) {

  ## force submission value to be numeric
  submission$value <- as.numeric(submission$value)

  if(!is.null(location)) {
    loc_name <- location
    submission <-
      submission %>%
      dplyr::filter(location %in% loc_name)
  }

  ## get epiweek and epiyear for week before based on submission data
  ## this will be used find event count to determine 1wk horizon % change
  submission_ew <- min(lubridate::epiweek(submission$target_end_date))
  submission_ey <- min(lubridate::epiyear(submission$target_end_date))

  previous_ew <- ifelse(submission_ew == 1, 53, submission_ew - 1)
  previous_ey <- ifelse(submission_ew == 1, submission_ey - 1, submission_ey)

  if(format == "legacy") {
    previous_week <-
      .data %>%
      dplyr::as_tibble() %>%
      dplyr::group_by(location) %>%
      ## restrict to appropriate epiyear/epiweek for week prior to submission
      dplyr::filter(epiyear == previous_ey, epiweek == previous_ew) %>%
      ## add a column for horizon 0 so we can stack on submission data (see below)
      dplyr::mutate(horizon = as.character(0)) %>%
      dplyr::select(horizon, location, flu.admits)
  } else if (format == "hubverse") {
    previous_week <-
      .data %>%
      dplyr::as_tibble() %>%
      dplyr::group_by(location) %>%
      ## restrict to appropriate epiyear/epiweek for week prior to submission
      dplyr::filter(epiyear == previous_ey, epiweek == previous_ew) %>%
      ## add a column for horizon 0 so we can stack on submission data (see below)
      dplyr::mutate(horizon = as.character(-1)) %>%
      dplyr::select(horizon, location, flu.admits)
  }


  if (format == "legacy") {
    ## take the submission data ..
    tmp_counts <-
      submission %>%
      ## restrict to point estimates
      dplyr::filter(type == "point") %>%
      ## only need target value and location columns
      dplyr::select(target, value, location) %>%
      ## string manip to get the horizon and target name separated
      tidyr::separate(., target, into = c("horizon", "target"), sep = "wk ahead") %>%
      dplyr::mutate(horizon = stringr::str_trim(horizon, "both"),
                    target = stringr::str_trim(target, "both")) %>%
      ## clean up taret name
      dplyr::mutate(target =
                      dplyr::case_when(
                        target == "inc flu hosp" ~ "flu.admits")) %>%
      ## reshape wide
      tidyr::spread(target, value) %>%
      ## stack on top of the "previous week" data
      dplyr::bind_rows(previous_week) %>%
      ## must sort by horizon and location so that window lag function below will work
      dplyr::arrange(horizon, location) %>%
      ## reshape long again
      tidyr::gather(target, value, flu.admits)

    ## formatting for percentage difference
    tmp_perc_diff <-
      tmp_counts %>%
      ## need to do the window function stuff by unique combo of location and target
      dplyr::group_by(location, target) %>%
      ## figure out the % change
      dplyr::mutate(diff = value / dplyr::lag(value)) %>%
      ## drop the horizon 0 (previous week) since we don't need it any more
      dplyr::filter(horizon != 0) %>%
      dplyr::mutate(diff = ifelse(diff < 1, -1*abs(1-diff), abs(1-diff))) %>%
      dplyr::mutate(diff = diff*100) %>%
      dplyr::mutate(diff = paste0(as.character(round(diff, 1)), "%")) %>%
      dplyr::select(-value) %>%
      dplyr::mutate(horizon = ifelse(horizon == 0, "Previous", paste0(horizon, "w ahead"))) %>%
      dplyr::group_by(target)


    ## get names for each target from group keys
    ## used to name the list below ...
    target_names <-
      tmp_perc_diff %>%
      dplyr::group_keys() %>%
      dplyr::pull(target)

    perc_diff <-
      tmp_perc_diff %>%
      dplyr::group_split(., .keep = FALSE) %>%
      purrr::map(., .f = function(x) spread_value(x, horizon, diff)) %>%
      purrr::set_names(target_names)

    ## formatting for counts
    tmp_counts <-
      tmp_counts %>%
      dplyr::mutate(horizon = ifelse(horizon == 0, "Previous", paste0(horizon, "w ahead"))) %>%
      dplyr::group_by(target)

    target_names <-
      tmp_counts %>%
      dplyr::group_keys() %>%
      dplyr::pull(target)

    counts <-
      tmp_counts %>%
      dplyr::group_split(., .keep = FALSE) %>%
      purrr::map(., .f = function(x) spread_value(x, horizon, value)) %>%
      purrr::set_names(target_names)
  } else if (format == "hubverse") {
    ## take the submission data ..
    tmp_counts <-
      submission %>%
      ## restrict to point estimates
      dplyr::filter(output_type_id == "0.5") %>%
      ## only need target value and location columns
      dplyr::select(target, horizon, value, location) %>%
      ## clean up target name
      dplyr::mutate(target =
                      dplyr::case_when(
                        target == "wk inc flu hosp" ~ "flu.admits")) %>%
      ## reshape wide
      tidyr::spread(target, value) %>%
      ## stack on top of the "previous week" data
      dplyr::bind_rows(previous_week) %>%
      ## must sort by horizon and location so that window lag function below will work
      dplyr::arrange(horizon, location) %>%
      ## reshape long again
      tidyr::gather(target, value, flu.admits)

    ## formatting for percentage difference
    tmp_perc_diff <-
      tmp_counts %>%
      ## need to do the window function stuff by unique combo of location and target
      dplyr::group_by(location, target) %>%
      ## figure out the % change
      dplyr::mutate(diff = value / dplyr::lag(value)) %>%
      ## drop the horizon 0 (previous week) since we don't need it any more
      dplyr::filter(horizon != -1) %>%
      dplyr::mutate(diff = ifelse(diff < 1, -1*abs(1-diff), abs(1-diff))) %>%
      dplyr::mutate(diff = diff*100) %>%
      dplyr::mutate(diff = paste0(as.character(round(diff, 1)), "%")) %>%
      dplyr::select(-value) %>%
      dplyr::mutate(horizon = ifelse(horizon == -1, "Previous", paste0(horizon, "w ahead"))) %>%
      dplyr::group_by(target)


    ## get names for each target from group keys
    ## used to name the list below ...
    target_names <-
      tmp_perc_diff %>%
      dplyr::group_keys() %>%
      dplyr::pull(target)

    perc_diff <-
      tmp_perc_diff %>%
      dplyr::group_split(., .keep = FALSE) %>%
      purrr::map(., .f = function(x) spread_value(x, horizon, diff)) %>%
      purrr::set_names(target_names)

    ## formatting for counts
    tmp_counts <-
      tmp_counts %>%
      dplyr::mutate(horizon = ifelse(horizon == -1, "Previous", paste0(horizon, "w ahead"))) %>%
      dplyr::group_by(target)

    target_names <-
      tmp_counts %>%
      dplyr::group_keys() %>%
      dplyr::pull(target)

    counts <-
      tmp_counts %>%
      dplyr::group_split(., .keep = FALSE) %>%
      purrr::map(., .f = function(x) spread_value(x, horizon, value)) %>%
      purrr::set_names(target_names)
  }


  ## if US is in there put it on top
  if("US" %in% counts$location) {
    counts <- dplyr::bind_rows(dplyr::filter(counts, location == "US"), dplyr::filter(counts, location !="US"))
  }
  if("US" %in% perc_diff$location) {
    perc_diff <- dplyr::bind_rows(dplyr::filter(perc_diff, location == "US"), dplyr::filter(perc_diff, location !="US"))
  }

  return(list(counts = counts, perc_diff = perc_diff))

}

## UI Side ##

ui <- fluidPage(
  useWaiter(),
  waiterOnBusy(html = spin_whirly(), color = transparent(.5)),
  titlePanel("FIPHDE Explorer"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("format", label = "Format", choices = c("Hubverse" = "hubverse","Legacy" = "legacy"), selected = "hubverse"),
      selectInput("forecast", "Select forecast date", choices = dates),
      uiOutput("loc_checkbox"),
      uiOutput("model_checkbox"),
      tags$br(),
      conditionalPanel(condition = "input.model.length == 1",
                       downloadButton("download")),
      tags$br(),
      conditionalPanel(condition = "input.model.length == 1 & input.format == 'legacy'",
                       downloadButton("download_cat", label = "Download (categorical)")),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", uiOutput("plots")),
        tabPanel("Plausibility",
                 tags$h3("Plausibility analysis with PLANES only available if one model is selected."),
                 selectInput("planes_components", "PLANES Components",choices = c("diff","cover","taper","trend","repeat","zero","shape"), selected = c("diff","cover","taper","trend","repeat","zero"), multiple = TRUE),
                 actionButton("run_planes","Run PLANES scoring"),
                 tags$br(),
                 DT::dataTableOutput("planes")),
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Summary",
                 tags$h3("Summary data only available if one model is selected."),
                 verbatimTextOutput("horizons"),
                 fluidRow(
                   column(
                     tags$h3("Counts"),
                     tableOutput("counts_summary"),
                     width = 6),
                   column(
                     tags$h3("% Change"),
                     tableOutput("percdiff_summary"),
                     width = 6)))
      ))
  )
)


server <- function(input, output) {
  waiter_hide()

  ## reactive to read in the original submission file
  ## this is reactive because the data will change depending on which input$forecast is supplied
  submission_raw <- reactive({
    ## get the path to the forecast file from the input basename
    tmp_fp <- grep(input$forecast, fps, value = TRUE)
    data <- map_df(tmp_fp, read_forc, format = input$format)
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
    submission_summary(.data = prepped_hosp, submission = submission()$data, location = submission()$selected_loc, format = input$format)
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
    pickerInput("location","Select location", choices = locs$location_name, selected = locs$location_name, options = list(`actions-box` = TRUE, `live-search` = TRUE),multiple = T)
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
                location = submission()$selected_loc,
                format = input$format)
    }

  })

  planes_dat <- eventReactive(input$run_planes, {

    req(length(input$model) == 1)

    ## prep signal objects for plausibility analysis
    ## need to find the forecast file selected first ...
    ## possible that there are multiple models with the same "forecast" date
    ## using two steps
    tmp_fp <- grep(input$forecast, fps, value = TRUE)
    tmp_fp <- grep(input$model, tmp_fp, value = TRUE)
    forc <- rplanes::read_forecast(tmp_fp, format = input$format)
    forc_signal <- to_signal(forc, outcome = "flu.admits", type = "forecast", resolution = "weeks", horizon = 4)

    obs_dat <-
      prepped_hosp %>%
      select(location, date = week_start, flu.admits)

    obs_signal <- to_signal(obs_dat, outcome = "flu.admits", type = "observed", resolution = "weeks")

    ## create the seed based on the saturday before the oldest date in the forecast
    ## forecast date column will be the saturday based on the target end date
    prepped_seed <- plane_seed(obs_signal, cut_date = as.Date(min(forc$date)-7))

    scores <- plane_score(forc_signal, prepped_seed, components = input$planes_components)

    res <-
      scores$scores_summary %>%
      map_df(., as_tibble)

    return(list(res = res))
  })

  output$planes <- DT::renderDataTable({

    req(length(input$model) == 1)

    ## return scores
    ## make sure to join to fiphde locations for location names
    planes_dat()$res %>%
      left_join(fiphde:::locations) %>%
      select(-abbreviation, -population, -location) %>%
      rename(location = location_name) %>%
      select(location, everything())

  })
  ## tabular output
  output$table <- DT::renderDataTable({
    submission()$formatted_data
  })

  ## text explaining dates
  output$horizons <- renderText({

    if(input$format == "legacy") {
      tmp <-
        submission()$data %>%
        dplyr::distinct(target,target_end_date) %>%
        tidyr::separate(target, into = c("horizon", "tmp"), sep = " wk ahead ") %>%
        dplyr::select(-tmp) %>%
        dplyr::distinct() %>%
        dplyr::arrange(horizon) %>%
        dplyr::mutate(frmt = toupper(paste0(horizon, "w ahead: week ending in ", target_end_date)))

      h1_date <-
        tmp %>%
        filter(horizon == 1) %>%
        pull(target_end_date)

    } else if (input$format == "hubverse") {
      tmp <-
        submission()$data %>%
        dplyr::distinct(horizon,target_end_date) %>%
        dplyr::arrange(horizon) %>%
        dplyr::mutate(frmt = toupper(paste0(horizon, "w ahead: week ending in ", target_end_date)))
      h1_date <-
        tmp %>%
        filter(horizon == 0) %>%
        pull(target_end_date)

    }

    prev <-
      tibble(horizon  = "Previous", target_end_date = h1_date - 7) %>%
      mutate(frmt = toupper(paste0(horizon, ": week ending in ", target_end_date)))

    bind_rows(prev, tmp) %>%
      pull(frmt) %>%
      paste0(., collapse = "\n")
  })


  ## summary table counts
  output$counts_summary <- renderTable({
    x <- summary_dat()$counts$flu.admits
    x <- x[complete.cases(x),]
    names(x) <- gsub(" ahead", "", names(x))
    names(x) <- toupper(names(x))
    x
  },
  digits = 0,
  bordered = TRUE)

  ## summary table perc change
  output$percdiff_summary <- renderTable({
    x <- summary_dat()$perc_diff$flu.admits
    x <- x[complete.cases(x),]
    names(x) <- gsub(" ahead", "", names(x))
    names(x) <- toupper(names(x))
    x
  },
  digits = 0,
  bordered = TRUE)

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

# Run the application
shinyApp(ui = ui, server = server)
