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
read_forc <- function(fp) {
  tmp <- read_csv(fp, col_types = "DcDcccc")
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

submission_summary <- function(.data, submission, location = NULL) {

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

  previous_week <-
    .data %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(location) %>%
    ## restrict to appropriate epiyear/epiweek for week prior to submission
    dplyr::filter(epiyear == previous_ey, epiweek == previous_ew) %>%
    ## add a column for horizon 0 so we can stack on submission data (see below)
    dplyr::mutate(horizon = as.character(0)) %>%
    dplyr::select(horizon, location, flu.admits)


  ## take the submission data ...
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

  ## if US is in there put it on top
  if("US" %in% counts$location) {
    counts <- dplyr::bind_rows(dplyr::filter(counts, location == "US"), dplyr::filter(counts, location !="US"))
  }
  if("US" %in% perc_diff$location) {
    perc_diff <- dplyr::bind_rows(dplyr::filter(perc_diff, location == "US"), dplyr::filter(perc_diff, location !="US"))
  }

  return(list(counts = counts, perc_diff = perc_diff))

}
