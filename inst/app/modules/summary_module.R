# Summary tab ####
SummaryUI <- function(id) {
  ns <- shiny::NS(id) # can specify NS like this to avoid repeating id, good if you have multiple outputs in tagList
  tagList(
    verbatimTextOutput(ns("horizons")),
    fluidRow(
      column(6, h3("Counts"), tableOutput(ns("counts_summary"))),
      column(6, h3("% Change"), tableOutput(ns("percdiff_summary")))
    )
  )


}

SummaryServer <- function(id, submission, summary_dat, action) {
  validate(need(is.reactive(submission), message = "Input submission must be reactive.")) # error message for developers

  moduleServer(id, function(input, output, session) {

    ## text explaining dates
    output$horizons <- renderText({
      tmp <-
        submission()$data %>%
        dplyr::distinct(target,target_end_date) %>%
        tidyr::separate(target, into = c("horizon", "tmp"), sep = " wk ahead ") %>%
        dplyr::select(-tmp) %>%
        dplyr::distinct() %>%
        dplyr::arrange(horizon) %>%
        dplyr::mutate(frmt = toupper(paste0(horizon, "w ahead: week ending in ", target_end_date)))

      ## get the date for horizon = 1
      ## used to
      h1_date <-
        tmp %>%
        filter(horizon == 1) %>%
        pull(target_end_date)

      prev <-
        tibble(horizon  = "Previous", target_end_date = h1_date - 7) %>%
        mutate(frmt = toupper(paste0(horizon, ": week ending in ", target_end_date)))

      bind_rows(prev, tmp) %>%
        pull(frmt) %>%
        paste0(., collapse = "\n")
    })

    ## summary table counts
    output$counts_summary <- renderTable({
      validate(need(length(action()) == 1, message = "Select only one model."))
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
  })
}






