# plot tab ####
PlotUI <- function(id) {
  tagList(
    uiOutput(NS(id, "plots"))
  )
}

PlotServer <- function(id, submission) {
  validate(need(is.reactive(submission), message = "Input submission must be reactive.")) # error message for developers

  moduleServer(id, function(input, output, session) {
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

  })
}
