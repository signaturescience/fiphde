# Table tab ####
TableUI <- function(id) {
  tagList(
    DT::dataTableOutput(NS(id, "table"))
  )
}

TableServer <- function(id, submission) {
  validate(need(is.reactive(submission), message = "Input submission must be reactive.")) # error message for developers

  moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDataTable({
      DT::datatable(submission()$formatted_data, rownames = F, extensions = 'Buttons', filter = "top", options = list(dom = 'lBrtip', buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download")), lengthMenu = list(c(10,20,-1), c(10,20,"All")), pageLength = 10))

    })
  })
}
