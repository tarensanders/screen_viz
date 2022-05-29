mod_aboutdata_ui <- function(id) {
  shiny::fluidPage(
    
  )
}

mod_aboutdata_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

mod_aboutdata_app <- function() {
  ui <- mod_aboutdata_ui("aboutdata")
  server <- function(input, output, session) {
    mod_aboutdata_server("aboutdata")
  }

  shiny::shinyApp(ui, server)
}
