screenviz_ui <- function(id) {
  shiny::navbarPage(
    "Screen Time Visualisation",
    # shinyWidgets::setBackgroundColor(color = "black"),
    shiny::tabPanel("Overview", mod_heatmap_ui("mod_heatmap")),
    shiny::tabPanel("Forest Plots"),
    shiny::tabPanel("About the Data", mod_aboutdata_ui("aboutdata")),
    shiny::tabPanel("About"),
    theme = shinythemes::shinytheme("slate")
  )
}


#' Run App
#'
#' Run the app.
#' @export
#' @return NULL
screenviz_app <- function() {
  # Load and tidy the data
  data <- make_dummy_data(2000)
  settings <- get_settings()

  ui <- screenviz_ui

  server <- function(input, output, session) {
    mod_heatmap_server("mod_heatmap", data, settings)
    mod_aboutdata_server("aboudata")
  }

  shiny::shinyApp(ui, server)
}