
#' Run App
#'
#' Run the app.
#' @export
#' @return NULL
screenviz_app <- function() {
  # Load and tidy the data
  data <- make_dummy_data(2000)
  settings <- get_settings()

  ui <- shiny::navbarPage(
    "Screen Time Visualisation",
    shiny::tabPanel("Overview", mod_heatmap_ui("overview"))
  )

  server <- function(input, output, session) {
    mod_heatmap_server(
      "overview",
      data,
      settings
    )
  }

  shiny::shinyApp(ui, server)
}