screenviz_app <- function() {
  # Load and tidy the data
  dataset <- load_data()

  ui <- shiny::navbarPage(
    "Screen Time Visualisation",
    shiny::tabPanel("Overview", overview_ui("overview"))
  )

  server <- function(input, output, session) {
    overview_server(
      "overview",
      shiny::reactive({
        dataset
      })
    )
  }

  shiny::shinyApp(ui, server)
}