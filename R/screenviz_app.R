
#' Run App
#'
#' Run the app.
#' @export
#' @return NULL
screenviz_app <- function() {
  # Load and tidy the data
  df_effects <- load_data("Shared/screenviz/combined_effects.rds")
  df_studies <- load_data("Shared/screenviz/studies_converted.rds")
  df_rob <- load_data("Shared/screenviz/rob_raw.rds")

  df_effects <- tidy_data_effects(df_effects)

  ui <- shiny::navbarPage(
    "Screen Time Visualisation",
    shiny::tabPanel("Overview", overview_ui("overview"))
  )

  server <- function(input, output, session) {
    overview_server(
      "overview",
      shiny::reactive({
        df_effects
      })
    )
  }

  shiny::shinyApp(ui, server)
}