
#' Overview Module UI
#'
#' @param id Namespace ID.
#'
#' @return A Shiny UI module.
overview_ui <- function(id) {
  shiny::fluidPage(
    shiny::titlePanel("Overview"),
    shiny::sidebarLayout(
      # Settings
      shiny::sidebarPanel(
        shiny::checkboxInput(
          shiny::NS(id, "subcategories"),
          "Show subcategories",
          FALSE
        ),
        shiny::selectInput(
          shiny::NS(id, "show"),
          "Show units as",
          choices = list("Counts", "Effect Sizes"),
          selected = "Effect Sizes"
        )
      ),
      shiny::mainPanel(
        shiny::plotOutput(shiny::NS(id, "heatmap"))
      )
    )
  )
}



#' Overview Module Server
#'
#' @param id Namespace ID.
#' @param dataset The clean dataset.
#'
#' @return A Shiny UI module.
overview_server <- function(id, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$heatmap <- shiny::renderPlot({
        make_heatmap(dataset(), input$subcategories, input$show)
      })
    }
  )
}



#' Overview Module App
#'
#' For testing purposes only
#'
#' @return NULL.
overview_app <- function() {
  dataset <- load_data()

  ui <- overview_ui("overview")

  server <- function(input, output, session) {
    overview_server("overview", shiny::reactive({
      dataset
    }))
  }

  shiny::shinyApp(ui, server)
}