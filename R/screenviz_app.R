screenviz_ui <- function(id) {
  backgroundimgcss <- "/* background-color: #cccccc; */
                       height: 91vh;
                       background-position: center;
                       background-repeat: no-repeat;
                       background-size: cover;
                       background-image: url('%s');
                       text-align: center;
                       "
  shiny::navbarPage(
    "ScreenViz",
    shiny::tabPanel(
      "Overview", mod_heatmap_ui("mod_heatmap"),
      style = sprintf(backgroundimgcss, "www/img/Tablet-1600x1600.png")
    ),
    shiny::navbarMenu(
      "In Development",
      shiny::tabPanel("Forest Plots"),
      shiny::tabPanel(
        "About the Data", mod_aboutdata_ui("aboutdata"),
        style = sprintf(backgroundimgcss, "www/img/Tablet-1600x1600.png")
      ),
      shiny::tabPanel("About",
        style = sprintf(backgroundimgcss, "www/img/TV.jpg"),
        shiny::div(
          id = "container",
          style = "
        margin: 0;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width:50%;
        height:30%;
        /*border: 5px solid red;*/
        text-align: center;
        ",
          shiny::tags$h2(
            shiny::HTML(paste0(
              "Built with<br>",
              emoji::emoji("coffee"), "<br>",
              "by ",
              shiny::tags$a(
                href = "https://github.com/tarensanders", "@TarenSanders"
              )
            ))
          )
        )
      )
    ),
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
  shiny::addResourcePath("www", system.file("www", package = "screenviz"))

  ui <- screenviz_ui

  server <- function(input, output, session) {
    mod_heatmap_server("mod_heatmap", data, settings)
    mod_aboutdata_server("aboudata")
  }

  shiny::shinyApp(ui, server)
}
