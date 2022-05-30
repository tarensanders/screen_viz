mod_aboutdata_ui <- function(id) {
  shiny::fluidPage(
    shiny::div(
      id = "container",
      style = "
        margin: 0;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width:75%;
        height:60%;
        */border: 5px solid red;*/
        background-color: #000000;
        color: white
        ",
      fullPage::fullPage(
        center = TRUE,
        fullPage::fullSection(
          shiny::tags$h1("About the Data"),
          shiny::tags$p("(Scroll to continue)"),
          center = TRUE
        ),
        fullPage::fullSection(
          shiny::tags$h2("At some point we can explain the dataset"),
          center = TRUE
        ),
        fullPage::fullSection(
          shiny::tags$h2("And describe how a meta-analysis works"),
          center = TRUE
        ),
        fullPage::fullSectionImage(
          img = "https://images.unsplash.com/photo-1511225317751-5c2d61819d58",
          shiny::tags$h2("We can use fancy graphs and images")
        ),
        fullPage::fullSectionImage(
          img = "https://images.unsplash.com/photo-1615326636736-8227774a2e3f",
          shiny::tags$h2("Mike will probably come up with a metaphor")
        ),
        fullPage::fullSection(
          shiny::tags$h2("But for now you just get this weird slide show"),
          shiny::tags$p(
            paste0("(which is a bit broken", emoji::emoji("shrug"), ")"),
            center = TRUE
          )
        ),
        fullPage::fullSection(shiny::tags$h1(emoji::emoji("wave")),
          center = TRUE
        ),
      )
    )
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
