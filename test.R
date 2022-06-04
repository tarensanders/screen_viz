
# Display a message that can be dismissed by clicking outside the modal dialog,
# or by pressing Esc.
shinyApp(
  ui = navbarPage(
    "Testing",
    theme = shinythemes::shinytheme("slate"),
    tabPanel(
      "overview",
      fluidPage(
        actionButton("showsa", "Show shinyalert dialog"),
        actionButton("showsm", "Show shiny modal dialog")
      )
    )
  ),
  server = function(input, output) {
    data <- make_dummy_data(2000)
    dataset <- data$data
    dataset %>% dplyr::filter(
      outcome_type == "outcome_specific", exposure_type == "exposure_specific"
    )

    plot_data <-
      dataset %>%
      dplyr::filter(
        outcome == "Body composition" &
          exposure == "Computer use: General" &
          outcome_type == "outcome_specific"
      )

    observeEvent(input$showsa, {
      shinyalert("This is a heading",
        text = tagList(
          plotOutput("plot")
        ),
        html = TRUE
      )
    })

    observeEvent(input$showsm, {
      show_metadata(plot_data)
    })

    output$plot <- renderPlot(plot(mtcars$wt, mtcars$mpg))
  }
)

library(ggforestplot)
library(dplyr)
df <-
  ggforestplot::df_linear_associations %>%
  filter(
    trait == "BMI",
    dplyr::row_number() <= 30
  )

ggforestplot::forestplot(
  df = df,
  name = name,
  estimate = beta,
  se = se
)
