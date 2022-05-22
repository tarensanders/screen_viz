mod_heatmap_ui <- function(id) {
  shiny::fluidPage(
    shiny::titlePanel("Heatmap"),
    echarts4r::echarts4rOutput(shiny::NS(id, "heatmap"), height = "40vh"),
    shiny::textOutput(shiny::NS(id, "clicked_data")),
    shiny::textOutput(shiny::NS(id, "clicked_data_value")),
    shiny::textOutput(shiny::NS(id, "clicked_row")),
    shiny::textOutput(shiny::NS(id, "xaxis_clicked")),
    shiny::textOutput(shiny::NS(id, "yaxis_clicked")),
  )
}

mod_heatmap_server <- function(id, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$heatmap <-
        echarts4r::renderEcharts4r({
          dataset() %>%
            dplyr::mutate(
              general_exposure = forcats::as_factor(.data$general_exposure),
              general_outcome = forcats::as_factor(.data$general_outcome)
            ) %>%
            dplyr::group_by(
              .data$general_exposure, .data$general_outcome,
              .drop = FALSE
            ) %>%
            dplyr::summarise(r = mean(.data$r)) %>%
            dplyr::ungroup() %>%
            echarts4r::e_chart(.data$general_exposure) %>%
            echarts4r::e_heatmap(.data$general_outcome, .data$r,
              itemStyle = list(emphasis = list(shadowBlur = 10))
            ) %>%
            echarts4r::e_visual_map(
              min = -0.8, max = 0.8, precision = 2,
              inRange = list(color = c("#0ee732", "#ffffff", "#990000"))
            ) %>%
            echarts4r::e_x_axis(
              axisLabel = list(
                interval = 0,
                rotate = 30
              ),
              triggerEvent = TRUE
            ) %>%
            echarts4r::e_y_axis(triggerEvent = TRUE) %>%
            echarts4r::e_grid(bottom = "20%") %>%
            echarts4r::e_on(
              "xAxis.category",
              "function(params){
               Shiny.setInputValue('heatmap_app-xaxis_clicked',
                                    params.value, {priority: 'event'});
               console.log(params.value);
              }"
            ) %>%
            echarts4r::e_on(
              "yAxis.category",
              "function(params){
               Shiny.setInputValue('heatmap_app-yaxis_clicked',
                                    params.value, {priority: 'event'});
               console.log(params.value);
              }"
            )
        })

      output$clicked_data <- shiny::renderText(paste(
        "clicked_data:",
        input$heatmap_clicked_data
      ))
      output$clicked_data_value <- shiny::renderText(paste(
        "clicked_data_value:",
        input$heatmap_clicked_data_value
      ))
      output$clicked_row <- shiny::renderText(paste(
        "clicked_row:",
        input$heatmap_clicked_row
      ))
      output$xaxis_clicked <- shiny::renderText(paste(
        "xaxis_clicked:",
        input$xaxis_clicked
      ))
      output$yaxis_clicked <- shiny::renderText(paste(
        "xaxis_clicked:",
        input$yaxis_clicked
      ))


      # TODO: Implement these sections
      not_implemented_err <- function(clicked) {
        shinyalert::shinyalert(
          title = "Not Implemented",
          text = paste(
            "You clicked on <b>", clicked, "</b>.<br>",
            "In the future this will take you to the exposures/outcomes you",
            "selected, but this is not yet implemented."
          ),
          html = TRUE, type = "error"
        )
      }

      shiny::observeEvent(
        input$xaxis_clicked,
        not_implemented_err(input$xaxis_clicked)
      )
      shiny::observeEvent(
        input$yaxis_clicked,
        not_implemented_err(input$yaxis_clicked)
      )
    }
  )
}

mod_heatmap_app <- function() {
  df_effects <- load_data("Shared/screenviz/combined_effects.rds")
  df_effects <- tidy_data_effects(df_effects)

  ui <- mod_heatmap_ui("heatmap_app")

  server <- function(input, output, session) {
    mod_heatmap_server(
      "heatmap_app",
      shiny::reactive({
        df_effects
      })
    )
  }
  shiny::shinyApp(ui, server)
}