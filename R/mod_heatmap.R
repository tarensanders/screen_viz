mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::titlePanel("Heatmap"),
    echarts4r::echarts4rOutput(ns("heatmap"), height = "40vh"),
    shiny::actionButton(ns("reset"), "Reset"),
    shiny::tags$br(),
    shiny::tags$h2(shiny::htmlOutput(ns("hover_sentence")))
  )
}

mod_heatmap_server <- function(id, data, settings) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # These objects are fixed
      dataset <- data$data
      exposures_key <- data$exp_key
      outcomes_key <- data$out_key
      outcome_types <- names(outcomes_key)
      exposure_types <- names(exposures_key)

      # Instantiate variables
      clicked <- shiny::reactiveValues(exp = NULL, out = NULL)
      exp_level <- shiny::reactiveValues(current = 1, updated = 1)
      out_level <- shiny::reactiveValues(current = 1, updated = 1)
      curr_exposure <- shiny::reactive(update_curr(
        exposures_key, exposure_types, exp_level$current, clicked$exp,
        exp_level$updated
      ))
      curr_outcome <- shiny::reactive(update_curr(
        outcomes_key, outcome_types, out_level$current, clicked$out,
        out_level$updated
      ))

      # Set observation logic
      shiny::observeEvent(input$xaxis_clicked, {
        # TODO: this should be a function
        # update the clicked variable
        clicked$exp <- input$xaxis_clicked
        # Update the level variable
        exp_level$current <- exp_level$updated
        exp_level$updated <- update_level(
          exposure_types, exp_level$current, clicked$exp
        )
        curr_exposure <- update_curr(
          exposures_key, exposure_types, exp_level$current, clicked$exp,
          exp_level$updated
        )
      })

      shiny::observeEvent(input$yaxis_clicked, {
        # TODO: this should be a function
        # update the clicked variable
        clicked$out <- input$yaxis_clicked
        # Update the level variable
        out_level$current <- out_level$updated
        out_level$updated <- update_level(
          outcome_types, out_level$current, clicked$out
        )
        curr_outcome <- update_curr(
          outcomes_key, outcome_types, out_level$current, clicked$out,
          out_level$updated
        )
      })

      shiny::observeEvent(input$heatmap_clicked_data, {
        # TODO: this should be a function
        if (is.null(clicked$exp)) {
          clicked$exp <- input$heatmap_clicked_data$value[1]
          exp_level$current <- exp_level$updated
          exp_level$updated <- update_level(
            exposure_types, exp_level$current, clicked$exp
          )
          curr_exposure <- update_curr(
            exposures_key, exposure_types, exp_level$current, clicked$exp,
            exp_level$updated
          )
        }

        if (is.null(clicked$out)) {
          clicked$out <- input$heatmap_clicked_data$value[2]
          out_level$current <- out_level$updated
          out_level$updated <- update_level(
            outcome_types, out_level$current, clicked$out
          )
          curr_outcome <- update_curr(
            outcomes_key, outcome_types, out_level$current, clicked$out,
            out_level$updated
          )
        }

        if (clicked$exp != input$heatmap_clicked_data$value[1]) {
          clicked$exp <- input$heatmap_clicked_data$value[1]
          exp_level$current <- exp_level$updated
          exp_level$updated <- update_level(
            exposure_types, exp_level$current, clicked$exp
          )
          curr_exposure <- update_curr(
            exposures_key, exposure_types, exp_level$current, clicked$exp,
            exp_level$updated
          )
        }
        if (clicked$out != input$heatmap_clicked_data$value[2]) {
          clicked$out <- input$heatmap_clicked_data$value[2]
          out_level$current <- out_level$updated
          out_level$updated <- update_level(
            outcome_types, out_level$current, clicked$out
          )
          curr_outcome <- update_curr(
            outcomes_key, outcome_types, out_level$current, clicked$out,
            out_level$updated
          )
        }
      })

      plot_data <- shiny::reactive({
        dataset %>%
          dplyr::filter(.data$outcome %in% curr_outcome() &
            .data$exposure %in% curr_exposure())
      })

      output$data <- shiny::renderDataTable(plot_data())

      output$heatmap <- echarts4r::renderEcharts4r(echarts_heatmap(
        plot_data(),
        curr_outcome(), curr_exposure(), settings
      ))

      shiny::observeEvent(input$reset, {
        clicked$exp <- NULL
        clicked$out <- NULL
        exp_level$current <- 1
        exp_level$updated <- 1
        out_level$current <- 1
        out_level$updated <- 1
        curr_exposure <- update_curr(
          exposures_key, exposure_types, exp_level$current, clicked$exp,
          exp_level$updated
        )
        curr_outcome <- update_curr(
          outcomes_key, outcome_types, out_level$current, clicked$out,
          out_level$updated
        )
      })

      output$hover_sentence <- shiny::renderText(
        shiny::HTML(parse_effect(
          plot_data(),
          input$heatmap_mouseover_data$value[1],
          input$heatmap_mouseover_data$value[2],
          settings
        ))
      )
    }
  )
}

mod_heatmap_app <- function() {
  data <- make_dummy_data(2000)

  ui <- mod_heatmap_ui("heatmap_app")

  settings <- get_settings()

  server <- function(input, output, session) {
    mod_heatmap_server(
      "heatmap_app",
      data,
      settings
    )
  }
  shiny::shinyApp(ui, server)
}