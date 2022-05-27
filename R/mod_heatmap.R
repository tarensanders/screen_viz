mod_heatmap_ui <- function(id) {
  shiny::fluidPage(
    shiny::titlePanel("Heatmap"),
    echarts4r::echarts4rOutput(shiny::NS(id, "heatmap"), height = "40vh"),
    shiny::tags$b("Interaction:"),
    shiny::textOutput(shiny::NS(id, "clicked_data")),
    shiny::textOutput(shiny::NS(id, "xaxis_clicked")),
    shiny::textOutput(shiny::NS(id, "yaxis_clicked")),
    shiny::tags$br(),
    shiny::tags$b("Exposure:"),
    shiny::textOutput(shiny::NS(id, "curr_exposure")),
    shiny::textOutput(shiny::NS(id, "clicked_exp")),
    shiny::textOutput(shiny::NS(id, "exp_level")),
    shiny::tags$br(),
    shiny::tags$b("Outcome:"),
    shiny::textOutput(shiny::NS(id, "curr_outcome")),
    shiny::textOutput(shiny::NS(id, "clicked_out")),
    shiny::textOutput(shiny::NS(id, "out_level")),
    shiny::tags$b("Data:"),
    shiny::dataTableOutput(shiny::NS(id, "data"))
  )
}

mod_heatmap_server <- function(id, data) {
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

      output$curr_outcome <- shiny::renderText(paste(
        "curr_outcome:",
        paste(curr_outcome(), collapse = ", ")
      ))

      output$curr_exposure <- shiny::renderText(paste(
        "curr_exposure:",
        paste(curr_exposure(), collapse = ", ")
      ))

      output$clicked_exp <- shiny::renderText(paste(
        "clicked_exp:",
        clicked$exp
      ))
      output$clicked_out <- shiny::renderText(paste(
        "clicked_out:",
        clicked$out
      ))

      output$exp_level <- shiny::renderText(paste(
        "exp_level:",
        "current:", exp_level$current,
        "updated:", exp_level$updated
      ))
      output$out_level <- shiny::renderText(paste(
        "out_level:",
        "current:", out_level$current,
        "updated:", out_level$updated
      ))
    }
  )
}

mod_heatmap_server_old <- function(id, data) {
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

      # curr_outcome <- shiny::reactive({
      #   update_curr(
      #     outcomes_key, outcome_types, out_level$current, clicked$out,
      #     out_level$updated
      #   )
      # })
      # curr_exposure <- shiny::reactive({
      #   update_curr(
      #     exposures_key, exposure_types, exp_level$current, clicked$exp,
      #     exp_level$updated
      #   )
      # })

      plot_data <- shiny::reactive({
        dataset %>%
          dplyr::filter(.data$outcome %in% curr_outcome() &
            .data$exposure %in% curr_exposure())
      })

      output$data <- shiny::renderDataTable(plot_data())

      output$heatmap <- echarts_heatmap(
        plot_data(),
        curr_outcome(), curr_exposure()
      )

      output$curr_outcome <- shiny::renderText(paste(
        "curr_outcome:",
        paste(curr_outcome(), collapse = ", ")
      ))

      output$curr_exposure <- shiny::renderText(paste(
        "curr_exposure:",
        paste(curr_exposure(), collapse = ", ")
      ))

      output$clicked_data <- shiny::renderText(paste(
        "clicked_data:",
        input$heatmap_clicked_data
      ))
      output$xaxis_clicked <- shiny::renderText(paste(
        "xaxis_clicked:",
        input$xaxis_clicked
      ))
      output$yaxis_clicked <- shiny::renderText(paste(
        "yaxis_clicked:",
        input$yaxis_clicked
      ))

      output$clicked_exp <- shiny::renderText(paste(
        "clicked_exp:",
        clicked$exp
      ))
      output$clicked_out <- shiny::renderText(paste(
        "clicked_out:",
        clicked$out
      ))

      output$exp_level <- shiny::renderText(paste(
        "exp_level:",
        "current:", exp_level$current,
        "updated:", exp_level$updated
      ))
      output$out_level <- shiny::renderText(paste(
        "out_level:",
        "current:", out_level$current,
        "updated:", out_level$updated
      ))
    }
  )
}

mod_heatmap_app <- function() {
  data <- make_dummy_data(2000)

  ui <- mod_heatmap_ui("heatmap_app")

  server <- function(input, output, session) {
    mod_heatmap_server(
      "heatmap_app",
      data
    )
  }
  shiny::shinyApp(ui, server)
}