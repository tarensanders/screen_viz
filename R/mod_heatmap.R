mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shinybrowser::detect(),
    shiny::div(
      id = "plotcontainer",
      style = "
        margin: 0;
        position: absolute;
        top: 51%;
        left: 50%;
        -ms-transform: translate(-50%, -50%);
        transform: translate(-50%, -50%);
        width:85vw;
        max-width: 120vmin;
        height: 87.5vh;
        max-height: 50vw;
        /*border: 5px solid red;*/
        ",
      shiny::div(
        shiny::tags$h2(shiny::HTML("<br><br>")),
        style = "
        margin: 0;
        position: relative;
        /*border: 5px solid yellow;*/
        "
      ),
      shiny::div(
        shiny::div(
          style = "display:inline-block; float:left",
          shiny::div(
            style = "display:inline-block",
            title = "Click to reset to the first page",
            shiny::actionButton(ns("reset"), "Reset")
          ),
          shiny::div(
            style = "display:inline-block",
            title = "Click to go back one page",
            shiny::actionButton(ns("prev"), "Previous")
          )
        ),
        shiny::tags$br(),
        shiny::tags$br(),
        echarts4r::echarts4rOutput(ns("heatmap")),
        shiny::tags$br(),
        style = "
        margin: 0;
        position: relative;
        /*border: 5px solid blue;*/
        "
      ),
      shiny::div(
        shiny::tags$h2(shiny::htmlOutput(ns("hover_sentence"))),
        style = "
        margin: 0;
        position: relative;
        /*border: 5px solid yellow;*/
        "
      )
    )
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

      # Pre-checks
      shiny::observeEvent(shinybrowser::get_width(), {
        warn_screen_dims(
          shinybrowser::get_width(),
          shinybrowser::get_height(),
          settings
        )
      })
      shinyjs::hide("reset")
      shinyjs::hide("prev")

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
      prev_state <- shiny::reactiveVal(c())

      # Set observation logic
      shiny::observeEvent(input$xaxis_clicked, {
        shinyjs::show("reset")
        shinyjs::show("prev")
        # TODO: this should be a function
        prev_state_list <- make_prev_state_list(
          clicked$exp, clicked$out, exp_level$current, exp_level$updated,
          out_level$current, out_level$updated
        )
        prev_state(c(prev_state(), list(prev_state_list)))
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
        shinyjs::show("reset")
        shinyjs::show("prev")
        cat(file = stderr(), "Clicked on y axis\n")
        # TODO: this should be a function
        prev_state_list <- make_prev_state_list(
          clicked$exp, clicked$out, exp_level$current, exp_level$updated,
          out_level$current, out_level$updated
        )
        prev_state(c(prev_state(), list(prev_state_list)))
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
        shinyjs::show("reset")
        shinyjs::show("prev")
        # TODO: this should be a function
        if (check_if_last(
          clicked$exp, clicked$out, input$heatmap_clicked_data
        )) {
          # We've hit the last level, show metadata
          output$forestplot <- shiny::renderPlot(make_forest_plot(plot_data()))
          show_metadata(id, plot_data())
        } else {
          prev_state_list <- make_prev_state_list(
            clicked$exp, clicked$out, exp_level$current, exp_level$updated,
            out_level$current, out_level$updated
          )
          prev_state(c(prev_state(), list(prev_state_list)))
        }


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

      output$heatmap <- echarts4r::renderEcharts4r(echarts_heatmap(
        plot_data(),
        curr_outcome(), curr_exposure(), settings
      ))

      shiny::observeEvent(input$reset, {
        shinyjs::hide("reset")
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

      shiny::observeEvent(input$prev, {
        if (length(prev_state()) == 1) {
          shinyjs::hide("prev")
          prev_state(c())
          shinyjs::click("reset")
        } else {
          # Get last state
          prev_state_list <- prev_state()[length(prev_state())][[1]]
          # Remove this state from array
          prev_state(prev_state()[seq_len(length(prev_state()) - 1)])

          clicked$exp <- prev_state_list$clicked_exp
          clicked$out <- prev_state_list$clicked_out
          exp_level$current <- prev_state_list$exp_level_current
          exp_level$updated <- prev_state_list$exp_level_updated
          out_level$current <- prev_state_list$out_level_current
          out_level$updated <- prev_state_list$out_level_updated

          curr_exposure <- update_curr(
            exposures_key, exposure_types, exp_level$current, clicked$exp,
            exp_level$updated
          )
          curr_outcome <- update_curr(
            outcomes_key, outcome_types, out_level$current, clicked$out,
            out_level$updated
          )
        }
      })

      output$hover_sentence <- shiny::renderText(
        shiny::HTML(parse_effect(
          plot_data(),
          input$heatmap_mouseover_data$value[1],
          input$heatmap_mouseover_data$value[2],
          exposure_types[[exp_level$updated]],
          outcome_types[[out_level$updated]],
          settings
        ))
      )
    }
  )
}

mod_heatmap_app <- function() {
  if (!file.exists("data/data.rds")) {
    data <- make_dummy_data(3500)
    saveRDS(data, file = "data/data.rds")
  } else {
    data <- readRDS(file = "data/data.rds")
  }

  settings <- get_settings()
  shiny::addResourcePath("www", system.file("www", package = "screenviz"))

  ui <- mod_heatmap_ui("mod_heatmap")

  server <- function(input, output, session) {
    mod_heatmap_server(
      "mod_heatmap",
      data,
      settings
    )
  }
  shiny::shinyApp(ui, server)
}
