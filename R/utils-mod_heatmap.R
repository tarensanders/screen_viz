
#' Update level
#'
#' Update the level of the current exposure and outcome.
#'
#' @param types The list of possible types the exposure or outcome can take.
#' @param level The current level as an int.
#' @param clicked The value that was clicked, or NULL if nothing was clicked.
#'
#' @return The updated level value as an int.
#' @keywords internal
update_level <- function(types, level, clicked) {
  if (is.null(clicked)) {
    return(level)
  }

  if (level < length(types)) {
    return(level + 1)
  } else {
    return(level)
  }
}


#' Update the current list of exposures or outcomes.
#'
#' Takes information about the current events and uses it to determine the
#' next list of exposures or outcomes that should be used.
#'
#' @param key The exposures or outcomes 'key'. \code{\link{make_dummy_data}}
#' provides an example of the expected key format.
#' @param types A list of exposure types, where the names match the column
#' names of the key.
#' @param level The current level, as an int.
#' @param clicked The value that was clicked, or \code{NULL} if nothing was
#' clicked.
#' @param clicked_level The level the click occured, where applicable.
#'
#' @return A list of exposures or outcomes.
#' @keywords internal
update_curr <- function(key, types, level, clicked, clicked_level = NULL) {
  if (is.null(clicked)) {
    out <-
      key %>%
      dplyr::distinct(.data[[types[[level]]]]) %>%
      dplyr::pull()
    return(out)
  }
  # Remove any string wrapping
  clicked <- stringr::str_replace_all(clicked, "\n", " ")
  out <-
    key %>%
    dplyr::filter(.data[[types[[level]]]] == clicked) %>%
    dplyr::distinct(.data[[types[[clicked_level]]]]) %>%
    dplyr::pull()
  return(out)
}


parse_effect <- function(plot_data,
                         hov_exposure,
                         hov_outcome,
                         exp_cat,
                         out_cat,
                         settings) {
  if (is.null(hov_exposure) | is.null(hov_outcome)) {
    return("")
  }
  hov_exposure <- stringr::str_replace_all(hov_exposure, "\n", " ")
  hov_outcome <- stringr::str_replace_all(hov_outcome, "\n", " ")

  es_row <- plot_data %>%
    dplyr::filter(
      .data$outcome == hov_outcome &
        .data$exposure == hov_exposure &
        .data$outcome_type == out_cat &
        .data$exposure_type == exp_cat
    ) %>%
    dplyr::distinct()

  if (nrow(es_row) != 1) {
    message(
      glue::glue(
        "Problem with number of rows in es_row ({nrow(es_row)}, expected 1)"
      )
    )
  }

  es_size <- dplyr::case_when(
    # TODO: add HTML to these
    abs(es_row$r) > settings$threshold_r_large ~ "strong",
    abs(es_row$r) > settings$threshold_r_mod ~ "moderate",
    abs(es_row$r) > settings$threshold_r_small ~ "weak",
    TRUE ~ "no"
  )

  es_dir <- dplyr::if_else(es_row$r > 0, "positive", "negative")

  es_col <- dplyr::case_when(
    es_dir == "positive" & es_size == "strong" ~ settings$colour_r_large_pos,
    es_dir == "positive" & es_size == "moderate" ~ settings$colour_r_mod_pos,
    es_dir == "positive" & es_size == "weak" ~ settings$colour_r_small_pos,
    es_dir == "negative" & es_size == "moderate" ~ settings$colour_r_large_neg,
    es_dir == "negative" & es_size == "strong" ~ settings$colour_r_mod_neg,
    es_dir == "negative" & es_size == "weak" ~ settings$colour_r_small_neg,
    es_size == "no" ~ "#FFFFFF"
  )

  es_dir <- dplyr::if_else(es_dir == "positive", "risk", "benefit")

  es_hetero <- dplyr::if_else(
    es_row[["i_2"]] > settings$threshold_i,
    ", but the evidence is mixed.", "."
  )

  if (es_size == "no") {
    out_string <- glue::glue(
      "<center><b>{es_row$exposure}</b> is ",
      "<font color==\"#FFFFFF\"><b>not</b></font> associated with ",
      "{stringr::str_to_lower(es_row$outcome)}{es_hetero}"
    )
  } else {
    out_string <- glue::glue(
      "<center><b>{es_row$exposure}</b> is associated with a ",
      "<font color=\"{es_col}\"><b>{es_size} {es_dir} </b></font> ",
      "for <b>{stringr::str_to_lower(es_row$outcome)}</b> outcomes{es_hetero}"
    )
  }

  return(out_string)
}

show_metadata <- function(id, data, method = "shinyalert") {
  if (method == "shinyalert") {
    shinyalert::shinyalert(
      title = glue::glue(
        "Association between {data$exposure} and {data$outcome}"
      ),
      text = shiny::tagList(
        shiny::plotOutput(shiny::NS(id, "forestplot")),
        shiny::HTML(
          glue::glue(
            "This association is based on the findings of ",
            "<b>{data$k} studies</b>, which collected data on ",
            "<b>{data$n} participants</b>. <br>
          <b>Original meta-analysis:</b> {data$author} {data$year}; ",
            "<a href = 'https://www.youtube.com/watch?v=dQw4w9WgXcQ', ",
            "target = '_blank', style='color:#a6d8f0;'>",
            "10.31219/osf.io/c59v3</a><br>"
          )
        )
      ),
      html = TRUE,
      size = "l"
    )
  }

  if (method == "shinymodal") {
    shiny::showModal(
      shiny::modalDialog(shiny::HTML(
        glue::glue(
          "This association is based on the findings of {plot_data$k} studies, ",
          "which collected data on {plot_data$n} participants."
        )
      ),
      shiny::plotOutput("forestplot"),
      title = glue::glue(
        "Association between {plot_data$exposure} and {plot_data$outcome}"
      )
      )
    )
  }
}

make_forest_plot <- function(plot_data) {
  plot_data %>%
    tidyr::unnest("data", names_sep = "_") %>%
    dplyr::mutate(
      data_author_year = glue::glue("{data_author} ({data_year})")
    ) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = stats::reorder(.data$data_author_year, -.data$data_r),
      y = .data$data_r,
      ymin = .data$data_r_lb,
      ymax = .data$data_r_ub,
      # TODO: Fix colours
      fill = "#a6d8f0", col = "#a6d8f0"
    )) +
    ggplot2::geom_linerange(size = 5) +
    ggplot2::geom_point(ggplot2::aes(size = .data$data_n),
      shape = 21, colour = "white", stroke = 0.5
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Study", y = "Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

warn_screen_dims <- function(width, height, settings) {
  if (width < settings$min_xdim | height < settings$min_ydim) {
    shinyalert::shinyalert(
      title = "Screen Size",
      text = glue::glue(
        "This app is best viewed in a screen with a minimum resolution",
        "of <b>{settings$min_xdim} x {settings$min_ydim} </b>.<br> ",
        "It looks like your screen is smaller than this, so you may",
        " experience some issues."
      ),
      html = TRUE, type = "warning"
    )
  }
}

make_prev_state_list <- function(clicked_exp, clicked_out,
                                 exp_level_current, exp_level_updated,
                                 out_level_current, out_level_updated) {
  prev_state_list <- list(
    clicked_exp = clicked_exp,
    clicked_out = clicked_out,
    exp_level_current = exp_level_current,
    exp_level_updated = exp_level_updated,
    out_level_current = out_level_current,
    out_level_updated = out_level_updated
  )
  return(prev_state_list)
}
