
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
update_curr <- function(key, types, level, clicked, clicked_level) {
  if (is.null(clicked)) {
    out <-
      key %>%
      dplyr::distinct(.data[[types[[level]]]]) %>%
      dplyr::pull()
    return(out)
  }
  out <-
    key %>%
    dplyr::filter(.data[[types[[level]]]] == clicked) %>%
    dplyr::distinct(.data[[types[[clicked_level]]]]) %>%
    dplyr::pull()
  return(out)
}


parse_effect <- function(data, hov_exposure, hov_outcome, settings) {
  if (is.null(hov_exposure) | is.null(hov_outcome)) {
    return("")
  }

  es_row <- data %>%
    dplyr::filter(
      .data$outcome == hov_outcome,
      .data$exposure == hov_exposure
    ) %>%
    dplyr::distinct()

  stopifnot(nrow(es_row) == 1)

  es_size <- dplyr::case_when(
    # TODO: add HTML to these
    abs(es_row$r) > settings$threshold_r_large ~ "strong",
    abs(es_row$r) > settings$threshold_r_mod ~ "moderate",
    abs(es_row$r) > settings$threshold_r_small ~ "weak",
    TRUE ~ "not"
  )

  es_dir <- dplyr::if_else(es_row$r > 0, "positive", "negative")

  es_col <- dplyr::case_when(
    es_dir == "positive" & es_size == "strong" ~ settings$colour_r_large_pos,
    es_dir == "positive" & es_size == "moderate" ~ settings$colour_r_mod_pos,
    es_dir == "positive" & es_size == "weak" ~ settings$colour_r_small_pos,
    es_dir == "negative" & es_size == "moderate" ~ settings$colour_r_large_neg,
    es_dir == "negative" & es_size == "strong" ~ settings$colour_r_mod_neg,
    es_dir == "negative" & es_size == "weak" ~ settings$colour_r_small_neg,
    es_size == "not" ~ "#FFFFFF"
  )

  es_hetero <- dplyr::if_else(
    es_row[["i_2"]] > settings$threshold_i,
    ", but the evidence is mixed.", "."
  )

  out_string <- glue::glue(
    "<center>There is <font color=\"{es_col}\"><b>{es_size}</b></font> ",
    "evidence that {stringr::str_to_lower(es_row$exposure)} is associated ",
    "with {stringr::str_to_lower(es_row$outcome)}{es_hetero}</center>"
  )

  return(out_string)
}