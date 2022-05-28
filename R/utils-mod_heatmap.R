
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