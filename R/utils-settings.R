
#' Get Application Settings
#'
#' To ensure consistency between modules, this function returns a list of
#' settings for things like thresholds, colour schemes, etc.
#'
#'
#' @return A named list of settings
get_settings <- function() {
  settings <- list()

  # Thresholds
  settings$threshold_r_large <- 0.5
  settings$threshold_r_mod <- 0.3
  settings$threshold_r_small <- 0.1
  settings$threshold_i <- 0.5

  # Colours
  settings$colour_r_large_pos <- "#DC1C13"
  settings$colour_r_mod_pos <- "#F07470"
  settings$colour_r_small_pos <- "#F6BDC0"
  settings$colour_r_null <- "#F6BDC0"
  settings$colour_r_small_neg <- "#BFBFFF"
  settings$colour_r_mod_neg <- "#7879FF"
  settings$colour_r_large_neg <- "#1F1FFF"

  # Fonts
  settings$wrap_width <- 15



  return(settings)
}
