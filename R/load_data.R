
#' Load and Tidy Data
#'
#' Load the dataset and convert to tidy format.
#'
#' @importFrom rlang .data
#'
#' @export
#' @return The tidy dataset
load_data <- function() {
  stopifnot(Sys.getenv("CLOUD_USER") != "", Sys.getenv("CLOUD_PASS") != "")

  df <- cloudstoR::cloud_get(
    "Shared/screenviz/effectsizedata.csv",
    user = Sys.getenv("CLOUD_USER"),
    password = Sys.getenv("CLOUD_PASS")
  )

  df %>%
    dplyr::mutate(
      plain_language_exposure =
        stringr::str_replace(
          .data$plain_language_exposure,
          "Video game console",
          "Video games"
        ),
      general_outcome = stringr::str_extract(
        .data$plain_language_outcome, "[^:]+"
      ),
      general_exposure = stringr::str_extract(
        .data$plain_language_exposure, "[^:]+"
      ),
      specific_outcome = gsub(".*: ", "", .data$plain_language_outcome),
      specific_exposure = gsub(".*: ", "", .data$plain_language_exposure),
      general_exposure =
        stringr::str_replace(
          .data$general_exposure,
          "Screen use",
          "General screen use"
        ),
      age_group = forcats::fct_relevel(
        .data$age_group,
        c("All", "Young children", "Children", "Adolescents")
      ),
      r = dplyr::if_else(
        is.na(.data$reanalysis_r),
        .data$converted_r,
        .data$reanalysis_r
      ),
      r_lb = dplyr::if_else(
        is.na(.data$reanalysis_cilb_95),
        .data$converted_cilb,
        .data$reanalysis_cilb_95
      ),
      r_ub = dplyr::if_else(
        is.na(.data$reanalysis_ciub_95),
        .data$converted_ciub,
        .data$reanalysis_ciub_95
      ),
      n = dplyr::if_else(
        is.na(.data$reanalysis_n), .data$original_n, .data$reanalysis_n
      ),
      k = dplyr::if_else(
        is.na(.data$reanalysis_k), .data$original_k, .data$reanalysis_k
      )
    ) %>%
    dplyr::group_by(
      .data$plain_language_outcome,
      .data$plain_language_exposure,
      .data$age_group
    ) %>%
    dplyr::slice_max(.data$n, with_ties = TRUE) %>%
    dplyr::ungroup()
}