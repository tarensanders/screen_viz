#' Tidy Data
#'
#' Tidies the dataset for visualisation.
#'
#' @importFrom rlang .data
#'
#' @param df The dataset to tidy.
#'
#' @return A tidy dataset
#' @name tidy_data
NULL
#> NULL

#' @rdname tidy_data
#' @export
tidy_data_effects <- function(df) {
  # Tidy the data
  df <-
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
        .data$moderator_age,
        c("All", "Young children", "Children", "Adolescents")
      ),
      year = as.integer(stringr::str_sub(.data$author_year, start = -4))
    )

  # Create the options for slicing the data
  slice_data <- function(df, var) {
    df %>%
      dplyr::group_by(
        .data$plain_language_outcome,
        .data$plain_language_exposure,
        .data$age_group
      ) %>%
      dplyr::slice_max(.data[[var]], with_ties = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::pull(.data$effect_size_id)
  }

  sliced_n <- slice_data(df, "n")
  sliced_k <- slice_data(df, "k")
  sliced_year <- slice_data(df, "year")

  df <- df %>%
    dplyr::mutate(
      use_effect_n =
        ifelse(.data$effect_size_id %in% sliced_n, TRUE, FALSE),
      use_effect_k =
        ifelse(.data$effect_size_id %in% sliced_k, TRUE, FALSE),
      use_effect_year =
        ifelse(.data$effect_size_id %in% sliced_year, TRUE, FALSE),
    )

  return(df)
}

#' @rdname tidy_data
#' @export
tidy_data_studies <- function(df) {
  df <-
    df %>%
    dplyr::select(
      c(
        "effect_size_id", "covidence_review_id", "within_effect_id",
        "study_author", "study_year", "study_name", "study_n"
      ),
      dplyr::starts_with("r_")
    )
  return(df)
}

#' @rdname tidy_data
#' @export
tidy_data_rob <- function(df) {
  df <-
    df %>%
    dplyr::filter(.data$reviewer == "Consensus") %>%
    dplyr::select(-dplyr::starts_with("x"), -.data$study_id, -.data$reviewer)
  return(df)
}

#' @rdname tidy_data
#' @export
tidy_data_refs <- function(df) {
  # TODO: Decide what of this data needs to stay
  return(df)
}