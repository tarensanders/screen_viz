
#' Create dummy data
#'
#' Creates a dummy dataset for testing and development.
#'
#' The function takes the existing data and uses it as the basis of the dummy
#' studies. This means that studies that are created can only have
#' exposure/outcome combinations that are already in the original data.
#'
#' In addition, the function returns two additional dataframes that contain the
#' different levels for outcomes and exposures.
#'
#' @param n_studies number of studies to simulate.
#'
#' @return A list with the created dataset, and the exposure/outcome keys.
make_dummy_data <- function(n_studies) {
  # Load real data as base
  df_effects <-
    load_data("Shared/screenviz/combined_effects.rds") %>%
    tidy_data_effects()

  df_cats <- df_effects %>%
    dplyr::transmute(
      outcome_category = outcome_category,
      outcome_group = general_outcome,
      outcome_specific = plain_language_outcome,
      exposure_group = general_exposure,
      exposure_specific = plain_language_exposure
    ) %>%
    dplyr::distinct()

  exposures_key <- df_cats %>%
    dplyr::select(starts_with("exposure")) %>%
    dplyr::distinct()

  outcomes_key <- df_cats %>%
    dplyr::select(starts_with("outcome")) %>%
    dplyr::distinct()

  # Create the dummy studies dataset
  df_studies <-
    dplyr::sample_n(df_cats, n_studies, replace = TRUE) %>%
    dplyr::mutate(
      study_id = dplyr::row_number(),
      r = runif(nrow(.), min = -0.5, max = 0.8),
      n = abs(floor(rnorm(nrow(.), mean = 100, sd = 50)))
    )

  outcome_types <- c("outcome_category", "outcome_group", "outcome_specific")
  exposure_types <- c("exposure_group", "exposure_specific")

  # Create an empty dataframe
  df_out <- dplyr::tibble(
    outcome = character(n_studies * 2),
    exposure = character(n_studies * 2),
    r = numeric(n_studies * 2),
    n = numeric(n_studies * 2),
    k = numeric(n_studies * 2),
    i_2 = numeric(n_studies * 2),
    outcome_type = character(n_studies * 2),
    exposure_type = character(n_studies * 2)
  )

  count <- 0
  for (outcome in outcome_types) {
    for (exposure in exposure_types) {
      temp_df <-
        df_studies %>%
        dplyr::group_by(.data[[outcome]], .data[[exposure]]) %>%
        dplyr::summarise(
          r = mean(r), n = sum(n), k = dplyr::n(), i_2 = runif(1),
          outcome_type = outcome, exposure_type = exposure
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename(
          outcome = .data[[outcome]],
          exposure = .data[[exposure]]
        )

      df_out[(count + 1):(count + nrow(temp_df)), ] <- temp_df
      count <- count + nrow(temp_df)
    }
  }
  df_out <- df_out %>% dplyr::filter(outcome != "")
  return(list(
    "data" = df_out,
    "exp_key" = exposures_key,
    "out_key" = outcomes_key
  ))
}