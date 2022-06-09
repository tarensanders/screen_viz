
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
#' @importFrom rlang .data
#'
#' @param n_studies number of studies to simulate.
#'
#' @return A list with the created dataset, and the exposure/outcome keys.
make_dummy_data <- function(n_studies) {
  if (!rlang::is_installed("randomNames")) {
    stop("Package \"randomNames\" is needed to use dummy data")
  }
  # Load real data as base
  df_effects <-
    load_data("Shared/screenviz/combined_effects.rds") %>%
    tidy_data_effects()

  df_cats <- df_effects %>%
    dplyr::filter(.data$general_exposure != "Intervention") %>%
    dplyr::transmute(
      outcome_category = stringr::str_to_sentence(.data$outcome_category),
      outcome_group = .data$general_outcome,
      outcome_specific = .data$plain_language_outcome,
      exposure_group = .data$general_exposure,
      exposure_specific = .data$plain_language_exposure
    ) %>%
    dplyr::distinct()

  exposures_key <- df_cats %>%
    dplyr::select(dplyr::starts_with("exposure")) %>%
    dplyr::distinct()

  outcomes_key <- df_cats %>%
    dplyr::select(dplyr::starts_with("outcome")) %>%
    dplyr::distinct()

  # Create the dummy studies dataset
  df_studies <-
    dplyr::sample_n(df_cats, n_studies, replace = TRUE) %>%
    dplyr::mutate(
      study_id = dplyr::row_number(),
      author = randomNames::randomNames(n = n_studies, which.names = "last"),
      year = sample(seq(1975, 2019),
        n_studies,
        replace = TRUE,
        prob = seq(0.01, 0.04, length.out = 45)
      ),
      r = stats::runif(nrow(.), min = -0.5, max = 0.8),
      se = abs(stats::rnorm(nrow(.), mean = 0.05, sd = 0.04)),
      r_lb = .data$r - (1.96 * .data$se),
      r_ub = .data$r + (1.96 * .data$se),
      n = abs(floor(stats::rnorm(nrow(.), mean = 100, sd = 50)))
    )

  outcome_types <- c("outcome_category", "outcome_group", "outcome_specific")
  exposure_types <- c("exposure_group", "exposure_specific")

  # Create an empty dataframe
  df_out <- dplyr::tibble(
    outcome = character(n_studies * 2),
    exposure = character(n_studies * 2),
    data = vector(mode = "list", length = n_studies * 2),
    r = numeric(n_studies * 2),
    r_lb = numeric(n_studies * 2),
    r_ub = numeric(n_studies * 2),
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
        tidyr::nest() %>%
        dplyr::mutate(
          r = sapply(.data$data, function(df) mean(df$r)),
          r_lb = sapply(.data$data, function(df) mean(df$r_lb)),
          r_ub = sapply(.data$data, function(df) mean(df$r_ub)),
          n = sapply(.data$data, function(df) sum(df$n)),
          k = sapply(.data$data, function(df) nrow(df)),
          i_2 = stats::runif(1),
          outcome_type = outcome,
          exposure_type = exposure
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

  df_out <- df_out %>%
    dplyr::filter(outcome != "") %>%
    dplyr::mutate(
      author =
        randomNames::randomNames(nrow(.), which.names = "last"),
      year = sapply(.data$data, function(df) max(df$year) + 1),
      author =
        dplyr::if_else(
          .data$outcome_type == outcome_types[[length(outcome_types)]] &
            .data$exposure_type == exposure_types[[length(exposure_types)]],
          .data$author, ""
        ),
      year =
        dplyr::if_else(
          .data$outcome_type == outcome_types[[length(outcome_types)]] &
            .data$exposure_type == exposure_types[[length(exposure_types)]],
          .data$year, NA_real_
        )
    )

  return(list(
    "data" = df_out,
    "exp_key" = exposures_key,
    "out_key" = outcomes_key
  ))
}
