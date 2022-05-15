
#' Make Heatmap
#'
#' Create the heatmap to descripve the exposure/outcome relationships.
#'
#' @importFrom rlang .data
#' @export
#' @param df The tidy dataframe.
#' @param subcategories Show subcategories?.
#' @param show Units to show as cells in the heatmap.
#'
#' @return A ggplot object
make_heatmap <- function(df, subcategories = FALSE, show = "Counts") {
  if (subcategories) {
    exposure <- rlang::sym("plain_language_exposure")
    outcome <- rlang::sym("plain_language_outcome")
  } else {
    exposure <- rlang::sym("general_exposure")
    outcome <- rlang::sym("general_outcome")
  }

  units <- switch(show,
    "Counts" = "n",
    "Effect Sizes" = "r"
  )

  base_plot <- df %>%
    dplyr::group_by(!!exposure, !!outcome) %>%
    dplyr::summarise(n = dplyr::n(), r = mean(.data$r)) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!exposure,
        y = !!outcome,
        fill = !!rlang::sym(units)
      )
    ) +
    ggplot2::geom_tile() +
    ggplot2::theme_minimal()

  if (units == "r") {
    base_plot <- base_plot +
      ggplot2::scale_fill_distiller(palette = "RdBu", limits = c(-.5, .5))
  }

  if (subcategories) {
    base_plot <- base_plot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  }

  return(base_plot)
}