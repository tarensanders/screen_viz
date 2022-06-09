echarts_heatmap <- function(plot_data, curr_outcome, curr_exposure, settings) {
  plot_data %>%
    dplyr::mutate(
      exposure = stringr::str_wrap(.data$exposure, settings$wrap_width_x),
      outcome = stringr::str_wrap(.data$outcome, settings$wrap_width_y),
      tooltip = glue::glue(
        "<strong>Correlation:</strong> {format(r, digits = 2)}<br>",
        "[{format(r_lb, digits = 2)} - {format(r_ub, digits = 2)}]<br>",
        "<small>Click for more details</small>"
      )
    ) %>%
    echarts4r::e_charts_("exposure") %>%
    echarts4r::e_heatmap_("outcome", "r",
      bind = "tooltip",
      itemStyle = list(
        emphasis = list(shadowBlur = 10),
        borderColor = "#606060",
        borderWidth = 2
      )
    ) %>%
    echarts4r::e_visual_map(
      top = "top",
      right = 0,
      min = -0.5, max = 0.5, precision = 2,
      inRange = list(color = c(
        settings$colour_r_large_neg, "#FFFFFF", settings$colour_r_large_pos
      )),
      orient = "horizontal",
      text = list("Risk", "Benefit"),
      textStyle = list(color = "#FFFFFF")
    ) %>%
    echarts4r::e_tooltip(
      formatter = htmlwidgets::JS("
      function(params){
        return(params.name)}
    ")
    ) %>%
    echarts4r::e_x_axis(
      axisLabel = list(
        interval = 0,
        fontSize = 15
      ),
      triggerEvent = TRUE
    ) %>%
    echarts4r::e_y_axis(
      axisLabel = list(
        interval = 0,
        fontSize = 15
      ),
      triggerEvent = TRUE
    ) %>%
    echarts4r::e_grid(
      left = "13.5%",
      bottom = "30%"
    ) %>%
    echarts4r::e_axis_labels(x = "Exposure", y = "Outcome") %>%
    echarts4r::e_text_style(
      color = "white"
    ) %>%
    echarts4r::e_on(
      "xAxis.category",
      "function(params) {
               Shiny.setInputValue('mod_heatmap-xaxis_clicked',
                                    params.value, {priority: 'event'});
               console.log(params.value);
              }"
    ) %>%
    echarts4r::e_on(
      "yAxis.category",
      "function(params) {
               Shiny.setInputValue('mod_heatmap-yaxis_clicked',
                                    params.value, {priority: 'event'});
               console.log(params.value);
              }"
    )
}
