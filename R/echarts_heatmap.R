echarts_heatmap <- function(plot_data, curr_outcome, curr_exposure, settings) {
  plot_data %>%
    dplyr::mutate(
      exposure = stringr::str_wrap(.data$exposure, settings$wrap_width),
      outcome = stringr::str_wrap(.data$outcome, settings$wrap_width),
    ) %>%
    echarts4r::e_charts_("exposure") %>%
    echarts4r::e_heatmap_("outcome", "r",
      itemStyle = list(emphasis = list(shadowBlur = 10))
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
    echarts4r::e_x_axis(
      axisLabel = list(
        interval = 0,
        rotate = 30
      ),
      triggerEvent = TRUE
    ) %>%
    echarts4r::e_y_axis(triggerEvent = TRUE) %>%
    # echarts4r::e_grid(bottom = 60, left = "200") %>%
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
