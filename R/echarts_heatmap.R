echarts_heatmap <- function(plot_data, curr_outcome, curr_exposure, settings) {
  plot_data %>%
    dplyr::mutate(
      exposure = stringr::str_wrap(exposure, settings$wrap_width),
      outcome = stringr::str_wrap(outcome, settings$wrap_width),
    ) %>%
    echarts4r::e_charts_("exposure") %>%
    echarts4r::e_heatmap_("outcome", "r",
      itemStyle = list(emphasis = list(shadowBlur = 10))
    ) %>%
    echarts4r::e_visual_map(
      top = "middle",
      right = 0,
      min = -0.8, max = 0.8, precision = 2,
      inRange = list(color = c(
        settings$colour_r_large_neg, "#FFFFFF", settings$colour_r_large_pos
      ))
    ) %>%
    echarts4r::e_x_axis(
      axisLabel = list(
        interval = 0,
        rotate = 30
      ),
      triggerEvent = TRUE
    ) %>%
    echarts4r::e_y_axis(triggerEvent = TRUE) %>%
    # echarts4r::e_grid(bottom = "30%", left = "200") %>%
    echarts4r::e_axis_labels(x = "Exposure", y = "Outcome") %>%
    echarts4r::e_text_style(
      color = "white"
    ) %>%
    echarts4r::e_on(
      "xAxis.category",
      "function(params) {
               Shiny.setInputValue('heatmap_app-xaxis_clicked',
                                    params.value, {priority: 'event'});
               console.log(params.value);
              }"
    ) %>%
    echarts4r::e_on(
      "yAxis.category",
      "function(params) {
               Shiny.setInputValue('heatmap_app-yaxis_clicked',
                                    params.value, {priority: 'event'});
               console.log(params.value);
              }"
    )
}
