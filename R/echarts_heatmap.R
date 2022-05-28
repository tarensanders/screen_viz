echarts_heatmap <- function(plot_data, curr_outcome, curr_exposure) {
  echarts4r::e_charts_(plot_data, "exposure") %>%
    echarts4r::e_heatmap_("outcome", "r",
      itemStyle = list(emphasis = list(shadowBlur = 10))
    ) %>%
    echarts4r::e_visual_map(
      top = "middle",
      right = 0,
      min = -0.8, max = 0.8, precision = 2,
      inRange = list(color = c("#0e1de7", "#ffffff", "#990000"))
    ) %>%
    echarts4r::e_x_axis(
      axisLabel = list(
        interval = 0,
        rotate = 30
      ),
      triggerEvent = TRUE
    ) %>%
    echarts4r::e_y_axis(triggerEvent = TRUE) %>%
    echarts4r::e_grid(bottom = "30%", left = "200") %>%
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