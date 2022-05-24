

dummy_data <- make_dummy_data(2000)
df <- dummy_data$data
exposures_key <- dummy_data$exp_key
outcomes_key <- dummy_data$out_key
outcome_types <- c("outcome_category", "outcome_group", "outcome_specific")
exposure_types <- c("exposure_group", "exposure_specific")
exp_level <- 1
out_level <- 1
curr_outcome <- update_curr(outcomes_key, outcome_types, out_level, NULL)
curr_exposure <- update_curr(exposures_key, exposure_types, exp_level, NULL)
clicked_exp <- NULL
clicked_out <- NULL

make_plot <- function(curr_outcome, curr_exposure) {
  df %>%
    filter(outcome %in% curr_outcome &
      exposure %in% curr_exposure) %>%
    e_charts(exposure) %>%
    e_heatmap(outcome, r) %>%
    e_visual_map(
      min = -1, max = 1, precision = 2,
      inRange = list(color = c("#0ee732", "#ffffff", "#990000"))
    )
}

# First screen
make_plot(curr_outcome, curr_exposure)

# click TV programs and movies
clicked_exp <- "TV programs and movies"
exp_level_clicked <- exp_level
clicked_out <- NULL
out_level_clicked <- out_level

exp_level <- update_level(exposure_types, exp_level, clicked_exp)
out_level <- update_level(outcome_types, out_level, clicked_out)
curr_outcome <- update_curr(
  outcomes_key, outcome_types, out_level, clicked_out, out_level_clicked
)
curr_exposure <- update_curr(
  exposures_key, exposure_types, exp_level, clicked_exp, exp_level_clicked
)
make_plot(curr_outcome, curr_exposure)

# click TV programs and movies: Coviewing / education
clicked_exp <- "TV programs and movies: Coviewing"
exp_level_clicked <- exp_level
clicked_out <- "education"
out_level_clicked <- out_level

exp_level <- update_level(exposure_types, exp_level, clicked_exp)
out_level <- update_level(outcome_types, out_level, clicked_out)
curr_outcome <- update_curr(
  outcomes_key, outcome_types, out_level, clicked_out, out_level_clicked
)
curr_exposure <- update_curr(
  exposures_key, exposure_types, exp_level, clicked_exp, exp_level_clicked
)
make_plot(curr_outcome, curr_exposure)


exposures_key %>%
  filter(.data[[exposures_types[[out_level]]]] == clicked_exp) %>%
  distinct(.data[[outcome_types[[out_level]]]])

x_level <- x_level + 1
y_level <- y_level + 1

df %>%
  filter(outcome = )


df_effects %>%
  select(outcome_category, starts_with(c("plain_", "general_", "specific"))) %>%
  distinct(general_exposure)

# exposures <- paste(
#   rep(LETTERS[1:4],
#     each = length(c(1, 2, 3, 4))
#   ), c(1, 2, 3, 4),
#   sep = ""
# )
# outcomes <- paste(
#   rep(LETTERS[23:26],
#     each = length(c(1, 2, 3, 4))
#   ),
#   c(1, 2, 3, 4),
#   sep = ""
# )

# exp_out <- expand.grid(exposures, outcomes, stringsAsFactors = FALSE)


# df_specific <- tibble(
#   outcome_specific = exp_out[[1]],
#   exposure_specific = exp_out[[2]]
# ) %>%
#   mutate(
#     r = runif(nrow(exp_out), -1, 1),
#     n = floor(runif(nrow(exp_out), 100, 1000)),
#     outcome_group = str_sub(outcome_specific, 1, 1),
#     exposure_group = str_sub(exposure_specific, 1, 1),
#     outcome_broad = if_else(
#       outcome_group %in% c("A", "B"), "AB", outcome_group
#     ),
#     exposure_broad = if_else(
#       exposure_group %in% c("Y", "Z"), "YZ", exposure_group
#     ),
#   ) %>%
#   sample_n(100)

# df_group <- df_specific %>%
#   group_by(outcome_group, exposure_group) %>%
#   summarise(r_group = mean(r), n_group = sum(n)) %>%
#   ungroup()

# df_broad <- df_specific %>%
#   group_by(outcome_broad, exposure_broad) %>%
#   summarise(r_broad = mean(r), n_broad = sum(n)) %>%
#   ungroup()


# df <- df_specific %>%
#   left_join(df_group, by = c("outcome_group", "exposure_group")) %>%
#   left_join(df_broad, by = c("outcome_broad", "exposure_broad"))

# e_charts(df, exposure_broad) %>%
#   e_heatmap(outcome_broad, r_broad) %>%
#   e_visual_map(
#     min = -1, max = 1, precision = 2,
#     inRange = list(color = c("#0ee732", "#ffffff", "#990000"))
#   )

# # click on AB/X
# df %>%
#   filter(outcome_broad == "AB" & exposure_broad == "X") %>%
#   e_charts(exposure_group) %>%
#   e_heatmap(outcome_group, r_group) %>%
#   e_visual_map(
#     min = -1, max = 1, precision = 2,
#     inRange = list(color = c("#0ee732", "#ffffff", "#990000"))
#   )

# # click on B/X
# df %>%
#   filter(outcome_group == "B" & exposure_group == "X") %>%
#   e_charts(exposure_specific) %>%
#   e_heatmap(outcome_specific, r) %>%
#   e_visual_map(
#     min = -1, max = 1, precision = 2,
#     inRange = list(color = c("#0ee732", "#ffffff", "#990000"))
#   )


# # click on just AB
# df %>%
#   filter(outcome_broad == "AB") %>%
#   e_charts(exposure_group) %>%
#   e_heatmap(outcome_group, r_group) %>%
#   e_visual_map(
#     min = -1, max = 1, precision = 2,
#     inRange = list(color = c("#0ee732", "#ffffff", "#990000"))
#   )

# # Then click on A
# df %>%
#   filter(outcome_group == "A") %>%
#   e_charts(exposure_group) %>%
#   e_heatmap(outcome_specific, r_group) %>%
#   e_visual_map(
#     min = -1, max = 1, precision = 2,
#     inRange = list(color = c("#0ee732", "#ffffff", "#990000"))
#   )