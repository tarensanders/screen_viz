settings <- get_settings()
test_that("thresholds are all numeric", {
  threshold_settings <- stringr::str_subset(names(settings), "threshold")
  for (thresh in threshold_settings) {
    expect_type(settings[[thresh]], "double")
  }
})

test_that("colours are valid hex codes", {
  colour_settings <- stringr::str_subset(names(settings), "colour")
  for (colour in colour_settings) {
    expect_true(stringr::str_detect(settings[[colour]], "^#[0-9a-fA-F]{6}$"))
  }
})