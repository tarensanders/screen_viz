# Setup
test_key <- dplyr::tibble(
  exposure_1 = c("a", "b", "b", "c", "c", "c"),
  exposure_2 = c("d", "e", "e", "f", "g", "g"),
  exposure_3 = c("h", "i", "j", "j", "j", "j")
)
test_types <- c("exposure_1", "exposure_2", "exposure_3")

# update_level
test_that("update_level works", {
  expect_equal(update_level(test_types, 1, NULL), 1)
  expect_equal(update_level(test_types, 1, "click"), 2)
  expect_equal(update_level(test_types, 2, "click"), 3)
  expect_equal(update_level(test_types, 3, "click"), 3)
})

# update_curr
update_curr_test <- function(level, clicked, cl_lvl) {
  update_curr(test_key, test_types, level, clicked, cl_lvl)
}
test_that("update_curr works with/without click_level provided", {
  # Should work with/without click_level provided
  expect_equal(update_curr_test(1, NULL), c("a", "b", "c"))
  expect_equal(update_curr_test(1, NULL, 1), c("a", "b", "c"))
})

test_that("update_curr works for different levels", {
  expect_equal(update_curr_test(1, "a", 2), c("d"))
  expect_equal(update_curr_test(1, "b", 2), c("e")) # remove duplicates
  expect_equal(update_curr_test(1, "c", 2), c("f", "g")) # return all unique

  expect_equal(update_curr_test(2, "d", 3), c("h"))
  expect_equal(update_curr_test(2, "e", 3), c("i", "j")) # return all unique
  expect_equal(update_curr_test(2, "g", 3), c("j")) # remove duplicates
})

test_that("update_curr click at same level returns itself", {
  expect_equal(update_curr_test(1, "a", 1), c("a"))
  expect_equal(update_curr_test(2, "e", 2), c("e"))
  expect_equal(update_curr_test(3, "i", 3), c("i"))
})