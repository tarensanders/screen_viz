test_that("'pop' and 'push' work as expected", {
  test_obj <- c()
  expect_equal(push(test_obj, "a"), list("a"))
  expect_equal(push(test_obj, "b"), list("a", "b"))
  expect_equal(pop(test_obj), "b")
  expect_equal(pop(test_obj), "a")
  expect_null(pop(test_obj))
})
