test_that("202012081700", {
  expect_equal(class(e2c(c2e("x"))), "character")
})

test_that("202012081701", {
  expect_equal(class(e2c(c2e(array("x")))), "array")
})