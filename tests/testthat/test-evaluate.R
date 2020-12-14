test_that("202012081602", {
  expect_equal(evaluate('x', list(x = 1)), array(1))
})

test_that("202012081603", {
  expect_equal(evaluate(expression(x), list(x = 1)), array(1))
})

test_that("202012081604", {
  expect_equal(evaluate(function(x) x, list(x = 1)), array(1))
})