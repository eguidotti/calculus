test_that("202012081602", {
  expect_equal(evaluate(array('x'), c(x = 1)), array(1))
})

test_that("202012081603", {
  expect_equal(evaluate(expression(x), c(x = 1)), 1)
})

test_that("202012081604", {
  x <- array(letters[1:4], dim = c(2,2))
  e <- data.frame(a = 1:3, b = 2, c = 3, d = 4)
  x <- evaluate(x, e)
  y <- array(c(1:3,2,2,2,3,3,3,4,4,4), dim = c(3,4))
  expect_equal(x,y)
})

test_that("202012081605", {
  expect_equal(evaluate(expression(x), data.frame(x = 1)), matrix(1))
})
