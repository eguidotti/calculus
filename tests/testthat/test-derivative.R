test_that("202012062212", {
  x <- derivative(f = "sin(x)", var = "x")
  y <- matrix("cos(x)", dimnames = list("sin(x)"))
  expect_equal(x,y)
})
