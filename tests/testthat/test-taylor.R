test_that("202012310012", {
  x <- taylor("exp(x)", var = "x", order = 2)
  x <- x$f
  y <- "(1) * 1 + (1) * x^1 + (0.5) * x^2"
  expect_equal(x,y)
})

test_that("202012310013", {
  x <- taylor(function(x) exp(x), var = "x", order = 2)
  x <- evaluate(x$f, c(x = 2))
  y <- 5
  expect_equal(x,y)
})

test_that("202012310014", {
  x <- taylor("x*(y-1)", var = c(x=0, y=1), order = 4)
  x <- evaluate(x$f, c(x = 2, y = 10))
  y <- 18
  expect_equal(x,y)
})

test_that("202012310015", {
  x <- taylor("a*x*(y-1)", var = c(x=0, y=1), params = list(a = 2), order = 4)
  x <- evaluate(x$f, c(x = 2, y = 10))
  y <- 36
  expect_equal(x,y)
})

test_that("202012310016", {
  x <- taylor(prod, var = c(0,0,0), order = 3)
  x <- x$f
  y <- "(1) * x1^1*x2^1*x3^1"
  expect_equal(x,y)
})

test_that("202012310017", {
  x <- taylor(function(x, s) prod(x)+s, var = c(0,0,0), params = list(s=100), order = 3)
  x <- evaluate(x$f, c(x1=1, x2=2, x3=3))
  y <- 106
  expect_equal(x,y)
})
