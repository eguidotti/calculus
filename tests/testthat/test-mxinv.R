test_that("202012291918", {
  set.seed(123)
  e <- runif(25)
  names(e) <- letters[1:length(e)]
  n <- matrix(e, nrow = 5)
  s <- matrix(names(e), nrow = 5)
  x <- solve(n) 
  y <- evaluate(mxinv(s), e)
  expect_equal(x, y)
})

test_that("202012291919", {
  set.seed(123)
  e <- runif(25)
  names(e) <- letters[1:length(e)]
  n <- matrix(e, nrow = 5)
  s <- matrix(names(e), nrow = 5)
  x <- mxinv(n) 
  y <- evaluate(mxinv(s), e)
  expect_equal(x, y)
})

test_that("202012291920", {
  set.seed(123)
  e <- runif(4)
  names(e) <- letters[1:4]
  n <- matrix(e, nrow = 2)
  s <- matrix(names(e), nrow = 2)
  x <- mxinv(n) 
  y <- evaluate(mxinv(s), e)
  expect_equal(x, y)
})

test_that("202012291921", {
  set.seed(123)
  e <- runif(1)
  names(e) <- letters[1]
  n <- matrix(e, nrow = 1)
  s <- matrix(names(e), nrow = 1)
  x <- mxinv(n) 
  y <- evaluate(mxinv(s), e)
  expect_equal(x, y)
})
