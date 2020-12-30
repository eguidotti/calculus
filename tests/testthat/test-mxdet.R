test_that("202012291914", {
  set.seed(123)
  e <- runif(25)
  names(e) <- letters[1:length(e)]
  n <- matrix(e, nrow = 5)
  s <- matrix(names(e), nrow = 5)
  x <- det(n) 
  y <- evaluate(mxdet(s), e)
  expect_equal(x, y)
})

test_that("202012291915", {
  set.seed(123)
  e <- runif(25)
  names(e) <- letters[1:length(e)]
  n <- matrix(e, nrow = 5)
  s <- matrix(names(e), nrow = 5)
  x <- mxdet(n) 
  y <- evaluate(mxdet(s), e)
  expect_equal(x, y)
})

test_that("202012291916", {
  set.seed(123)
  e <- runif(4)
  names(e) <- letters[1:4]
  n <- matrix(e, nrow = 2)
  s <- matrix(names(e), nrow = 2)
  x <- mxdet(n) 
  y <- evaluate(mxdet(s), e)
  expect_equal(x, y)
})

test_that("202012291917", {
  set.seed(123)
  e <- runif(1)
  names(e) <- letters[1]
  n <- matrix(e, nrow = 1)
  s <- matrix(names(e), nrow = 1)
  x <- mxdet(n) 
  y <- evaluate(mxdet(s), e)
  expect_equal(x, y)
})
