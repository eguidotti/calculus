test_that("202012291904", {
  e <- 1:26
  names(e) <- letters[1:26]
  n <- matrix(e, nrow = 2)
  s <- matrix(names(e), nrow = 2)
  x <- n %*% t(n) 
  y <- evaluate(s %mx% t(s), e)
  expect_equal(x, y)
})

test_that("202012291905", {
  e <- 1:26
  names(e) <- letters[1:26]
  n <- matrix(e, nrow = 2)
  s <- matrix(names(e), nrow = 2)
  x <- n %mx% t(n) 
  y <- evaluate(s %mx% t(s), e)
  expect_equal(x, y)
})

test_that("202012291906", {
  e <- 1:26
  names(e) <- letters[1:26]
  n <- matrix(e, nrow = 1)
  s <- matrix(names(e), nrow = 1)
  x <- n %*% t(n) 
  y <- evaluate(s %mx% t(s), e)
  expect_equal(x, y)
})

test_that("202012291907", {
  e <- 1:26
  names(e) <- letters[1:26]
  n <- matrix(e, nrow = 1)
  s <- matrix(names(e), nrow = 1)
  x <- n %mx% t(n) 
  y <- evaluate(s %mx% t(s), e)
  expect_equal(x, y)
})

test_that("202012291908", {
  e <- 1
  names(e) <- letters[1]
  n <- matrix(e, nrow = 1)
  s <- matrix(names(e), nrow = 1)
  x <- n %*% t(n) 
  y <- evaluate(s %mx% t(s), e)
  expect_equal(x, y)
})

test_that("202012291909", {
  e <- 1
  names(e) <- letters[1]
  n <- matrix(e, nrow = 1)
  s <- matrix(names(e), nrow = 1)
  x <- n %mx% t(n) 
  y <- evaluate(s %mx% t(s), e)
  expect_equal(x, y)
})

test_that("202506160842", {
  x <- array(c(1 + 2i, 2, 3, 4), dim = c(i = 2, j = 2))
  d1 <- mxdet(x)
  d2 <- prod(eigen(x, only.values = TRUE)$values)
  expect_equal(d1, d2)
})
