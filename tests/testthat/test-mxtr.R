test_that("202012291912", {
  e <- 1:25
  names(e) <- letters[1:25]
  n <- matrix(e, nrow = 5)
  s <- matrix(names(e), nrow = 5)
  x <- mxtr(n) 
  y <- evaluate(mxtr(s), e)
  expect_equal(x, y)
})

test_that("202012291913", {
  e <- 1
  names(e) <- letters[1]
  n <- matrix(e, nrow = 1)
  s <- matrix(names(e), nrow = 1)
  x <- mxtr(n) 
  y <- evaluate(mxtr(s), e)
  expect_equal(x, y)
})
