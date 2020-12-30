test_that("202012131431", {
  x <- hessian(f = "sin(x)", var = "x")
  y <- matrix("-sin(x)")
  expect_equal(x,y)
})

test_that("202012131536", {
  x <- hessian(f = "sin(x)", var = "x", drop = FALSE)
  y <- array("-sin(x)", dim = c(1,1,1))
  expect_equal(x,y)
})

test_that("202012131432", {
  x <- hessian(f = "x^2*y^2", var = c("x","y"))
  y <- matrix(c("2 * y^2", "2 * x * (2 * y)", "2 * x * (2 * y)", "x^2 * 2"), nrow = 2)
  expect_equal(x,y)
})

test_that("202012131433", {
  f <- c("sin(x)*y", "cos(y)*x")
  x <- hessian(f = f, var = c("x","y","z"))
  y <- array(c("-(sin(x) * y)","0","cos(x)","-sin(y)","0","0","cos(x)","-sin(y)","0","-(cos(y) * x)","0","0","0","0","0","0","0","0"), dim = c(2,3,3))
  expect_equal(x,y)
})

test_that("202012131846", {
  f.num <- function(x, y) array(c(x^2*y^2, x, x^2, y), dim = c(2,2))
  f.sym <- array(c('x^2*y^2', 'x', 'x^2', 'y'), dim = c(2,2))
  x <- hessian(f.num, var = c('x' = 2,'y' = 3))
  y <- hessian(f.sym, var = c('x' = 2,'y' = 3))
  expect_equal(x,y)
})

test_that("202012131847", {
  f.num <- function(x, y) array(c(x^2*y^2, x*cos(y), x^2, sin(y)*x), dim = c(1,2,2))
  f.sym <- array(c('x^2*y^2', 'x*cos(y)', 'x^2', 'sin(y)*x'), dim = c(1,2,2))
  x <- hessian(f.num, var = c('x' = 2,'y' = 3), drop = FALSE, accuracy = 4)
  y <- hessian(f.sym, var = c('x' = 2,'y' = 3), drop = FALSE)
  expect_equal(x,y)
})

test_that("202012131847", {
  f.num <- function(x, y) array(c(x^2*y^2, exp(x)*y^2, sqrt(y)*x^2, sin(y)*x), dim = c(1,2,2))
  f.sym <- array(c('x^2*y^2', 'exp(x)*y^2', 'sqrt(y)*x^2', 'sin(y)*x'), dim = c(1,2,2))
  x <- hessian(f.num, var = c('x' = 2,'y' = 3), accuracy = 4)
  y <- hessian(f.sym, var = c('x' = 2,'y' = 3))
  expect_equal(x,y)
})

test_that("202012131910", {
  f.num <- function(x, y) x^2*y^2
  f.sym <- 'x^2*y^2'
  x <- hessian(f.num, var = c('x' = 2,'y' = 3))
  y <- hessian(f.sym, var = c('x' = 2,'y' = 3))
  expect_equal(x,y)
})

test_that("202012131915", {
  f <- function(x, y) array(x^2*y^2, dim = c(1,1,1))
  x <- hessian(f, var = c('x' = 2,'y' = 3))
  y <- matrix(c(18,24,24,8), nrow = 2)
  expect_equal(x,y)
})

test_that("202012131917", {
  f <- function(x, y) array(x^2*y^2, dim = c(1,1,1))
  x <- hessian(f, var = c('x' = 2,'y' = 3), drop = FALSE)
  y <- array(c(18,24,24,8), dim = c(1,1,1,2,2))
  expect_equal(x,y)
})

# test_that("202012131926", {
#   f.num <- function(x, y, z) x^2*y^2*z^2
#   f.sym <- 'x^2*y^2*z^2'
#   x <- hessian(f.num, var = c('x' = 2,'y' = 3, 'z' = 45), accuracy = 4, coordinates = 'spherical')
#   y <- hessian(f.sym, var = c('x' = 2,'y' = 3, 'z' = 45), coordinates = 'spherical')
#   expect_equal(x,y)
# })

# test_that("202012131931", {
#   f.num <- function(x, y, z) c(x^2*y^2*z^2, x^2*y^2*z^2)
#   f.sym <- 'x^2*y^2*z^2'
#   x <- hessian(f.num, var = c('x' = 2,'y' = 3, 'z' = 45), accuracy = 4, coordinates = 'spherical')
#   y <- hessian(f.sym, var = c('x' = 2,'y' = 3, 'z' = 45), coordinates = 'spherical')
#   expect_equal(x[1,,],y) & expect_equal(x[2,,],y)
# })

# test_that("202012131941", {
#   f.num <- function(x) x[1]^1*x[2]^2*x[3]^3
#   f.sym <- 'x^1*y^2*z^3'
#   x <- hessian(f.num, var = c(2, 3, 45), accuracy = 4, coordinates = 'spherical')
#   y <- hessian(f.sym, var = c('x' = 2,'y' = 3, 'z' = 45), coordinates = 'spherical')
#   expect_equal(x,y)
# })

test_that("202012132005", {
  f <- function(x) x
  x <- hessian(f, 1)
  y <- matrix(0)
  expect_equal(x,y)
})

test_that("202012141710", {
  f <- function(x, extra) if(extra) x
  x <- hessian(f, 1, params = list(extra = TRUE))
  y <- matrix(0)
  expect_equal(x,y)
})

test_that("202012302350", {
  f.num <- function(x, y) array(c(x^2*y^2, x, x^2, y), dim = c(2,2))
  f.sym <- array(c('a*x^2*y^2', 'b*x', 'x^2', 'y'), dim = c(2,2))
  x <- hessian(f.num, var = c('x' = 2,'y' = 3))
  y <- hessian(f.sym, var = c('x' = 2,'y' = 3), params = list(a = 1, b = 1))
  expect_equal(x,y)
})
