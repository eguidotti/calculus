test_that("202012062212", {
  x <- derivative(f = "sin(x)", var = "x")
  y <- array("cos(x)")
  expect_equal(x,y)
})

test_that("202012062213", {
  x <- derivative(f = "sin(x)", var = c("x" = 0))
  y <- array(1)
  expect_equal(x,y)
})

test_that("202012062214", {
  x <- derivative(f = "sin(x)", var = "x", order = 2)
  y <- array("-sin(x)")
  expect_equal(x,y)
})

test_that("202012062215", {
  f <- c("y*sin(x)", "x*cos(y)")
  x <- derivative(f = f, var = c("x","y"), order = 1)
  y <- matrix(c("y * cos(x)","cos(y)","sin(x)","-(x * sin(y))"), nrow = 2)
  expect_equal(x,y)
})

test_that("202012062216", {
  x <- derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(1,2))
  y <- array("2 * cos(x)")
  expect_equal(x,y)
})

test_that("202012062217", {
  x <- derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(x = 1, y = 2))
  y <- array("2 * cos(x)")
  expect_equal(x,y)
})

test_that("202012062218", {
  x <- derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(x = 2, y = 1))
  y <- array("-(2 * y * sin(x))")
  expect_equal(x,y)
})

test_that("202012062219", {
  x <- derivative(f = "y^2*sin(x)", var = c(x = 1, y = 0), order = c(x = 2, y = 1))
  y <- array(0)
  expect_equal(x,y)
})

test_that("202012062220", {
  f <- array(c("x","x","y","y","y*sin(x)","x*cos(y)"), dim = c(2,3,1))
  x <- derivative(f = f, var = c("x", "y"), order = c(x = 1))
  y <- array(c("1","1","0","0","y * cos(x)","cos(y)"), dim = c(2,3,1))
  expect_equal(x,y)
})

test_that("202012062221", {
  f <- array(c("x","x","y","y","y*sin(x)","x*cos(y)"), dim = c(2,3,1))
  x <- derivative(f = f, var = c("x" = 0, "y" = 0), order = c(x = 1))
  y <- array(c(1,1,0,0,0,1), dim = c(2,3,1))
  expect_equal(x,y)
})

test_that("202012062222", {
  f <- array(c("x","x","y","y","y*sin(x)","x*cos(y)"), dim = c(2,3,1))
  x <- derivative(f = f, var = c("x", "y"), order = 1)
  y <- array(c("1","1","0","0","y * cos(x)","cos(y)","0","0","1","1","sin(x)","-(x * sin(y))"), dim = c(2,3,1,2))
  expect_equal(x,y)
})

test_that("202012101552", {
  f <- array(expression(x,x,y,y,y*sin(x),x*cos(y)), dim = c(2,3,1))
  x <- derivative(f = f, var = c("x", "y"), order = 1)
  y <- array(c("1","1","0","0","y * cos(x)","cos(y)","0","0","1","1","sin(x)","-(x * sin(y))"), dim = c(2,3,1,2))
  expect_equal(x,y)
})

test_that("202012062223", {
  f.sym <- array(c("x","x","y","y","y*sin(x)","x*cos(y)"), dim = c(2,3,1))
  f.num <- function(x, y) array(c(x,x,y,y,y*sin(x),x*cos(y)), dim = c(2,3,1))
  var <- c(x = 1, y = 2)
  x <- derivative(f = f.sym, var = var, order = 1)
  y <- derivative(f = f.num, var = var, order = 1)
  expect_equal(x,y)
})

test_that("202012062224", {
  f.sym <- array(c("x","x","y","y","y*sin(x)","x*cos(y)"), dim = c(2,3,1))
  f.num <- function(x, y, extra = TRUE) if(extra) return(array(c(x,x,y,y,y*sin(x),x*cos(y)), dim = c(2,3,1)))
  var <- c(x = 1, y = 2)
  x <- derivative(f = f.sym, var = var, order = 1)
  y <- derivative(f = f.num, var = var, order = 1)
  expect_equal(x,y)
})

test_that("202012062225", {
  f.sym <- "x^3*exp(y)"
  f.num <- function(x, y, extra) if(!extra) return(x^3*exp(y))
  var <- c(x = 1, y = 2)
  x <- derivative(f = f.sym, var = var, order = 1)
  y <- derivative(f = f.num, var = var, order = 1, params = list(extra = FALSE))
  expect_equal(x,y)
})

test_that("202012062226", {
  f.sym <- "x^3*exp(y)"
  f.num <- function(x, y, extra) if(!extra) return(x^3*exp(y))
  var <- c(x = 1, y = 2)
  x <- derivative(f = f.sym, var = var, order = c(x = 1))
  y <- derivative(f = f.num, var = var, order = c(x = 1), params = list(extra = FALSE))
  expect_equal(x,y)
})

test_that("202012101605", {
  f.sym <- "x^3*exp(y)"
  var <- c(x = 1, y = 2)
  x <- derivative(f = f.sym, var = var, order = c(x = 1, y = 2))
  y <- derivative(f = f.sym, var = var, order = c(y = 2, x = 1))
  expect_equal(x,y)
})

test_that("202012111312", {
  x <- derivative(sum, 1:100)
  y <- matrix(1, ncol = 100, nrow = 1)
  expect_equal(x,y)
})

test_that("202012111314", {
  x <- derivative(prod, 1:10)
  y <- matrix(sapply(1:10, function(i) prod(c(1:10)[-i])), nrow = 1)
  expect_equal(x,y)
})

test_that("202012111503", {
  f <- function(x, y, z) matrix(c(x^3*y^2*z, 2*x^2*y, 3*x^2*y, 4*x^2*y), nrow = 2)
  x <- derivative(f, var = c(x = 1, y = 2, z = 3), order = c(y = 1, x = 2))
  y <- matrix(c(72, 4, 6, 8), nrow = 2)
  expect_equal(x,y)
})

test_that("202012111621", {
  f <- function(x, y) x^2*exp(y)
  x <- derivative(f, var = c(x = 1, y = 0), order = 2, accuracy = 4)
  y <- matrix(c(2, 1), ncol = 2)
  expect_equal(x,y)
})

test_that("202012111623", {
  f <- function(x) x
  x <- derivative(f, 1, order = 0)
  y <- array(1)
  expect_equal(x,y)
})

test_that("202012111658", {
  f <- function(x) x
  x <- derivative(f, 1:3, order = 0)
  y <- matrix(1:3, nrow = 3, ncol = 3)
  expect_equal(x,y)
})

test_that("202012302318", {
  f <- "a*x"
  x <- derivative(f, c(x=0), order = 1, params = list(a=3))
  y <- array(3)
  expect_equal(x,y)
})
