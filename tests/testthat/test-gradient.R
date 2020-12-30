test_that("202012131411", {
  x <- gradient(f = "sin(x)", var = "x")
  y <- "cos(x)"
  expect_equal(x,y)
})

test_that("202012131510", {
  x <- gradient(f = "sin(x)", var = "x", drop = FALSE)
  y <- array("cos(x)", dim = c(1,1))
  expect_equal(x,y)
})

test_that("202012131412", {
  x <- "sin(x)" %gradient% "x"
  y <- "cos(x)"
  expect_equal(x,y)
})

test_that("202012131413", {
  x <- gradient(f = "sin(x)", var = c("x" = 0))
  y <- 1
  expect_equal(x,y)
})

test_that("202012131414", {
  x <- "sin(x)" %gradient% c(x=0)
  y <- 1
  expect_equal(x,y)
})

test_that("202012131415", {
  x <- gradient(f = "y*sin(x)", var = c("x","y"))
  y <- c("y * cos(x)", "sin(x)")
  expect_equal(x,y)
})

test_that("202012131416", {
  f <- c("y*sin(x)", "x*cos(y)")
  x <- gradient(f = f, var = c("x","y"))
  y <- array(c("y * cos(x)", "cos(y)", "sin(x)", "-(x * sin(y))"), dim = c(2,2))
  expect_equal(x,y)
})

test_that("202012131417", {
  f.num <- function(x, y) c(y*sin(x), x*cos(y))
  f.sym <- c("y*sin(x)", "x*cos(y)")
  var <- c(x=-pi, y=-13)
  x <- gradient(f = f.num, var = var)
  y <- gradient(f = f.sym, var = var)
  expect_equal(x,y)
})

test_that("202012131418", {
  x <- gradient('r*theta*phi', var = c('r','theta','phi'), coordinates = 'spherical')
  y <- c("1/1 * (theta * phi)", "1/r * (r * phi)", "1/(r*sin(theta)) * (r * theta)")
  expect_equal(x,y)
})

test_that("202012131418", {
  f.num <- function(r, theta, phi) rep(r*theta*phi, 2)
  f.sym <- rep("r*theta*phi", 2)
  var <- c(r = 100, theta = 52, phi = pi/3)
  x <- gradient(f.num, var = var, coordinates = 'spherical')
  y <- gradient(f.sym, var = var, coordinates = 'spherical')
  expect_equal(x,y)
})

test_that("202012131807", {
  f.num <- function(x) rep(prod(x), 2)
  f.sym <- rep("r*theta*phi", 2)
  x <- gradient(f.num, var = c(100, 52, pi/3), coordinates = 'spherical')
  y <- gradient(f.sym, var = c(r = 100, theta = 52, phi = pi/3), coordinates = 'spherical')
  expect_equal(x,y)
})

test_that("202012131919", {
  f <- function(x, y) array(x^2*y^2, dim = c(1,1,1))
  x <- gradient(f, var = c('x' = 2,'y' = 3))
  y <- c(36, 24)
  expect_equal(x,y)
})

test_that("202012131920", {
  f <- function(x, y) array(x^2*y^2, dim = c(1,1,1))
  x <- gradient(f, var = c('x' = 2,'y' = 3), drop = FALSE)
  y <- array(c(36, 24), dim = c(1,1,1,2))
  expect_equal(x,y)
})

test_that("202012141708", {
  f <- function(x, y, extra) if(extra) array(x^2*y^2, dim = c(1,1,1))
  x <- gradient(f, var = c('x' = 2,'y' = 3), drop = FALSE, params = list(extra = TRUE))
  y <- array(c(36, 24), dim = c(1,1,1,2))
  expect_equal(x,y)
})

test_that("202012141709", {
  f.num <- function(x, extra) if(extra) rep(prod(x), 2)
  f.sym <- rep("r*theta*phi", 2)
  x <- gradient(f.num, var = c(100, 52, pi/3), coordinates = 'spherical', params = list(extra = TRUE))
  y <- gradient(f.sym, var = c(r = 100, theta = 52, phi = pi/3), coordinates = 'spherical')
  expect_equal(x,y)
})

test_that("202012302347", {
  x <- gradient(f = "a*sin(x)", var = c("x"=0), params = list(a = 10))
  y <- 10
  expect_equal(x,y)
})
