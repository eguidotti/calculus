test_that("202012141334", {
  f <- c('x^2','y^3','z^4')
  x <- divergence(f, var = c('x','y','z'))
  y <- "2 * x + 3 * y^2 + 4 * z^3"
  expect_equal(x,y)
})

test_that("202012141335", {
  f <- c('x^2','y^3','z^4')
  x <- divergence(f, var = c('x','y','z'), drop = FALSE)
  y <- array("2 * x + 3 * y^2 + 4 * z^3")
  expect_equal(x,y)
})

test_that("202012141336", {
  f <- array(rep(c('x^2','y^3','z^4'), each = 2), dim = c(2,3))
  x <- divergence(f, var = c('x','y','z'))
  y <- array("2 * x + 3 * y^2 + 4 * z^3", dim = 2)
  expect_equal(x,y)
})

test_that("202012141345", {
  f <- function(x, y, z) c(x^2, y^3, z^4)
  x <- divergence(f, var = c('x' = 1, 'y' = 2, 'z' = 3))
  y <- 122
  expect_equal(x,y)
})

test_that("202012141346", {
  f <- function(x, y, z) c(x^2, y^3, z^4)
  x <- divergence(f, var = c('y' = 2, 'z' = 3, 'x' = 1))
  y <- 0
  expect_equal(x,y)
})

test_that("202012141351", {
  f <- function(x) c(x[1]^2, x[2]^3, x[3]^4)
  x <- divergence(f, var = c(1, 2, 3))
  y <- 122
  expect_equal(x,y)
})

test_that("202012141352", {
  f <- function(x) c(x[1]^2, x[2]^3, x[3]^4)
  x <- divergence(f, var = c(2, 3, 1))
  y <- 35
  expect_equal(x,y)
})

test_that("202012141353", {
  f <- c("r^3","r*z","r*z*sin(phi)")
  x <- divergence(f, var = c('r'=10,'phi'=pi/6,'z'=Inf), coordinates = 'cylindrical')
  y <- 405
  expect_equal(x,y)
})

test_that("202012141410", {
  f <- function(r, phi, z) c(r^3,r*z,r*z*sin(phi))
  x <- divergence(f, var = c('r'=10,'phi'=pi/6,'z'=100), coordinates = 'cylindrical')
  y <- 405
  expect_equal(x,y)
})

test_that("202012141411", {
  f <- function(x) c(x[1]^3,x[1]*x[3],x[1]*x[3]*sin(x[2]))
  x <- divergence(f, var = c(10,pi/6,100), coordinates = 'cylindrical')
  y <- 405
  expect_equal(x,y)
})

test_that("202012141412", {
  f <- function(r, phi, z, extra) if(extra) c(r^3,r*z,r*z*sin(phi))
  x <- divergence(f, var = c('r'=10,'phi'=pi/6,'z'=100), coordinates = 'cylindrical', params = list(extra = TRUE))
  y <- 405
  expect_equal(x,y)
})

test_that("202012141625", {
  f <- function(x, extra) if(extra) array(1:6*rep(c(x[1]^3,x[1]*x[3],x[1]*x[3]*sin(x[2])), each = 6), dim = c(1,3,2,3))
  x <- divergence(f, var = c(10,pi/6,100), coordinates = 'cylindrical', params = list(extra = TRUE))
  y <- 405*array(1:6, dim = c(1,3,2))
  expect_equal(x,y)
})

test_that("202012141633", {
  f <- c('0', 'r^-2', 'k')
  x <- divergence(f, var = c('r','phi','z'), coordinates = 'cylindrical')
  y <- "0"
  expect_equal(x,y)
})

test_that("202012141647", {
  f <- c('r*cos(phi)', '-r*sin(phi)', 'r*z')
  x <- divergence(f, var = c('r'=12,'phi'=pi/3,'z'=Inf), coordinates = 'cylindrical')
  y <- 12.5
  expect_equal(x,y)
})

test_that("202012141704", {
  f <- array(c('r','r^2*sin(theta)','r*sin(theta)','0','0','r^2*sin(phi)','0','0','r*cos(theta)'), dim = c(3,3))
  x <- divergence(f, var = c('r'=10,'theta'=20,'phi'=30), coordinates = 'spherical')
  y <- function(r,theta,phi) array(c(3, 4*r*sin(theta), 3*sin(theta) + r*cos(theta)/sin(theta)*sin(phi)))
  expect_equal(x,y(10,20,30))
})

test_that("202012141704", {
  f <- function(x) x
  x <- divergence(f, 1:100)
  y <- 100
  expect_equal(x,y)
})

test_that("202012142259", {
  f <- function(x) x
  x <- divergence(f, 1, coordinates = "spherical")
  y <- 1
  expect_equal(x,y)
})

test_that("202012302353", {
  f <- c('a*x^2', 'y^3', 'z^4')
  x <- divergence(f, var = c('x' = 1, 'y' = 2, 'z' = 3), params = list(a = 1))
  y <- 122
  expect_equal(x,y)
})
