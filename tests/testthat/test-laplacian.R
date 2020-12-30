test_that("202012142242", {
  f <- 'x^2'
  x <- laplacian(f, var = c('x','y','z'))
  y <- "2"
  expect_equal(x,y)
})

test_that("202012142244", {
  f <- 'y^2*x^2'
  x <- laplacian(f, var = c('x','y'))
  y <- "y^2 * 2 + 2 * x^2"
  expect_equal(x,y)
})

test_that("202012142245", {
  f <- function(x,y) c(x^2*y^2, x^2)
  x <- laplacian(f, var = c(x=3,y=5))
  y <- array(c(68,2))
  expect_equal(x,y)
})

test_that("202012142246", {
  f <- function(x,y) array(rep(c(x^2*y^2, x^2), 3), dim = c(2,3))
  x <- laplacian(f, var = c(x=3,y=5))
  y <- array(rep(c(68,2), 3), dim = c(2,3))
  expect_equal(x,y)
})

test_that("202012142310", {
  f <- function(x) x^2
  x <- laplacian(f, var = 100, coordinates = "spherical")
  y <- 2
  expect_equal(x,y)
})

test_that("202012142311", {
  f.num <- function(x,y,z) array(rep(c(x^2*y*sin(z),z*y,x*z+y^2), 2), dim = c(2,3))
  f.sym <- array(rep(c('x^2*y*sin(z)','z*y','x*z+y^2'), 2), dim = c(2,3))
  x.num <- laplacian(f.num, var = c(x=2,y=3,z=4), coordinates = "spherical", accuracy = 4)
  x.sym <- laplacian(f.sym, var = c(x=2,y=3,z=4), coordinates = "spherical")
  expect_equal(x.num,x.sym)
})

test_that("202012142312", {
  f.num <- function(x,y,z) array(rep(c(x^2*y*sin(z),z*y,x*z+y^2), 2), dim = c(2,3))
  f.sym <- array(rep(c('x^2*y*sin(z)','z*y','x*z+y^2'), 2), dim = c(2,3))
  x.num <- laplacian(f.num, var = c(x=2,y=3,z=4), accuracy = 4)
  x.sym <- laplacian(f.sym, var = c(x=2,y=3,z=4))
  expect_equal(x.num,x.sym)
})

test_that("202012142312", {
  f.num <- function(x,y,z,extra) if(extra) array(rep(c(x^2*y*sin(z),z*y,x*z+y^2), 2), dim = c(2,3))
  f.sym <- array(rep(c('x^2*y*sin(z)','z*y','a*x*z+y^2'), 2), dim = c(2,3))
  x.num <- laplacian(f.num, var = c(x=2,y=3,z=4), accuracy = 4, params = list(extra = TRUE))
  x.sym <- laplacian(f.sym, var = c(x=2,y=3,z=4), params = list(a = 1))
  expect_equal(x.num,x.sym)
})
