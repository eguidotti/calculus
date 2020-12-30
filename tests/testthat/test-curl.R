test_that("202012141736", {
  f <- c('x*y','y*z','x*z')
  x <- curl(f, var = c('x','y','z'))
  y <- c("(y) * -1", "(z) * -1", "(x) * -1")
  expect_equal(x,y)
})

test_that("202012141739", {
  f <- c('x','-y','z')
  x <- curl(f, var = c('x','y','z'))
  y <- c("0","0","0")
  expect_equal(x,y)
})

test_that("202012141845", {
  f <- function(x,y,z) c(x*y,y*z,x*z)
  x <- curl(f, var = c('x'=1,'y'=2,'z'=3))
  y <- c(-2,-3,-1)
  expect_equal(x,y)
})

test_that("202012141846", {
  f <- function(x,y,z, extra) if(extra) c(x*y,y*z,x*z)
  x <- curl(f, var = c('x'=1,'y'=2,'z'=3), params = list(extra = TRUE))
  y <- c(-2,-3,-1)
  expect_equal(x,y)
})

test_that("202012141847", {
  f <- function(r, phi, z, k) c(0, r^-2, k)
  x <- curl(f, var = c(r = 2, phi = -30, z = 40), params = list(k = 100), coordinates = "cylindrical")
  y <- c(0,0,-1/8)
  expect_equal(x,y)
})

test_that("202012141856", {
  f <- function(x, k) c(0, x[1]^-2, k)
  x <- curl(f, var = c(2, -30, 40), params = list(k = 100), coordinates = "cylindrical")
  y <- c(0,0,-1/8)
  expect_equal(x,y)
})

test_that("202012141905", {
  f <- c('0', 'r^-2', 'k')
  x <- curl(f, var = c(r = 2, phi = -30, z = 40), coordinates = "cylindrical")
  y <- c(0,0,-1/8)
  expect_equal(x,y)
})

test_that("202012141906", {
  f <- c('0', 'r^-2', 'k')
  x <- curl(f, var = c('r' = 2, 'phi' = -30, 'z' = 40), coordinates = "cylindrical")
  y <- c(0,0,-1/8)
  expect_equal(x,y)
})

test_that("202012141907", {
  f <- c('0', 'r^-2', 'k')
  x <- curl(f, var = c('r' = 2, 'phi' = -30, 'z' = 40), coordinates = "cylindrical", drop = FALSE)
  y <- array(c(0,0,-1/8))
  expect_equal(x,y)
})

test_that("202012141912", {
  f <- c('0', '0', 'k')
  x <- curl(f, var = c('r', 'phi', 'z'), coordinates = "cylindrical")
  y <- c("0","0","0")
  expect_equal(x,y)
})

test_that("202012141921", {
  f <- c('r^3', 'r*z', 'r*z*sin(phi)')
  x <- curl(f, var = c('r'=2, 'phi'=3, 'z'=4), coordinates = "cylindrical")
  y <- function(r,phi,z) c(z*cos(phi)-r, -z*(sin(phi)), 2*z)
  expect_equal(x,y(2,3,4))
})

test_that("202012141924", {
  f <- function(r, phi, z) c(r^3, r*z, r*z*sin(phi))
  x <- curl(f, var = c('r'=2, 'phi'=3, 'z'=4), coordinates = "cylindrical")
  y <- function(r,phi,z) c(z*cos(phi)-r, -z*(sin(phi)), 2*z)
  expect_equal(x,y(2,3,4))
})

test_that("202012141926", {
  f <- function(x) c(x[1]^3+x[1]*x[2]^2-x[2]*x[3],x[1]^2*x[2]+x[2]^3+x[1]*x[3],x[2]*x[3])
  x <- curl(f, var = c(2,3,4))
  y <- function(x,y,z) c(z-x, -y, 2*z)
  expect_equal(x,y(2,3,4))
})

test_that("202012141930", {
  f.sym <- array(c('r^4','r^2*cos(theta)','0','sin(theta)','0','sin(theta)^2'), dim = c(2,3)) 
  f.num <- function(r,theta,phi) array(c(r^4,r^2*cos(theta),0,sin(theta),0,sin(theta)^2), dim = c(2,3)) 
  x.sym <- curl(f.sym, var = c(r=2,theta=3,phi=4), coordinates = "spherical")
  x.num <- curl(f.num, var = c(r=2,theta=3,phi=4), coordinates = "spherical")
  y <- function(r,theta,phi) array(c(0,3*sin(theta)*cos(theta)/r,0,-sin(theta)^2/r,0,(1+r^2)/r*sin(theta)), dim = c(2,3))
  expect_equal(x.sym,y(2,3,4)) & expect_equal(x.num,y(2,3,4))
})

test_that("202012142000", {
  f.sym <- array(c('r^4','r^2*cos(theta)','0','sin(theta)','0','sin(theta)^2'), dim = c(2,3)) 
  f.num <- function(r,theta,phi) array(c(r^4,r^2*cos(theta),0,sin(theta),0,sin(theta)^2), dim = c(2,3)) 
  x.sym <- curl(f.sym, var = c(r=2,theta=3,phi=4))
  x.num <- curl(f.num, var = c(r=2,theta=3,phi=4))
  expect_equal(x.sym,x.num)
})

test_that("202012142333", {
  f.sym <- c('sqrt(r)/10','sqrt(r)') 
  f.num <- function(r,theta) c(sqrt(r)/10, sqrt(r))
  x.sym <- curl(f.sym, var = c('r'=2,'theta'=3), coordinates = "spherical")
  x.num <- curl(f.num, var = c(r=2,theta=3), coordinates = "spherical")
  expect_equal(x.sym,x.num)
})


test_that("202012302351", {
  f <- c('x*y','y*z','x*z*a')
  x <- curl(f, var = c('x'=1,'y'=2,'z'=3), params = list(a = 1))
  y <- c(-2,-3,-1)
  expect_equal(x,y)
})
