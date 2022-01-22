test_that("202012272202", {
  f <- "x"
  x <- integral(f, bounds = list(x = c(0,1)))
  y <- 0.5
  expect_equal(x$value, y)
})

test_that("202012272203", {
  f <- "x"
  x <- integral(f, bounds = list(x = c(1,0)))
  y <- -0.5
  expect_equal(x$value, y)
})

test_that("202012272204", {
  f <- function(x) x
  x <- integral(f, bounds = list(x = c(1,0)))
  y <- -0.5
  expect_equal(x$value, y)
})

test_that("202012272205", {
  f <- function(x) x
  x <- integral(f, bounds = list(x = c(1,0)), vectorize = TRUE)
  y <- -0.5
  expect_equal(x$value, y)
})

test_that("202012272206", {
  f <- function(x) c(x, x^2)
  x <- integral(f, bounds = list(x = c(0,1)))
  y <- c(0.5, 1/3)
  expect_equal(x$value, y)
})

test_that("202012272207", {
  f <- function(x) c(x, x^2)
  x <- integral(f, bounds = list(x = c(0,1)), vectorize = TRUE)
  y <- c(0.5, 1/3)
  expect_equal(x$value, y)
})

test_that("202012272208", {
  f <- function(x) array(c(x, x^2))
  x <- integral(f, bounds = list(x = c(0,1)), drop = FALSE)
  y <- array(c(0.5, 1/3))
  expect_equal(x$value, y)
})

test_that("202012272209", {
  f <- array(c('x', 'x^2'))
  x <- integral(f, bounds = list(x = c(0,1)), drop = FALSE)
  y <- array(c(0.5, 1/3))
  expect_equal(x$value, y)
})

test_that("202012272210", {
  f <- function(x) array(c(x, x, x^2, x/2), dim = c(2,2))
  x <- integral(f, bounds = list(x = c(0,1)))
  y <- array(c(0.5, 0.5, 1/3, 0.25), dim = c(2,2))
  expect_equal(x$value, y)
})

test_that("202012272211", {
  f <- array(c('x', 'x', 'x^2', 'x/2'), dim = c(2,2))
  x <- integral(f, bounds = list(x = c(0,1)))
  y <- array(c(0.5, 0.5, 1/3, 0.25), dim = c(2,2))
  expect_equal(x$value, y)
})

test_that("202012272212", {
  f <- function(x, extra) if(extra) array(c(x, x, x^2, x/2), dim = c(2,2))
  x <- integral(f, bounds = list(x = c(0,1), extra = TRUE))
  y <- array(c(0.5, 0.5, 1/3, 0.25), dim = c(2,2))
  expect_equal(x$value, y)
})

test_that("202012272213", {
  f <- array(c('x*y', 'x', 'x^2', 'x/2'), dim = c(2,2))
  x <- integral(f, bounds = list(x = c(0,1), y = c(0,1)))
  y <- array(c(0.25, 0.5, 1/3, 0.25), dim = c(2,2))
  expect_equal(x$value, y)
})

test_that("202012272214", {
  f <- 1
  x <- integral(f, bounds = list(x = c(0,2), theta = c(0,2*pi)), coordinates = "polar")
  y <- pi*2^2
  expect_equal(x$value, y)
})

test_that("202012272214", {
  f <- function(x, theta) 1
  x <- integral(f, bounds = list(x = c(0,2), theta = c(0,2*pi)), coordinates = "polar")
  y <- pi*2^2
  expect_equal(x$value, y)
})

test_that("202012272215", {
  f.num <- function(x, theta) x*theta
  f.sym <- 'x*theta'
  x.num <- integral(f.num, bounds = list(x = c(0,2), theta = c(0,2*pi)), coordinates = "polar")
  x.sym <- integral(f.sym, bounds = list(x = c(0,2), theta = c(0,2*pi)), coordinates = "polar")
  expect_equal(x.num$value, x.sym$value)
})

test_that("202012272216", {
  f <- 1
  x <- integral(f, bounds = list(x = c(0,2), theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical")
  y <- 4/3*pi*2^3
  expect_equal(x$value, y, tolerance = abs(max(x$error/x$value)))
})

test_that("202012272217", {
  f <- 1
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical")
  y <- 4*pi*2^2
  expect_equal(x$value, y, tolerance = abs(max(x$error/x$value)))
})

test_that("202012272218", {
  f <- function(x, theta, phi) 1
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical")
  y <- 4*pi*2^2
  expect_equal(x$value, y, tolerance = abs(max(x$error/x$value)))
})

test_that("202012272218", {
  f <- function(x, theta, phi) 1
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", vectorize = TRUE)
  y <- 4*pi*2^2
  expect_equal(x$value, y, tolerance = abs(max(x$error/x$value)))
})

test_that("202012272219", {
  f <- function(x, theta, phi) c(1, 2)
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical")
  y <- c(4*pi*2^2, 8*pi*2^2)
  expect_equal(x$value, y, tolerance = abs(max(x$error/x$value)))
})

test_that("202012272220", {
  f <- function(x, theta, phi) rep(c(1, 2), each = length(theta))
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", vectorize = TRUE)
  y <- c(4*pi*2^2, 8*pi*2^2)
  expect_equal(x$value, y, tolerance = abs(max(x$error/x$value)))
})

test_that("202012272221", {
  f <- c(1, 2)
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", vectorize = TRUE)
  y <- c(4*pi*2^2, 8*pi*2^2)
  expect_equal(x$value, y, tolerance = abs(max(x$error/x$value)))
})

test_that("202012272222", {
  set.seed(123)
  f <- "x"
  x <- integral(f, bounds = list(x = c(0,1)), relTol = 1e-2, method = "mc")
  y <- 0.5
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272223", {
  set.seed(123)
  f <- "x"
  x <- integral(f, bounds = list(x = c(1,0)), relTol = 1e-2, method = "mc")
  y <- -0.5
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272224", {
  set.seed(123)
  f <- function(x) x
  x <- integral(f, bounds = list(x = c(1,0)), relTol = 1e-2, method = "mc")
  y <- -0.5
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272225", {
  set.seed(123)
  f <- function(x) x
  x <- integral(f, bounds = list(x = c(1,0)), relTol = 1e-2, method = "mc", vectorize = TRUE)
  y <- -0.5
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272226", {
  set.seed(123)
  f <- function(x) c(x, x^2)
  x <- integral(f, bounds = list(x = c(0,1)), relTol = 1e-2, method = "mc")
  y <- c(0.5, 1/3)
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272227", {
  set.seed(123)
  f <- function(x) c(x, x^2)
  x <- integral(f, bounds = list(x = c(0,1)), relTol = 1e-2, method = "mc", vectorize = TRUE)
  y <- c(0.5, 1/3)
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272228", {
  set.seed(123)
  f <- function(x) array(c(x, x^2))
  x <- integral(f, bounds = list(x = c(0,1)), drop = FALSE, relTol = 1e-2, method = "mc")
  y <- array(c(0.5, 1/3))
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272229", {
  set.seed(123)
  f <- array(c('x', 'x^2'))
  x <- integral(f, bounds = list(x = c(0,1)), drop = FALSE, relTol = 1e-2, method = "mc")
  y <- array(c(0.5, 1/3))
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272230", {
  set.seed(123)
  f <- function(x) array(c(x, x, x^2, x/2), dim = c(2,2))
  x <- integral(f, bounds = list(x = c(0,1)), relTol = 1e-2, method = "mc")
  y <- array(c(0.5, 0.5, 1/3, 0.25), dim = c(2,2))
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272231", {
  set.seed(123)
  f <- array(c('x', 'x', 'x^2', 'x/2'), dim = c(2,2))
  x <- integral(f, bounds = list(x = c(0,1)), relTol = 1e-2, method = "mc")
  y <- array(c(0.5, 0.5, 1/3, 0.25), dim = c(2,2))
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272232", {
  set.seed(123)
  f <- function(x, extra) if(extra) array(c(x, x, x^2, x/2), dim = c(2,2))
  x <- integral(f, bounds = list(x = c(0,1), extra = TRUE), relTol = 1e-2, method = "mc")
  y <- array(c(0.5, 0.5, 1/3, 0.25), dim = c(2,2))
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272233", {
  set.seed(123)
  f <- array(c('x*y', 'x', 'x^2', 'x/2'), dim = c(2,2))
  x <- integral(f, bounds = list(x = c(0,1), y = c(0,1)), relTol = 1e-2, method = "mc")
  y <- array(c(0.25, 0.5, 1/3, 0.25), dim = c(2,2))
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272234", {
  set.seed(123)
  f <- 1
  x <- integral(f, bounds = list(x = c(0,2), theta = c(0,2*pi)), coordinates = "polar", relTol = 1e-2, method = "mc")
  y <- pi*2^2
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272235", {
  set.seed(123)
  f <- function(x, theta) 1
  x <- integral(f, bounds = list(x = c(0,2), theta = c(0,2*pi)), coordinates = "polar", relTol = 1e-2, method = "mc")
  y <- pi*2^2
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272236", {
  set.seed(123)
  f.num <- function(x, theta) x*theta
  f.sym <- 'x*theta'
  x.num <- integral(f.num, bounds = list(x = c(0,2), theta = c(0,2*pi)), coordinates = "polar", relTol = 1e-2, method = "mc")
  x.sym <- integral(f.sym, bounds = list(x = c(0,2), theta = c(0,2*pi)), coordinates = "polar", relTol = 1e-2, method = "mc")
  expect_equal(x.num$value, x.sym$value, tolerance = 3*abs(max(x.num$error/x.num$value)))
})

test_that("202012272237", {
  set.seed(123)
  f <- 1
  x <- integral(f, bounds = list(x = c(0,2), theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", relTol = 1e-2, method = "mc")
  y <- 4/3*pi*2^3
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272238", {
  set.seed(123)
  f <- 1
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", relTol = 1e-2, method = "mc")
  y <- 4*pi*2^2
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272239", {
  set.seed(123)
  f <- function(x, theta, phi) 1
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", relTol = 1e-2, method = "mc")
  y <- 4*pi*2^2
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272240", {
  set.seed(123)
  f <- function(x, theta, phi) 1
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", relTol = 1e-2, method = "mc", vectorize = TRUE)
  y <- 4*pi*2^2
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272241", {
  set.seed(123)
  f <- function(x, theta, phi) c(1, 2)
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", relTol = 1e-2, method = "mc")
  y <- c(4*pi*2^2, 8*pi*2^2)
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272242", {
  set.seed(123)
  f <- function(x, theta, phi) rep(c(1, 2), each = length(theta))
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", relTol = 1e-2, method = "mc", vectorize = TRUE)
  y <- c(4*pi*2^2, 8*pi*2^2)
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012272243", {
  set.seed(123)
  f <- c(1, 2)
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), coordinates = "spherical", relTol = 1e-2, method = "mc", vectorize = TRUE)
  y <- c(4*pi*2^2, 8*pi*2^2)
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012310003", {
  set.seed(123)
  f <- c(1, "2*a")
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), params = list(a = 1), coordinates = "spherical", relTol = 1e-2, method = "mc", vectorize = TRUE)
  y <- c(4*pi*2^2, 8*pi*2^2)
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})

test_that("202012310004", {
  set.seed(123)
  f <- c(1, "2*a")
  x <- integral(f, bounds = list(x = 2, theta = c(0,pi), phi = c(0,2*pi)), params = list(a = 1), coordinates = "spherical", relTol = 1e-2, vectorize = TRUE)
  y <- c(4*pi*2^2, 8*pi*2^2)
  expect_equal(x$value, y, tolerance = 3*abs(max(x$error/x$value)))
})
