test_that("202012281517", {
  x <- ode("x", c(x = 10), seq(0, 1, by = 0.001), drop = TRUE)
  y <- c(x = 10*exp(1))
  expect_equal(x, y)
})

test_that("202012281518", {
  x <- ode("a*x", c(x = 1), seq(0, 1, by = 0.001), params = list(a = 2), drop = TRUE)
  y <- c(x = exp(2))
  expect_equal(x, y)
})

test_that("202012281519", {
  x <- ode("x*t/100", c(x = 0.1), seq(1,10,0.001), timevar = "t", drop = TRUE)
  y <- c(x = 0.1640498)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281520", {
  x <- ode("x*t/n", c(x = 0.1), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE)
  y <- c(x = 0.1640498)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281521", {
  f <- function(x) x
  x <- ode(f, c(x = 10), seq(0, 1, by = 0.001), drop = TRUE)
  y <- c(x = 10*exp(1))
  expect_equal(x, y)
})

test_that("202012281522", {
  f <- function(x, a) a*x
  x <- ode(f, c(x = 1), seq(0, 1, by = 0.001), params = list(a = 2), drop = TRUE)
  y <- c(x = exp(2))
  expect_equal(x, y)
})

test_that("202012281523", {
  f <- function(x, t) x*t/100
  x <- ode(f, c(x = 0.1), seq(1,10,0.001), timevar = "t", drop = TRUE)
  y <- c(x = 0.1640498)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281524", {
  f <- function(x, t, n) x*t/n
  x <- ode(f, c(x = 0.1), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE)
  y <- c(x = 0.1640498)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281525", {
  f <- function(x) x
  x <- ode(f, 10, seq(0, 1, by = 0.001), drop = TRUE)
  y <- 10*exp(1)
  expect_equal(x, y)
})

test_that("202012281526", {
  f <- function(x, a) a*x
  x <- ode(f, 1, seq(0, 1, by = 0.001), params = list(a = 2), drop = TRUE)
  y <- exp(2)
  expect_equal(x, y)
})

test_that("202012281527", {
  f <- function(x, t) x*t/100
  x <- ode(f, 0.1, seq(1,10,0.001), timevar = "t", drop = TRUE)
  y <- 0.1640498
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281528", {
  f <- function(x, t, n) x*t/n
  x <- ode(f, 0.1, seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE)
  y <- 0.1640498
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281529", {
  x <- ode(c("x","x*y"), c(x=1, y=1), seq(0, 1, by = 0.001), drop = TRUE)
  y <- c(x = exp(1), y = 5.574942)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281530", {
  x <- ode(c("a*x","x*y"), c(x=1, y=1), seq(0, 1, by = 0.001), params = list(a = 1), drop = TRUE)
  y <- c(x = exp(1), y = 5.574942)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281531", {
  x <- ode(c("x*t/100","t"), c(x = 0.1, y = 0), seq(1,10,0.001), timevar = "t", drop = TRUE)
  y <- c(x = 0.1640498, y = 49.5)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281532", {
  x <- ode(c("x*t/n","t*n"), c(x = 0.1, y = 0), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE)
  y <- c(x = 0.1640498, y = 4950)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281533", {
  f <- function(y, x) c(x, x*y)
  x <- ode(f, c(x=1, y=1), seq(0, 1, by = 0.001), drop = TRUE)
  y <- c(x = exp(1), y = 5.574942)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281534", {
  f <- function(x, y, a) c(a*x,x*y) 
  x <- ode(f, c(x=1, y=1), seq(0, 1, by = 0.001), params = list(a = 1), drop = TRUE)
  y <- c(x = exp(1), y = 5.574942)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281535", {
  f <- function(y, x, t) c(x*t/100, t)
  x <- ode(f, c(x = 0.1, y = 0), seq(1,10,0.001), timevar = "t", drop = TRUE)
  y <- c(x = 0.1640498, y = 49.5)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281536", {
  f <- function(x, y, t, n) c(x*t/n,t*n)
  x <- ode(f, c(x = 0.1, y = 0), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE)
  y <- c(x = 0.1640498, y = 4950)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281537", {
  f <- function(x) c(x[1], x[1]*x[2])
  x <- ode(f, c(1, 1), seq(0, 1, by = 0.001), drop = TRUE)
  y <- c(exp(1), 5.574942)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281538", {
  f <- function(x, a) c(a*x[1],x[1]*x[2]) 
  x <- ode(f, c(1, 1), seq(0, 1, by = 0.001), params = list(a = 1), drop = TRUE)
  y <- c(exp(1), 5.574942)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281539", {
  f <- function(x, t) c(x[1]*t/100, t)
  x <- ode(f, c(0.1, 0), seq(1,10,0.001), timevar = "t", drop = TRUE)
  y <- c(0.1640498, 49.5)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281540", {
  f <- function(x, t, n) c(x[1]*t/n,t*n)
  x <- ode(f, c(0.1, 0), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE)
  y <- c(0.1640498, 4950)
  expect_equal(x, y, tolerance = 1e-7)
})

test_that("202012281541", {
  x <- ode("x", c(x = 10), seq(0, 1, by = 0.0001), drop = TRUE, method = "euler")
  y <- c(x = 10*exp(1))
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281542", {
  x <- ode("a*x", c(x = 1), seq(0, 1, by = 0.00001), params = list(a = 2), drop = TRUE, method = "euler")
  y <- c(x = exp(2))
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281543", {
  x <- ode("x*t/100", c(x = 0.1), seq(1,10,0.0001), timevar = "t", drop = TRUE, method = "euler")
  y <- c(x = 0.1640498)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281544", {
  x <- ode("x*t/n", c(x = 0.1), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE, method = "euler")
  y <- c(x = 0.1640498)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281545", {
  f <- function(x) x
  x <- ode(f, c(x = 10), seq(0, 1, by = 0.0001), drop = TRUE, method = "euler")
  y <- c(x = 10*exp(1))
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281546", {
  f <- function(x, a) a*x
  x <- ode(f, c(x = 1), seq(0, 1, by = 0.00001), params = list(a = 2), drop = TRUE, method = "euler")
  y <- c(x = exp(2))
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281547", {
  f <- function(x, t) x*t/100
  x <- ode(f, c(x = 0.1), seq(1,10,0.001), timevar = "t", drop = TRUE, method = "euler")
  y <- c(x = 0.1640498)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281548", {
  f <- function(x, t, n) x*t/n
  x <- ode(f, c(x = 0.1), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE, method = "euler")
  y <- c(x = 0.1640498)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281549", {
  f <- function(x) x
  x <- ode(f, 10, seq(0, 1, by = 0.00001), drop = TRUE, method = "euler")
  y <- 10*exp(1)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281550", {
  f <- function(x, a) a*x
  x <- ode(f, 1, seq(0, 1, by = 0.00001), params = list(a = 2), drop = TRUE, method = "euler")
  y <- exp(2)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281551", {
  f <- function(x, t) x*t/100
  x <- ode(f, 0.1, seq(1,10,0.001), timevar = "t", drop = TRUE, method = "euler")
  y <- 0.1640498
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281552", {
  f <- function(x, t, n) x*t/n
  x <- ode(f, 0.1, seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE, method = "euler")
  y <- 0.1640498
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281553", {
  x <- ode(c("x","x*y"), c(x=1, y=1), seq(0, 1, by = 0.00001), drop = TRUE, method = "euler")
  y <- c(x = exp(1), y = 5.574942)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281554", {
  x <- ode(c("a*x","x*y"), c(x=1, y=1), seq(0, 1, by = 0.00001), params = list(a = 1), drop = TRUE, method = "euler")
  y <- c(x = exp(1), y = 5.574942)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281555", {
  x <- ode(c("x*t/100","t"), c(x = 0.1, y = 0), seq(1,10,0.001), timevar = "t", drop = TRUE, method = "euler")
  y <- c(x = 0.1640498, y = 49.5)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281556", {
  x <- ode(c("x*t/n","t*n"), c(x = 0.1, y = 0), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE, method = "euler")
  y <- c(x = 0.1640498, y = 4950)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281557", {
  f <- function(y, x) c(x, x*y)
  x <- ode(f, c(x=1, y=1), seq(0, 1, by = 0.00001), drop = TRUE, method = "euler")
  y <- c(x = exp(1), y = 5.574942)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281558", {
  f <- function(x, y, a) c(a*x,x*y) 
  x <- ode(f, c(x=1, y=1), seq(0, 1, by = 0.00001), params = list(a = 1), drop = TRUE, method = "euler")
  y <- c(x = exp(1), y = 5.574942)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281559", {
  f <- function(y, x, t) c(x*t/100, t)
  x <- ode(f, c(x = 0.1, y = 0), seq(1,10,0.001), timevar = "t", drop = TRUE, method = "euler")
  y <- c(x = 0.1640498, y = 49.5)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281600", {
  f <- function(x, y, t, n) c(x*t/n,t*n)
  x <- ode(f, c(x = 0.1, y = 0), seq(1,10,0.001), timevar = "t", params = list(n = 100), drop = TRUE, method = "euler")
  y <- c(x = 0.1640498, y = 4950)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281601", {
  f <- function(x) c(x[1], x[1]*x[2])
  x <- ode(f, c(1, 1), seq(0, 1, by = 0.00001), drop = TRUE, method = "euler")
  y <- c(exp(1), 5.574942)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281602", {
  f <- function(x, a) c(a*x[1],x[1]*x[2]) 
  x <- ode(f, c(1, 1), seq(0, 1, by = 0.00001), params = list(a = 1), drop = TRUE, method = "euler")
  y <- c(exp(1), 5.574942)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281603", {
  f <- function(x, t) c(x[1]*t/100, t)
  x <- ode(f, c(0.1, 0), seq(1,10,0.0001), timevar = "t", drop = TRUE, method = "euler")
  y <- c(0.1640498, 49.5)
  expect_equal(x, y, tolerance = 1e-4)
})

test_that("202012281604", {
  f <- function(x, t, n) c(x[1]*t/n,t*n)
  x <- ode(f, c(0.1, 0), seq(1,10,0.0001), timevar = "t", params = list(n = 100), drop = TRUE, method = "euler")
  y <- c(0.1640498, 4950)
  expect_equal(x, y, tolerance = 1e-4)
})

