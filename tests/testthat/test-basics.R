test_that("202012271603", {
  x <- "0"
  y <- "0"
  z <- cpp_paste(x, y, sep = " + ")
  expect_equal(z, "0")
})

test_that("202012271604", {
  x <- "0"
  y <- letters[1:2]
  z <- cpp_paste(x, y, sep = " + ")
  expect_equal(z, y)
})

test_that("202012271605", {
  x <- letters[1:2]
  y <- "0"
  z <- cpp_paste(x, y, sep = " + ")
  expect_equal(z, x)
})

test_that("202012271606", {
  x <- letters[1:2]
  y <- letters[1:2]
  z <- cpp_paste(x, y, sep = " + ")
  expect_equal(z, c("a + a", "b + b"))
})

test_that("202012271607", {
  x <- c("0", letters[1:2], "0")
  y <- c(letters[1:2], "0", "0")
  z <- cpp_paste(x, y, sep = " + ")
  expect_equal(z, c("a", "a + b", "b", "0"))
})

test_that("202012271608", {
  x <- "0"
  y <- "0"
  z <- cpp_paste(x, y, sep = " - ")
  expect_equal(z, "0")
})

test_that("202012271609", {
  x <- "0"
  y <- letters[1:2]
  z <- cpp_paste(x, y, sep = " - ")
  expect_equal(z, c(" - a", " - b"))
})

test_that("202012271610", {
  x <- letters[1:2]
  y <- "0"
  z <- cpp_paste(x, y, sep = " - ")
  expect_equal(z, x)
})

test_that("202012271611", {
  x <- letters[1:2]
  y <- letters[1:2]
  z <- cpp_paste(x, y, sep = " - ")
  expect_equal(z, c("0", "0"))
})

test_that("202012271612", {
  x <- c("0", letters[1:2], "0")
  y <- c(letters[1:2], "0", "0")
  z <- cpp_paste(x, y, sep = " - ")
  expect_equal(z, c(" - a", "a - b", "b", "0"))
})

test_that("202012271613", {
  x <- "0"
  y <- "0"
  z <- cpp_paste(x, y, sep = " * ")
  expect_equal(z, "0")
})

test_that("202012271614", {
  x <- "0"
  y <- letters[1:2]
  z <- cpp_paste(x, y, sep = " * ")
  expect_equal(z, c("0", "0"))
})

test_that("202012271615", {
  x <- letters[1:2]
  y <- "0"
  z <- cpp_paste(x, y, sep = " * ")
  expect_equal(z, c("0", "0"))
})

test_that("202012271616", {
  x <- letters[1:2]
  y <- letters[1:2]
  z <- cpp_paste(x, y, sep = " * ")
  expect_equal(z, c("a * a", "b * b"))
})

test_that("202012271617", {
  x <- c("0", letters[1:2], "0")
  y <- c(letters[1:2], "0", "0")
  z <- cpp_paste(x, y, sep = " * ")
  expect_equal(z, c("0", "a * b", "0", "0"))
})

test_that("202012271618", {
  x <- "0"
  y <- "0"
  z <- cpp_paste(x, y, sep = " / ")
  expect_equal(z, "0")
})

test_that("202012271619", {
  x <- "0"
  y <- letters[1:2]
  z <- cpp_paste(x, y, sep = " / ")
  expect_equal(z, c("0", "0"))
})

test_that("202012271620", {
  x <- letters[1:2]
  y <- "0"
  z <- cpp_paste(x, y, sep = " / ")
  expect_equal(z, c("a / 0", "b / 0"))
})

test_that("202012271621", {
  x <- letters[1:2]
  y <- letters[1:2]
  z <- cpp_paste(x, y, sep = " / ")
  expect_equal(z, c("1", "1"))
})

test_that("202012271622", {
  x <- c("0", letters[1:2], "0")
  y <- c(letters[1:2], "0", "0")
  z <- cpp_paste(x, y, sep = " / ")
  expect_equal(z, c("0", "a / b", "b / 0", "0"))
})
