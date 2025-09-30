library(mobilizr)
context("Test head/tail functions")

test_that("head works", {
  expect_identical(head(1:10), 1:6)
  expect_identical(head(~height, data = cdc), c(1.73, 1.50, 1.90, NA, 1.63, 1.70))
})

test_that("tail works", {
  expect_identical(tail(1:10), 5:10)
  expect_identical(tail(~height, data = cdc), c(1.70, 1.60, 1.60, 1.27, 1.80, 1.78))
})
