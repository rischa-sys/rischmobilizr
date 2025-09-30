library(mobilizr)
context("Test load_labs")

test_that("Load lab breaks", {
  expect_error(load_lab(0))
  expect_error(load_lab("1A"))
  expect_error(load_labs(0))
  expect_error(load_labs("1A"))
})
