library(mobilizr)
context("Test timeuse_format")

timeuse <- read.csv("data/timeuse_sample.csv")
timeuse_formatted <- timeuse_format(timeuse)

test_that("timeuse_format works", {
  expect_equal(dim(timeuse_formatted), c(34, 16))
  expect_equal(round(mean(timeuse_formatted$school)), 429)
})

rm(timeuse)
rm(timeuse_formatted)
