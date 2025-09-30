library(mobilizr)
context("do function")

df <- do(5) * mean(~height | shuffle(gender), data = cdc)

test_that("do function works", {
  expect_error(do(501) * mean(1:10))
  expect_equal(names(df), c("Female", "Male"))
  expect_equal(class(df), c("do.data.frame", "data.frame"))
})

rm(df)
