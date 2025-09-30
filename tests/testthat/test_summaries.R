library(mobilizr)
context("Test summary functions")

values <- c(1:10, NA)

test_that("iqr function works", {
  expect_equal(iqr(values), 4.5)
  expect_warning(iqr(values), "The data contains missing values")
  expect_equal(iqr(~height, data = cdc), 0.15)
  expect_equal(iqr(~height | gender, data = cdc), c(Female = 0.11, Male = 0.10))
})

test_that("IQR function works", {
  expect_equal(IQR(values), 4.5)
  expect_warning(IQR(values), "The data contains missing values")
  expect_equal(IQR(~height, data = cdc), 0.15)
  expect_equal(IQR(~height | gender, data = cdc), c(Female = 0.11, Male = 0.10))
})

test_that("SAD function works", {
  expect_equal(SAD(values), 25)
  expect_warning(SAD(values), "The data contains missing values")
  expect_equal(round(SAD(~height, data = cdc), digits = 3), 1214.76)
  expect_equal(round(SAD(~height | gender, data = cdc), digits = 4), c(Female = 428.1610, Male = 491.0038))
})

test_that("MAD function works", {
  expect_equal(MAD(values), 2.5)
  expect_warning(MAD(values), "The data contains missing values")
  expect_equal(round(MAD(~height, data = cdc), digits = 8), 0.08460508)
  expect_equal(round(MAD(~height | gender, data = cdc), digits = 8), c(Female = 0.05992456, Male = 0.06807206))
})

test_that("max function works", {
  expect_true(is.na(max(values)))
  expect_warning(max(~height, data = cdc), "The data contains missing values")
  expect_equal(max(~height, data = cdc), 2.11)
  expect_equal(max(~height | gender, data = cdc), c(Female = 1.98, Male = 2.11))
})

test_that("mean function works", {
  expect_equal(mean(values), 5.5)
  expect_warning(mean(values), "The data contains missing values")
  expect_equal(round(mean(~height, data = cdc), digits = 6), 1.687064)
  expect_equal(round(mean(~height | gender, data = cdc), digits = 6), c(Female = 1.621450, Male = 1.752059))
})

test_that("median function works", {
  expect_equal(median(values), 5.5)
  expect_warning(median(values), "The data contains missing values")
  expect_equal(median(~height, data = cdc), 1.68)
  expect_equal(median(~height | gender, data = cdc), c(Female = 1.63, Male = 1.75))
})

test_that("min function works", {
  expect_true(is.na(min(values)))
  expect_warning(min(~height, data = cdc), "The data contains missing values")
  expect_equal(min(~height, data = cdc), 1.27)
  expect_equal(min(~height | gender, data = cdc), c(Female = 1.30, Male = 1.27))
})

test_that("prod function works", {
  expect_equal(prod(values), 3628800)
  expect_warning(prod(values), "The data contains missing values")
  expect_equal(round(prod(~height, data = head(cdc)), digits = 5), 13.66242)
  expect_equal(round(prod(~height | gender, data = head(cdc)), digits = 6), c(Female = 2.4450, Male = 5.5879))
})

test_that("quantile function works", {
  expect_equal(quantile(values), c("0%" = 1, "25%" = 3.25, "50%" = 5.5, "75%" = 7.75, "100%" = 10))
  expect_warning(quantile(~height, data = cdc), "The data contains missing values")
  expect_equal(quantile(~height, data = cdc), c("0%" = 1.27, "25%" = 1.60, "50%" = 1.68, "75%" = 1.75, "100%" = 2.11))
  expect_equal(unname(quantile(~height | gender, data = cdc)), c(1.30, 1.57, 1.63, 1.68, 1.98, 1.27, 1.70, 1.75, 1.80, 2.11))
})

test_that("var function works", {
  expect_equal(var(values), 9.1666666)
  expect_warning(var(~height, data = cdc), "The data contains missing values")
  expect_equal(var(~height, data = cdc), 0.0108521)
  expect_equal(unname(var(~height | gender, data = cdc)), c(0.005655064, 0.007511526))
})

# sd
test_that("sd function works", {
  expect_equal(sd(values), 3.02765, tolerance = 1e5)
  expect_warning(sd(~height, data = cdc), "The data contains missing values")
  expect_equal(sd(~height, data = cdc), 0.1041734, tolerance = 1e5)
  expect_equal(unname(sd(~height | gender, data = cdc)), c(0.07520016, 0.08666906))
})

# sum
test_that("sum function works", {
  expect_equal(sum(values), 55)
  expect_warning(sum(~height, data = cdc), "The data contains missing values")
  expect_equal(sum(~height, data = cdc), 24222.86)
  expect_equal(unname(sum(~height | gender, data = cdc)), c(11585.26, 12637.60))
})


# range
test_that("range function works", {
  expect_equal(range(values), c(1, 10))
  expect_warning(range(~height, data = cdc), "The data contains missing values")
  expect_equal(range(~height, data = cdc), c(1.27, 2.11))
  expect_equal(unname(range(~height | gender, data = cdc)), c(1.30, 1.98, 1.27, 2.11))
})

rm(values)
