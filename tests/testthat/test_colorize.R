library(mobilizr)
context("colorize function")

mt_colors <- dplyr::mutate(mtcars, colors = colorize(cyl))
df <- data.frame(l = as.factor(letters[1:13]))

test_that("colorize works", {
  expect_equal(head(mt_colors$colors), c("#1F78B4", "#1F78B4", "#A6CEE3", "#1F78B4", "#B2DF8A", "#1F78B4"))
  expect_error(mutate(df, colors = colorize(l)))
})
