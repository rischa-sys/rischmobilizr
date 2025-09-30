library(mobilizr)
context("Test tree and treeplot functions")

# Create models
simple_tree <- tree(survived ~ age + embarked + class, data = titanic)
complex_tree <- tree(survived ~ age + embarked + class, data = titanic, cp = 0.005, minsplit = 5)
overly_complex_tree <- tree(survived ~ age + embarked + class, data = titanic, cp = 0.001, minsplit = 5)

# Load plots
load("data/trees.rda")

.tree_splits <- function(x) {
  # A helper function to determine whether the model has too many branches to plot.
  cp_table <- x["cptable"]
  cp_df <- as.data.frame(cp_table)
  splits <- cp_df$cptable.nsplit
  return(splits)
}

test_that("tree models work", {
  expect_equal(max(.tree_splits(x = simple_tree)), 3)
  expect_equal(max(.tree_splits(x = complex_tree)), 14)
  expect_equal(max(.tree_splits(x = overly_complex_tree)), 54)
})

test_that("treeplots work", {
  expect_equal(treeplot(simple_tree)$obj$frame, simple_df)
  expect_equal(treeplot(complex_tree)$obj$frame, complex_df)
  expect_error(treeplot(overly_complex_tree))
})

rm(simple_tree)
rm(complex_tree)
rm(overly_complex_tree)
rm(.tree_splits)
rm(simple_df)
rm(complex_df)
