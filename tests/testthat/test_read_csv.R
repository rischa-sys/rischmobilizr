library(mobilizr)
context("Test read.csv")

timeuse <- read.csv("data/timeuse_sample.csv")

test_that("read.csv works", {
  expect_equal(length(which(names(timeuse) != c('user.id', 'timestamp', 'activities', 'chores', 'day', 'friends', 'grooming', 'homework', 'meals', 'online', 'read', 'school', 'sleep', 'sports', 'television', 'travel', 'videogames', 'work', 'latitude', 'longitude'))), 0)
})

rm(timeuse)
