library(mobilizr)
has_visualTest <- require("visualTest")

if(isTRUE(has_visualTest)){

  library(visualTest)
  context("Test plot functions")

  # Functions for add_curve and add_line
  test_add_curve <- function() {
    m_line <- lm(weight ~ height, data = cdc)
    m_curve <- lm(weight ~ poly(height, 2), data = cdc)
    print(xyplot(weight ~ height, data = cdc))
    print(add_curve(m_line, col = "red"))
    print(add_curve(m_curve, col = "red"))
  }

  test_add_line <- function() {
    print(xyplot(weight ~ height, data = cdc))
    print(add_line(intercept = -76.91809, slope = 85.87074))
  }

  # # Create reference plots (This should be commented out when testing)
  # png(filename = "tests/testthat/data/add_curve_ref_plot.png")
  # test_add_curve()
  # dev.off()
  # png(filename = "tests/testthat/data/add_line_ref_plot.png")
  # test_add_line()
  # dev.off()

  # Save files for comparisons
  png(filename = "data/add_curve_plot.png")
  test_add_curve()
  dev.off()
  png(filename = "data/add_line_plot.png")
  test_add_line()
  dev.off()

  test_that("bargraphs work", {
    expect_true(is.null(bargraph(~age, data = cdc)[[4]]))
    expect_true(is.null(bargraph(~age | gender, data = cdc)[[4]]))
    expect_false(is.null(bargraph(~age, data = cdc, groups = gender)[[4]]))
  })

  test_that("bwplots work", {
    expect_true(is.null(bwplot(~height, data = cdc)[[4]]))
    expect_equal(length(bwplot(~height | gender, data = cdc)[[35]]), 2)
    expect_match(bwplot(gender~height, data = cdc)[[17]], "gender")
    expect_match(bwplot(height~gender, data = cdc)[[17]], "height")
  })

  test_that("histograms work", {
    # Fit argument works
    expect_true(is.null(histogram(~height, data = cdc)[[34]]$fit))
    expect_equal(length(histogram(~height | gender, data = cdc)[[35]]), 2)
    expect_match(histogram(~height, data = cdc, fit = "normal")[[34]]$fit, "normal")
  })

  test_that("xyplots work", {
    # Legend displays appropriately
    expect_true(is.null(xyplot(weight~height, data = cdc)[[4]]))
    expect_false(is.null(xyplot(weight~height, data = cdc, groups = gender)[[4]]
    ))
    expect_equal(length(xyplot(weight~height | gender, data = cdc)[[35]]), 2)
  })

  test_that("add_curve works", {
    expect_true(visualTest::isSimilar(file = "data/add_curve_plot.png",
                                      fingerprint = visualTest::getFingerprint(
                                        file = "data/add_curve_ref_plot.png"),
                                      exact = TRUE))
  })

  test_that("add_line works", {
    expect_true(visualTest::isSimilar(file = "data/add_line_plot.png",
                                      fingerprint = visualTest::getFingerprint(
                                        file = "data/add_line_ref_plot.png"),
                                      exact = TRUE))
  })

  rm(test_add_line)
  rm(test_add_curve)

}
