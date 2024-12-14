#' Tests for normalise_drift_means function
#'
#' @importFrom testthat test_that expect_equal expect_error describe
suppressPackageStartupMessages(library(testthat))

describe("normalise_drift_means function", {

  test_that("function handles single pre/post values correctly", {
    data <- c(15, 16, 17, 18, 19)
    pre <- 10
    post <- 20

    result <- normalise_drift_means(data, pre, post)

    # Expected baseline is seq(10, 20, length.out = 5)
    expected <- data - seq(10, 20, length.out = 5)
    expect_equal(result, expected)
  })

  test_that("function correctly averages multiple pre/post values", {
    data <- c(15, 16, 17, 18, 19)
    pre <- c(9, 10, 11)  # mean = 10
    post <- c(19, 20, 21)  # mean = 20

    expect_snapshot(
      result <- normalise_drift_means(data, pre, post)
    )

    # Should give same result as using means directly
    expected <- normalise_drift_means(data, mean(pre), mean(post))
    expect_equal(result, expected)
  })

  test_that("function handles NA values in pre/post correctly", {
    data <- c(15, 16, 17, 18, 19)
    pre <- c(9, NA, 11)
    post <- c(19, 20, NA)

    expect_snapshot(
      result <- normalise_drift_means(data, pre, post)
    )

    # Should match results using non-NA values only
    expected <- normalise_drift_means(data,
                                      mean(c(9, 11)),
                                      mean(c(19, 20)))
    expect_equal(result, expected)
  })

  test_that("function errors on invalid inputs", {
    data <- c(15, 16, 17, 18, 19)

    # Test non-numeric data
    expect_error(
      normalise_drift_means(data = "not numeric", pre = 10, post = 20),
      class = "invalid_data_type"
    )

    # Test non-numeric pre
    expect_error(
      normalise_drift_means(data = data, pre = "10", post = 20),
      class = "invalid_pre_type"
    )

    # Test non-numeric post
    expect_error(
      normalise_drift_means(data = data, pre = 10, post = "20"),
      class = "invalid_post_type"
    )
  })

  test_that("function preserves data length", {
    # Test with different data lengths
    data_lengths <- c(5, 10, 100)

    for (len in data_lengths) {
      data <- seq(1, len)
      result <- normalise_drift_means(data, pre = 0, post = 10)
      expect_equal(length(result), len)
    }
  })

  test_that("function correctly removes drift", {
    # Create data with known linear drift
    drift <- seq(0, 10, length.out = 10)  # Linear drift from 0 to 10
    pure_signal <- rep(5, 10)  # Constant signal
    data <- pure_signal + drift  # Signal plus drift

    result <- normalise_drift_means(data, pre = 0, post = 10)

    # After removing drift, should be close to pure signal
    # Using tolerance because of potential floating point differences
    expect_equal(result, pure_signal, tolerance = 1e-10)
  })
})
