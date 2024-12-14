library(testthat)
suppressPackageStartupMessages(library(dplyr))

# Helper function to create test data
create_test_data <- function() {
  # Pre-experiment data
  data_pre <- data.frame(
    time = seq(0, 5, by = 1),
    co2d_um_m = c(5, 7, 9, 11, 13, 6)
  )

  # Post-experiment data
  data_post <- data.frame(
    time = seq(15, 20, by = 1),
    co2d_um_m = c(15, 17, 19, 21, 23, 16)
  )

  # Main data to normalize
  data <- data.frame(
    time = seq(0, 10, by = 1),
    co2d_um_m = seq(10, 12, length.out = 11)
  )

  return(list(data = data, data_pre = data_pre, data_post = data_post))
}

# Test suite
describe("normalise_drift_lm function", {

  test_that("function returns a data frame", {
    test_data <- create_test_data()

    result <- normalise_drift_lm(
      data = test_data$data,
      data_pre = test_data$data_pre,
      data_post = test_data$data_post
    )

    expect_true(is.data.frame(result))
    expect_identical(names(result), names(test_data$data))
  })

  test_that("normalization reduces baseline drift", {
    test_data <- create_test_data()

    result <- normalise_drift_lm(
      data = test_data$data,
      data_pre = test_data$data_pre,
      data_post = test_data$data_post
    )

    # Check that the normalized data has smaller mean than the original
    original_mean <- mean(test_data$data$co2d_um_m)
    normalized_mean <- mean(result$co2d_um_m)

    expect_true(normalized_mean <= original_mean)
  })

  test_that("function handles custom column names", {
    test_data <- create_test_data()

    # Rename columns
    test_data$data <- test_data$data %>%
      rename(measurement = co2d_um_m, timestamp = time)
    test_data$data_pre <- test_data$data_pre %>%
      rename(measurement = co2d_um_m, timestamp = time)
    test_data$data_post <- test_data$data_post %>%
      rename(measurement = co2d_um_m, timestamp = time)

    result <- normalise_drift_lm(
      data = test_data$data,
      data_pre = test_data$data_pre,
      data_post = test_data$data_post,
      colname_time = "timestamp",
      colname_measure = "measurement"
    )

    expect_true(is.data.frame(result))
    expect_true("measurement" %in% names(result))
  })

  test_that("function throws error for invalid inputs", {
    test_data <- create_test_data()

    # Test non-data frame input
    expect_error(
      normalise_drift_lm(
        data = c(1,2,3),
        data_pre = test_data$data_pre,
        data_post = test_data$data_post
      ),
      class = "invalid_data_type"
    )

    # Test missing required columns
    invalid_data_pre <- test_data$data_pre %>% select(-time)
    expect_error(
      normalise_drift_lm(
        data = test_data$data,
        data_pre = invalid_data_pre,
        data_post = test_data$data_post
      ),
      class = "missing_time_column"
    )
  })

  test_that("function warns about small datasets", {
    test_data <- create_test_data()

    # Create very small pre and post datasets
    small_pre <- test_data$data_pre[1:2, ]
    small_post <- test_data$data_post[1:2, ]

    # Capture all warnings
    expect_snapshot(
      normalise_drift_lm(
        data = test_data$data,
        data_pre = small_pre,
        data_post = small_post
      )
    )
  })
})
