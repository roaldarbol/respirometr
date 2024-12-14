#' Tests for read_licor_li7000 function
#'
#' @importFrom testthat test_that expect_equal expect_error describe
suppressPackageStartupMessages({
  library(testthat)
  library(readr)
  library(dplyr)
})

# Get path to test data file
li7000_file <- test_path("..", "data", "LI7000.txt")

describe("read_licor_li7000", {

  test_that("function reads and processes data correctly", {
    result <- read_licor_li7000(li7000_file)

    # Check basic structure
    expect_true(is.data.frame(result))

    # Check for expected columns based on the file
    expected_columns <- c(
      "time", "aux1", "aux2", "co2a_um_m", "co2b_um_m", "co2d_um_m",
      "diag", "flow_v", "h2oa_dp_c", "h2oa_mm_m", "h2ob_dp_c",
      "h2ob_mm_m", "h2od_dp_c", "h2od_mm_m", "p_k_pa", "rh_percent", "t_c"
    )
    expect_true(all(expected_columns %in% names(result)))

    # Check number of rows matches original data (27 rows in sample)
    expect_equal(nrow(result), 27)
  })

  test_that("time values are correct", {
    result <- read_licor_li7000(li7000_file)

    # Check time is numeric
    expect_true(is.numeric(result$time))

    # Check time increments
    time_diffs <- diff(result$time)
    expect_true(all(time_diffs > 0))  # Time should always increase
    expect_true(mean(time_diffs) < 0.04)  # Approximately 0.033 seconds between measurements
  })

  test_that("numeric columns are properly parsed", {
    result <- read_licor_li7000(li7000_file)

    numeric_cols <- c("aux1", "aux2", "co2a_um_m", "co2b_um_m", "flow_v",
                      "h2oa_mm_m", "h2ob_mm_m", "p_k_pa", "t_c")

    for(col in numeric_cols) {
      expect_true(is.numeric(result[[col]]))
    }
  })

  test_that("function handles invalid file path", {
    expect_error(
      read_licor_li7000("nonexistent_file.txt"),
      class = "file_not_found"
    )
  })

  test_that("small numeric values are correctly read", {
    result <- read_licor_li7000(li7000_file)

    # Check aux2 values which are very small numbers in scientific notation
    expect_true(all(result$aux2 < 0.001))
    expect_true(all(result$aux2 > 0))
  })
})
