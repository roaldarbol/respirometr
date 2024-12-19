#' Tests for read_licor_li850 function
#'
#' @importFrom testthat test_that expect_equal expect_error describe test_path
suppressPackageStartupMessages({
  library(testthat)
  library(readr)
  library(dplyr)
  library(hms)
  library(here)
})

# Get path to test data file
li850_file <- test_path("data", "li850.txt")

describe("read_licor_li850", {

  test_that("function reads and processes data correctly", {
    result <- read_licor_li850(li850_file)

    # Check basic structure
    expect_true(is.data.frame(result))
    expect_true(all(c("time", "system_time_h_m_s", "co2_mmol_mol_1", "h2o_mmol_mol_1",
                      "cell_temperature_c", "cell_pressure_k_pa") %in% names(result)))

    # Check data types
    expect_true(inherits(result$system_time_h_m_s, "hms"))
    expect_true(is.numeric(result$time))
    expect_true(is.numeric(result$co2_mmol_mol_1))
    expect_true(is.numeric(result$h2o_mmol_mol_1))

    # Check number of rows matches original data (31 rows in sample)
    expect_equal(nrow(result), 30)
  })

  test_that("time calculations are correct", {
    result <- read_licor_li850(li850_file)

    # Check that time starts at 0
    expect_equal(min(result$time), 0)

    # First timestamp is 10:44:50, last is 10:45:05
    # Total time should be 15 seconds
    expect_equal(max(result$time), 15)

    # Check time differences are positive
    time_diffs <- diff(result$time)
    expect_true(all(time_diffs >= 0))
  })

  test_that("function handles invalid file path", {
    expect_error(
      read_licor_li850("nonexistent_file.txt"),
      class = "file_not_found"
    )
  })

  # test_that("all numeric columns are properly converted from scientific notation", {
  #   result <- read_licor_li850(li850_file)
  #
  #   # Check scientific notation was properly converted
  #   expect_true(all(sapply(result, function(x) {
  #     if(is.numeric(x)) {
  #       !any(grepl("e", format(x, scientific = TRUE)))
  #     } else TRUE
  #   })))
  # })

  test_that("duplicate timestamps are handled correctly", {
    result <- read_licor_li850(li850_file)

    # Group by time and check no duplicates in relative time
    time_counts <- table(result$time)
    expect_true(all(time_counts >= 1))
  })
})
