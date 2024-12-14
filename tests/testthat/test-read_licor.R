# Test arguments
path_correct <- testthat::test_path("data", "li850.txt")

# File headers
# test_that("File headers", {
#   expect_false(
#     ensure_file_has_headers(path_correct)
#   )
#   expect_true(
#     ensure_file_has_headers(path_named_cols)
#   )
# })

# Read file
test_that("Read file", {
  expect_no_error(
    read_licor(path_correct, model = "850")
  )
  # expect_contains(
  #   read_opticalflow(path_correct, col_time = 4) |>
  #     names(),
  #   c("dx", "dy", "time")
  # )
})
