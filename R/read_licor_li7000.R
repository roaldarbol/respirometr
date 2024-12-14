#' Read and Process LI-7000 Data Files
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Reads data from a LI-7000 CO2/H2O analyzer file, cleans column names,
#' and ensures proper time column handling.
#'
#' @param filepath Character string specifying the path to the LI-7000 data file.
#'   File should be tab-separated with two header rows.
#'
#' @return A tibble containing the processed LI-7000 data with:
#'   * All original columns with cleaned names (lowercase, no spaces)
#'   * Standardized time column
#'
#' @details
#' The function performs the following operations:
#' 1. Skips the first two header rows
#' 2. Cleans column names using janitor::make_clean_names()
#' 3. Ensures proper time column naming
#'
#' @examples
#' \dontrun{
#' # Read a LI-7000 data file
#' data <- read_licor_li7000("path/to/li7000_data.txt")
#' }
#'
#' @importFrom readr read_tsv
#' @importFrom janitor make_clean_names
#' @importFrom dplyr rename
#' @importFrom cli cli_abort cli_inform
#'
#' @export
read_licor_li7000 <- function(filepath) {
  # Validate input
  if (!is.character(filepath)) {
    cli::cli_abort(
      "Filepath must be a character string",
      class = "invalid_filepath_type"
    )
  }

  if (!file.exists(filepath)) {
    cli::cli_abort(
      "File {.file {filepath}} does not exist",
      class = "file_not_found"
    )
  }

  # Inform user about file reading
  cli::cli_inform(
    "Reading LI-7000 data from {.file {filepath}}"
  )

  # Read and process data
  tryCatch({
    data <- filepath |>
      readr::read_tsv(
        skip = 2,
        name_repair = janitor::make_clean_names,
        show_col_types = FALSE
      )

    # Check if we have any data
    if (nrow(data) == 0) {
      cli::cli_abort(
        "No data found in file after skipping headers",
        class = "empty_data"
      )
    }

    # Check for required time column
    if (!"time_s" %in% names(data)) {
      cli::cli_abort(
        "Required column 'time' not found in data",
        class = "missing_time_column"
      )
    }

    # Process data
    processed_data <- data |>
      dplyr::rename(time = "time_s")

    # Inform about processing results
    cli::cli_inform(
      c("Successfully processed LI-7000 data:",
        "*" = "Processed {nrow(processed_data)} rows",
        "*" = "Contains {ncol(processed_data)} columns"
      )
    )

    return(processed_data)

  }, error = function(e) {
    cli::cli_abort(
      c("Failed to read or process LI-7000 data:",
        "i" = "Error message: {e$message}"),
      class = "data_processing_error"
    )
  })
}
