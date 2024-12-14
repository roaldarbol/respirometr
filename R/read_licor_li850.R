#' Read and Process LI-850 Data Files
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Reads data from a LI-850 CO2/H2O analyzer file, cleans column names,
#' removes columns with NA values, and adds an elapsed time column.
#'
#' @param filepath Character string specifying the path to the LI-850 data file.
#'   File should be tab-separated with a one-line header.
#'
#' @return A tibble containing the processed LI-850 data with:
#'   * All original columns except those containing NA values
#'   * Cleaned column names (lowercase, no spaces)
#'   * `system_time_h_m_s` converted to HMS format
#'   * New `time` column with seconds elapsed from start
#'
#' @details
#' The function performs the following operations:
#' 1. Reads the TSV file, skipping the first row
#' 2. Cleans column names using janitor::make_clean_names()
#' 3. Removes any columns containing NA values
#' 4. Converts system time to HMS format
#' 5. Adds an elapsed time column in seconds
#'
#' @examples
#' \dontrun{
#' # Read a LI-850 data file
#' data <- read_licor_li850("path/to/li850_data.txt")
#' }
#'
#' @importFrom readr read_tsv
#' @importFrom janitor make_clean_names
#' @importFrom dplyr select mutate where
#' @importFrom hms as_hms
#' @importFrom rlang .data
#' @importFrom cli cli_abort cli_inform
#'
#' @export
read_licor_li850 <- function(filepath) {
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

  # Helper function to check if a column has any NA values
  .not_any_na <- function(x) !any(is.na(x))

  # Inform user about file reading
  cli::cli_inform(
    "Reading LI-850 data from {.file {filepath}}"
  )

  # Read and process data
  tryCatch({
    data <- filepath |>
      readr::read_tsv(
        skip = 1,
        name_repair = janitor::make_clean_names,
        show_col_types = FALSE
      ) |>
      suppressWarnings()

    # Check if we have any data
    if (nrow(data) == 0) {
      cli::cli_abort(
        "No data found in file after skipping header",
        class = "empty_data"
      )
    }

    # Check for required time column
    if (!"system_time_h_m_s" %in% names(data)) {
      cli::cli_abort(
        "Required column 'system_time_h_m_s' not found in data",
        class = "missing_time_column"
      )
    }

    # Process data
    processed_data <- data |>
      dplyr::select(where(.not_any_na)) |>
      dplyr::mutate(
        system_time_h_m_s = hms::as_hms(.data$system_time_h_m_s),
        time = as.numeric(.data$system_time_h_m_s - min(.data$system_time_h_m_s))
      )

    # Inform about processing results
    cli::cli_inform(
      c("Successfully processed LI-850 data:",
        "*" = "Retained {ncol(processed_data)} columns",
        "*" = "Processed {nrow(processed_data)} rows",
        "*" = "Time span: {round(max(processed_data$time))} seconds"
      )
    )

    return(processed_data)

  }, error = function(e) {
    cli::cli_abort(
      c("Failed to read or process LI-850 data:",
        "i" = "Error message: {e$message}"),
      class = "data_processing_error"
    )
  })
}
