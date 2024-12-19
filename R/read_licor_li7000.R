#' Read and Process LI-7000 Data Files
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Reads data from a LI-7000 CO2/H2O analyzer file, cleans column names,
#' and ensures proper time column handling. Handles cases where headers
#' are repeated mid-file.
#'
#' @param filepath Character string specifying the path to the LI-7000 data file.
#'   File should be tab-separated with two header rows.
#'
#' @return A tibble containing the processed LI-7000 data with:
#'   * All original columns with cleaned names (lowercase, no spaces)
#'   * Standardized time column
#'   * Data combined across any mid-file header breaks
#'
#' @details
#' The function performs the following operations:
#' 1. Reads the entire file as lines
#' 2. Identifies and removes any mid-file headers
#' 3. Cleans column names using janitor::make_clean_names()
#' 4. Ensures proper time column naming
#'
#' @examples
#' \dontrun{
#' # Read a LI-7000 data file
#' data <- read_licor_li7000("path/to/li7000_data.txt")
#' }
#'
#' @importFrom readr read_tsv read_lines
#' @importFrom janitor make_clean_names
#' @importFrom dplyr rename bind_rows
#' @importFrom cli cli_abort cli_inform cli_warn
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

  # Read all lines from file
  lines <- readr::read_lines(filepath)

  # Find indices of timestamp lines (they start with quotes and contain AM/PM)
  header_indices <- grep('^"[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}.*[AP]M"$', lines)

  if (length(header_indices) > 1) {
    cli::cli_warn(
      c("Found {length(header_indices)} header sections in file:",
        "*" = "Data will be combined across sections")
    )
  }

  # Process each section and combine
  data_sections <- vector("list", length(header_indices))

  for (i in seq_along(header_indices)) {
    start_idx <- header_indices[i] + 2  # Skip timestamp and column headers
    end_idx <- if (i < length(header_indices)) {
      header_indices[i + 1] - 1
    } else {
      length(lines)
    }

    # Create temporary file with this section
    temp_file <- tempfile()
    on.exit(unlink(temp_file), add = TRUE)

    # Write headers and data to temp file
    # Use file() and writeLines() with proper connection handling
    con <- file(temp_file, "w")
    writeLines(lines[1:3], con)  # Write headers
    close(con)

    # Append data using connection in append mode
    con <- file(temp_file, "a")
    writeLines(lines[start_idx:end_idx], con)
    close(con)

    # Read this section
    section_data <- tryCatch({
      readr::read_tsv(
        temp_file,
        skip = 2,
        name_repair = janitor::make_clean_names,
        show_col_types = FALSE
      )
    }, error = function(e) {
      cli::cli_abort(
        c("Failed to read data section {i}:",
          "i" = "Error message: {e$message}"),
        class = "section_processing_error"
      )
    })

    data_sections[[i]] <- section_data
  }

  # Combine all sections
  processed_data <- dplyr::bind_rows(data_sections)

  # Check if we have any data
  if (nrow(processed_data) == 0) {
    cli::cli_abort(
      "No data found in file after processing",
      class = "empty_data"
    )
  }

  # Check for required time column
  if (!"time_s" %in% names(processed_data)) {
    cli::cli_abort(
      "Required column 'time' not found in data",
      class = "missing_time_column"
    )
  }

  # Process data
  processed_data <- processed_data |>
    dplyr::rename(time = "time_s")

  # Inform about processing results
  cli::cli_inform(
    c("Successfully processed LI-7000 data:",
      "*" = "Processed {nrow(processed_data)} rows",
      "*" = "Contains {ncol(processed_data)} columns",
      "*" = "Combined {length(data_sections)} data sections"
    )
  )

  return(processed_data)
}
