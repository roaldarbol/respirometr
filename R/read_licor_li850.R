#' Read and Process LI-850 Data Files
#'
#' @description
#'
#' Reads data from a LI-850 CO2/H2O analyzer file, cleans column names,
#' removes columns with NA values, and adds an elapsed time column.
#' Handles cases where headers are repeated mid-file.
#'
#' @param filepath Character string specifying the path to the LI-850 data file.
#'   File should be tab-separated with a one-line header.
#'
#' @return A tibble containing the processed LI-850 data with:
#'   * All original columns except those containing NA values
#'   * Cleaned column names (lowercase, no spaces)
#'   * `system_time_h_m_s` converted to HMS format
#'   * New `time` column with seconds elapsed from start
#'   * Data combined across any mid-file header breaks
#'
#' @details
#' The function performs the following operations:
#' 1. Reads the entire file and handles any mid-file headers
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
#' @importFrom readr read_tsv read_lines
#' @importFrom janitor make_clean_names
#' @importFrom dplyr select mutate where bind_rows
#' @importFrom hms as_hms
#' @importFrom rlang .data
#' @importFrom cli cli_abort cli_inform cli_warn
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

  # Read all lines from file
  lines <- readr::read_lines(filepath)

  # Find indices of header lines by looking for the first line pattern
  # We'll use the first line to identify the header pattern
  header_pattern <- lines[1]
  header_indices <- grep(paste0("^", header_pattern, "$"), lines)

  if (length(header_indices) > 1) {
    cli::cli_warn(
      c("Found {length(header_indices)} header sections in file:",
        "*" = "Data will be combined across sections")
    )
  }

  # Process each section and combine
  data_sections <- vector("list", length(header_indices))

  for (i in seq_along(header_indices)) {
    start_idx <- header_indices[i]  # Include header for this section
    end_idx <- if (i < length(header_indices)) {
      header_indices[i + 1] - 1
    } else {
      length(lines)
    }

    # Create temporary file with this section
    temp_file <- tempfile()
    on.exit(unlink(temp_file), add = TRUE)

    # Write header and data to temp file
    con <- file(temp_file, "w")
    writeLines(lines[start_idx:(start_idx + 1)], con)  # Write header
    close(con)

    if (end_idx > (start_idx + 1)) {  # If we have data rows
      con <- file(temp_file, "a")
      writeLines(lines[(start_idx + 2):end_idx], con)  # Write data
      close(con)
    }

    # Read this section
    section_data <- tryCatch({
      readr::read_tsv(
        temp_file,
        skip = 1,
        name_repair = janitor::make_clean_names,
        show_col_types = FALSE
      ) |>
        suppressWarnings()
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
  data <- dplyr::bind_rows(data_sections)

  # Check if we have any data
  if (nrow(data) == 0) {
    cli::cli_abort(
      "No data found in file after processing",
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
      "*" = "Time span: {round(max(processed_data$time))} seconds",
      "*" = "Combined {length(data_sections)} data sections"
    )
  )

  return(processed_data)
}
