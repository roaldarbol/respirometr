#' Ensure that column types are not all characters
#' @description Ensure that column types are not all characters. This sometimes
#'   occurs if the machine re-initiates the column headers twice in the same
#'   data set.
#' @importFrom cli cli_abort
#' @keywords internal
ensure_column_types <- function(data) {
  col_types <- data |>
    sapply(class) |>
    unique()
  if (all(col_types == "character")) {
    cli::cli_abort("Cannot read file, as columns are all read as characters. This sometimes occurs if the machine re-initiates the column headers twice in the same data set.")
  }
}

