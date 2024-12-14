#' Normalize Measurement Data Using Linear Regression
#'
#' This function normalizes measurement data by subtracting baseline values
#' predicted from a linear regression of pre and post data.
#'
#' @param data A data frame containing the main dataset to be normalized
#' @param data_pre A data frame containing pre-experiment data used for baseline calculation
#' @param data_post A data frame containing post-experiment data used for baseline calculation
#' @param colname_time Character string specifying the column name for time measurements.
#'   Default is "time".
#' @param colname_measure Character string specifying the column name for the measurement.
#'   Default is "co2d_um_m".
#'
#' @return A modified version of the input `data` with baseline-adjusted measurements
#'
#' @details
#' The function performs the following steps:
#' 1. Combines pre and post data
#' 2. Fits a linear regression model using time as the predictor
#' 3. Predicts baseline values for the main dataset
#' 4. Subtracts these baseline values from the original measurements
#'
#' @examples
#' \dontrun{
#' # Assuming you have pre, post, and main datasets
#' normalized_data <- normalise_lm(
#'   data = main_data,
#'   data_pre = pre_experiment_data,
#'   data_post = post_experiment_data
#' )
#' }
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom stats lm predict
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom rlang sym
#' @export
normalise_lm <- function(data, data_pre, data_post,
                         colname_time = "time",
                         colname_measure = "co2d_um_m") {
  # Validate required packages
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("The 'cli' package is required to use this function.")
  }

  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.var data} must be a data frame",
      class = "invalid_data_type"
    )
  }

  if (!is.data.frame(data_pre)) {
    cli::cli_abort(
      "{.var data_pre} must be a data frame",
      class = "invalid_pre_data_type"
    )
  }

  if (!is.data.frame(data_post)) {
    cli::cli_abort(
      "{.var data_post} must be a data frame",
      class = "invalid_post_data_type"
    )
  }

  # Check for required columns
  if (!colname_time %in% names(data_pre)) {
    cli::cli_abort(
      "Column {.field {colname_time}} not found in {.var data_pre}",
      class = "missing_time_column"
    )
  }

  if (!colname_measure %in% names(data_pre)) {
    cli::cli_abort(
      "Column {.field {colname_measure}} not found in {.var data_pre}",
      class = "missing_measure_column"
    )
  }

  # Warning if datasets seem small
  if (nrow(data_pre) < 5) {
    cli::cli_warn(
      "Very few observations in {.var data_pre} may lead to unstable baseline estimation"
    )
  }

  if (nrow(data_post) < 5) {
    cli::cli_warn(
      "Very few observations in {.var data_post} may lead to unstable baseline estimation"
    )
  }

  # Inform about normalization process
  cli::cli_inform(
    "Normalizing {.field {deparse(substitute(data))}} using linear regression with {.field {colname_time}} and {.field {colname_measure}}"
  )

  # Combine pre and post data
  prepost <- dplyr::bind_rows(data_pre, data_post)

  # Fit linear regression
  lm_drift <- tryCatch({
    stats::lm(get(colname_measure) ~ get(colname_time), data = prepost)
  }, error = function(e) {
    cli::cli_abort(
      "Failed to fit linear regression model. Check input data.",
      class = "lm_fit_error"
    )
  })

  # Predict baseline values
  baseline_vals <- tryCatch({
    stats::predict(lm_drift, data) |> as.vector()
  }, error = function(e) {
    cli::cli_abort(
      "Failed to predict baseline values. Ensure predictor variables match.",
      class = "prediction_error"
    )
  })

  # Adjust column names for non-standard evaluation
  colname_time <- rlang::sym(colname_time)
  colname_measure <- rlang::sym(colname_measure)

  # Normalize data by subtracting baseline
  data <- data |>
    dplyr::mutate({{ colname_measure }} := {{ colname_measure }} - baseline_vals)

  return(data)
}
