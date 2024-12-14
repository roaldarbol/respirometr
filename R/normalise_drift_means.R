#' Normalize Data for Linear Drift Between Pre and Post Measurements
#'
#' This function normalizes a data vector by subtracting a linear interpolation
#' between the means of pre and post measurements. This is useful for correcting
#' measurement drift in experimental data.
#'
#' @param data Numeric vector containing the measurements to be normalized
#' @param pre Numeric vector or single value representing pre-measurement(s)
#' @param post Numeric vector or single value representing post-measurement(s)
#'
#' @return A numeric vector of the same length as `data` containing the
#'   normalized values
#'
#' @aliases normalize_drift_means
#'
#' @details
#' The function performs the following steps:
#' 1. Calculates mean of pre-measurements if multiple values provided
#' 2. Calculates mean of post-measurements if multiple values provided
#' 3. Creates a linear interpolation between pre and post means
#' 4. Subtracts interpolated values from the data to correct for drift
#'
#' @examples
#' # Single pre/post values
#' data <- c(15, 16, 17, 18, 19)
#' normalise_drift_means(data, pre = 10, post = 20)
#'
#' # Multiple pre/post values
#' normalise_drift_means(
#'   data = c(15, 16, 17, 18, 19),
#'   pre = c(10, 11, 12),
#'   post = c(19, 20, 21)
#' )
#'
#' @importFrom cli cli_abort cli_inform
#' @export
normalise_drift_means <- function(data, pre, post) {
  # Input validation
  if (!is.numeric(data)) {
    cli::cli_abort(
      "Input {.var data} must be numeric",
      class = "invalid_data_type"
    )
  }

  if (!is.numeric(pre)) {
    cli::cli_abort(
      "Input {.var pre} must be numeric",
      class = "invalid_pre_type"
    )
  }

  if (!is.numeric(post)) {
    cli::cli_abort(
      "Input {.var post} must be numeric",
      class = "invalid_post_type"
    )
  }

  # Calculate means for pre and post values
  if (length(pre) > 1) {
    pre_mean <- mean(pre, na.rm = TRUE)
    cli::cli_inform(
      "Using mean of {length(pre)} pre-measurements: {round(pre_mean, 3)}"
    )
    pre <- pre_mean
  }

  if (length(post) > 1) {
    post_mean <- mean(post, na.rm = TRUE)
    cli::cli_inform(
      "Using mean of {length(post)} post-measurements: {round(post_mean, 3)}"
    )
    post <- post_mean
  }

  # Create linear interpolation and subtract
  baseline <- seq(
    from = pre,
    to = post,
    length.out = length(data)
  )
  corrected_vals <- data - baseline

  cli::cli_inform(
    "Corrected for linear drift between {round(pre, 3)} and {round(post, 3)}"
  )

  return(corrected_vals)
}
