#' Plot Time Series of Values
#'
#' @description
#' Creates a visualization of time series data.
#' Useful for analyzing patterns in continuous measurements such as
#' physiological data or other single-trace recordings.
#'
#' @param data A data frame containing time series data with the following columns:
#'   - Time column (specified by `time_col`, defaults to "time")
#'   - Column specified by `value_col` containing the values to plot
#' @param value_col Character string specifying which column to plot
#' @param time_col Character string specifying the time column (default: "time")
#' @param y_max Optional numeric value specifying the maximum value for the y-axis.
#'   If NULL (default), the y-axis limit is automatically determined from the data.
#'
#' @return A ggplot object wrapped in patchwork.
#'
#' @examples
#' \dontrun{
#' # Plot heart rate data
#' plot_timeseries(physio_data, value_col = "heart_rate")
#'
#' # Plot temperature with custom time column
#' plot_timeseries(physio_data,
#'                value_col = "temperature",
#'                time_col = "timestamp")
#' }
#'
#' @importFrom rlang .data
#' @export
plot_timeseries <- function(data,
                            value_col,
                            time_col = "time",
                            y_max = NULL) {
  # Input validation
  if (missing(value_col)) {
    cli::cli_abort("Must specify {.arg value_col}")
  }

  if (!value_col %in% names(data)) {
    cli::cli_abort(c(
      "Column {.val {value_col}} not found in data.",
      "i" = "Available columns: {.val {names(data)}}"
    ))
  }

  if (!time_col %in% names(data)) {
    cli::cli_abort(c(
      "Time column {.val {time_col}} not found in data.",
      "i" = "Available columns: {.val {names(data)}}"
    ))
  }

  # Create the single time series plot
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[time_col]])) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[value_col]]),
                       colour = "#1f77b4") +
    ggplot2::labs(y = tools::toTitleCase(value_col),
                  x = "Time") +
    ggplot2::theme_linedraw()

  if (!is.null(y_max)) {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, y_max))
  }

  # Wrap in patchwork for consistent layout with other plots
  output_plot <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = paste("Time Series of", tools::toTitleCase(value_col))
    ) +
    patchwork::plot_layout(
      axes = "collect",
      axis_titles = "collect",
      guides = "collect"
    )

  return(output_plot)
}
