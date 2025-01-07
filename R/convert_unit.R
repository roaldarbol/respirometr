#' Convert CO2 measurements from gas analyzer data
#'
#' @description
#' Converts CO2 measurements from gas analyzer data into standardized units,
#' with options for temperature correction. It can process both single measurements and vectors of measurements.
#'
#' The function implements standard gas analysis conversions and can optionally
#' apply temperature corrections using a Q10 = 2 approach (metabolic rate doubles
#' for every 10°C increase).
#'
#' @param co2_value Numeric vector. The differential CO2 value(s) from gas analyzer
#'        (default in μmol/mol or ppm)
#' @param flow_rate Numeric. The flow rate of gas though the system.
#' @param unit_flowrate Character. Flow rate unit, either "mL/min" or "mL/h".
#'        Default: "mL/min"
#' @param unit_input_co2 Character. Input CO2 unit, either "uM/M" or "umol/mol"
#'        (both equivalent to ppm). Default: "umol/mol"
#' @param unit_output_time Character. Desired time unit for the output rate, one of
#'        "s", "min", or "h". Default: "min"
#' @param unit_output_vol Character. Desired volume unit for the output, either
#'        "mL" or "uL". Default: "mL"
#' @param from_temperature Numeric. Source temperature in Celsius. Only required if
#'        metabolic temperature correction is desired (must be provided with to_temperature).
#' @param to_temperature Numeric. Optional target temperature in Celsius. Must be
#'        provided together with from_temperature if temperature correction is desired.

#'
#' @return Numeric vector of converted CO2 values in the specified output units
#'
#' @details
#' The function performs the following conversions:
#' 1. Converts CO2 measurements from ppm (μmol/mol) to fractions
#' 4. Optionally applies temperature correction using Q10 = 2
#' 5. Converts to desired output units (μL conversion is direct volume conversion)
#'
#' Temperature correction uses the formula:
#' correction_factor = 10^((to_temperature - from_temperature) * (log10(2)/10))
#'
#' @examples
#' # Basic usage
#' convert_unit_co2(
#'   co2_value = 100,
#'   flow_rate = 1300,
#'   unit_output_time = "h",
#'   unit_output_vol = "uL"
#' )
#'
#'
#' # With temperature correction
#' convert_unit_co2(
#'   co2_value = 100,
#'   flow_rate = 200,
#'   from_temperature = 17, # Experiment temperature
#'   to_temperature = 25,   # Standard temperature
#'   unit_output_vol = "uL",
#'   unit_output_time = "min"
#' )
#'
#' # Processing a sequence of measurements
#' co2_values <- c(95, 98, 100, 103, 99)
#' convert_unit_co2(
#'   co2_value = co2_values,
#'   flow_rate = 200,
#'   unit_output_vol = "mL",
#'   unit_output_time = "min"
#' )
#'
#' @references
#' Temperature correction method based on standard Q10 = 2 approach, where
#' metabolic rate doubles for every 10°C increase in temperature.
#'
#' @export
convert_unit_co2 <- function(co2_value,
                             flow_rate,
                             unit_flowrate = "mL/min",
                             unit_input_co2 = "umol/mol",
                             unit_output_time = "h",
                             unit_output_vol = "mL",
                             from_temperature = NULL,
                             to_temperature = NULL
                             ) {

  # Input validation
  valid_time <- c("s", "min", "h")
  valid_vol <- c("mL", "uL")
  valid_flowrate <- c("mL/min", "mL/h")
  valid_co2 <- c("uM/M", "umol/mol", "ppm")

  if (!unit_output_time %in% valid_time) {
    cli::cli_abort("Output time units must be one of: {.val {valid_time}}")
  }

  if (!unit_output_vol %in% valid_vol) {
    cli::cli_abort("Volume units must be one of: {.val {valid_vol}}")
  }

  if (!unit_flowrate %in% valid_flowrate) {
    cli::cli_abort("Flow rate units must be one of: {.val {valid_flowrate}}")
  }

  if (!unit_input_co2 %in% valid_co2) {
    cli::cli_abort("Input CO2 units must be one of: {.val {valid_co2}}")
  }

  # Temperature validation
  if (is.null(from_temperature) & !is.null(to_temperature)) {
    cli::cli_abort("Both from_temperature and to_temperature must be provided for temperature correction")
  }

  # Convert flow rate to mL/min
  if (unit_flowrate == "mL/h") {
    flow_rate <- flow_rate / 60
  } else if (unit_flowrate == "mL/s"){
    flow_rate <- flow_rate * 60
  }

  # Basic CO2 calculation (to mL/min)
  co2_value <- co2_value / 1000000 # Convert from ppm to fraction
  result <- flow_rate * co2_value #* time_intervals

  # Apply temperature correction if temperatures provided
  # Using Q10 = 2 as in the original function
  if (!is.null(from_temperature) && !is.null(to_temperature)) {
    temp_correction <- 10^((to_temperature - from_temperature) * (log10(2)/10))
    result <- result * temp_correction
  }

  # Convert between volume units if needed
  if (unit_output_vol == "uL") {
    result <- result * 1000  # Convert mL to μL
  }

  # Convert to desired time unit
  if (unit_output_time == "h") {
    result <- result * 60
  } else if (unit_output_time == "s") {
    result <- result / 60
  }

  return(result)
}
