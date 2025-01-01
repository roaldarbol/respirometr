#' Convert CO2 measurements from gas analyzer data
#'
#' @description
#' Converts CO2 measurements from gas analyzer data into standardized units,
#' with options for temperature correction. The function handles both regular sampling
#' intervals (specified by sampling frequency) and irregular intervals (specified by
#' time vector). It can process both single measurements and vectors of measurements.
#'
#' The function implements standard gas analysis conversions and can optionally
#' apply temperature corrections using a Q10 = 2 approach (metabolic rate doubles
#' for every 10°C increase).
#'
#' @param co2_value Numeric vector. The differential CO2 value(s) from gas analyzer
#'        (default in μmol/mol or ppm)
#' @param time Numeric vector. Optional vector of timestamps in seconds. If provided,
#'        must be the same length as co2_value. Use this for irregular sampling
#'        intervals or when there are gaps in the data.
#' @param sampling_rate Numeric. The sampling frequency in Hz (e.g., use 30 for
#'        30 Hz sampling). Ignored if time is provided. This is used for regular
#'        sampling intervals.
#' @param flow_rate Numeric. The flow rate of gas through the system.
#' @param from_temperature Numeric. Source temperature in Celsius. Required when
#'        converting to μL to adjust molar volume using ideal gas law. Must be
#'        provided together with to_temperature if metabolic temperature correction
#'        is desired.
#' @param to_temperature Numeric. Optional target temperature in Celsius. Must be
#'        provided together with from_temperature if temperature correction is desired.
#' @param unit_input_co2 Character. Input CO2 unit, either "uM/M" or "umol/mol"
#'        (both equivalent to ppm). Default: "umol/mol"
#' @param unit_output_time Character. Desired time unit for the output rate, one of
#'        "sec", "min", or "hr". Default: "min"
#' @param unit_output_vol Character. Desired volume unit for the output, either
#'        "umol" or "uL". Default: "umol"
#' @param unit_flowrate Character. Flow rate unit, either "mL/min" or "mL/hr".
#'        Default: "mL/min"
#'
#' @return Numeric vector of converted CO2 values in the specified output units
#'
#' @details
#' The function performs the following conversions:
#' 1. Converts CO2 measurements from ppm (μmol/mol) to fractions
#' 2. Calculates time intervals from either sampling rate or timestamps
#' 3. Applies flow rate conversion if needed
#' 4. Optionally applies temperature correction using Q10 = 2
#' 5. Converts to desired output units (for μL, uses ideal gas law to adjust molar volume based on temperature)
#'
#' Temperature correction uses the formula:
#' correction_factor = 10^((to_temperature - from_temperature) * (log10(2)/10))
#'
#' For μL conversions, molar volume is adjusted using ideal gas law:
#' molar_volume = 22.4 L/mol * (T2/273.15K)
#'
#' @examples
#' # Basic usage with 2 Hz sampling
#' convert_unit_co2(
#'   co2_value = 100,
#'   sampling_rate = 2,
#'   flow_rate = 1300,
#'   from_temperature = 20,
#'   unit_output_time = "hr",
#'   unit_output_vol = "uL"
#' )
#'
#' # Using timestamps for irregular sampling
#' convert_unit_co2(
#'   co2_value = c(100, 102, 98),
#'   time = c(0, 1.5, 3.2),
#'   flow_rate = 1300,
#'   from_temperature = 20,
#'   unit_output_time = "hr",
#'   unit_output_vol = "uL"
#' )
#'
#' # With temperature correction
#' convert_unit_co2(
#'   co2_value = 100,
#'   sampling_rate = 30,    # 30 Hz sampling
#'   flow_rate = 200,
#'   from_temperature = 17, # Experiment temperature
#'   to_temperature = 25,   # Standard temperature
#'   unit_output_vol = "uL",
#'   unit_output_time = "min"
#' )
#'
#' # Processing a sequence of measurements
#' co2_values <- c(95, 98, 100, 103, 99)
#' times <- seq(0, 8, by = 2)  # Measurements every 2 seconds
#' convert_unit_co2(
#'   co2_value = co2_values,
#'   time = times,
#'   flow_rate = 200,
#'   unit_output_vol = "umol",
#'   unit_output_time = "min"
#' )
#'
#' @references
#' Temperature correction method based on standard Q10 = 2 approach, where
#' metabolic rate doubles for every 10°C increase in temperature.
#'
#' @note
#' When using sampling_rate, provide the frequency in Hz (e.g., 30 for 30 Hz
#' sampling), not the period (e.g., not 1/30).
#'
#' @export
convert_unit_co2 <- function(co2_value,
                             time = NULL,
                             sampling_rate = NULL,
                             flow_rate,
                             from_temperature = NULL,
                             to_temperature = NULL,
                             unit_input_co2 = "umol/mol",
                             unit_output_time = "min",
                             unit_output_vol = "umol",
                             unit_flowrate = "mL/min") {

  # Input validation
  valid_time <- c("sec", "min", "hr")
  valid_vol <- c("umol", "uL")
  valid_flowrate <- c("mL/min", "mL/hr")
  valid_co2 <- c("uM/M", "umol/mol")

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

  # Check timing inputs
  if (is.null(sampling_rate) && is.null(time)) {
    cli::cli_abort("Must provide either sampling_rate or time")
  }

  if (!is.null(time)) {
    if (length(time) != length(co2_value)) {
      cli::cli_abort("Length of time must match length of co2_value")
    }
    # Calculate time intervals in minutes
    time_intervals <- diff(c(0, time)) / 60  # convert seconds to minutes
  } else {
    # Use constant sampling rate
    time_intervals <- rep(1 / (sampling_rate * 60), length(co2_value))  # converts Hz to minutes
  }

  # Convert flow rate to mL/min if needed
  if (unit_flowrate == "mL/hr") {
    flow_rate <- flow_rate / 60
  }

  # Basic CO2 calculation (in μmol/min)
  result <- flow_rate * co2_value * (1/1000000) / time_intervals

  # Apply temperature correction if temperatures provided
  # Using Q10 = 2 as in the original function
  if (!is.null(from_temperature) && !is.null(to_temperature)) {
    temp_correction <- 10^((to_temperature - from_temperature) * (log10(2)/10))
    result <- result * temp_correction
  }

  # Convert between volume units if needed
  if (unit_output_vol == "uL") {
    if (is.null(from_temperature)) {
      cli::cli_abort("from_temperature must be provided when converting to uL")
    }
    # Adjust molar volume for temperature (K)
    molar_volume <- 22.4 * (273.15 + from_temperature) / 273.15
    result <- result * molar_volume
  }

  # Convert to desired time unit
  if (unit_output_time == "hr") {
    result <- result * 60
  } else if (unit_output_time == "sec") {
    result <- result / 60
  }

  return(result)
}

# # Examples:
# # Using time vector with temperature correction
# example1 <- convert_unit_co2(
#   co2_value = c(100, 102, 98),
#   time = c(0, 1.5, 3.2),
#   flow_rate = 1300,
#   from_temperature = 17,
#   to_temperature = 25,
#   unit_output_time = "hr",
#   unit_output_vol = "uL"
# )
#
# # Using sampling rate without temperature correction
# example2 <- convert_unit_co2(
#   co2_value = 100,
#   sampling_rate = 2,
#   flow_rate = 1300,
#   unit_output_time = "hr",
#   unit_output_vol = "uL"
# )
