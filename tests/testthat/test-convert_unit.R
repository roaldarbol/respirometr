library(testthat)

test_that("basic conversion works", {
  result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 2,
    flow_rate = 1300
  )
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gt(result, 0)
})

test_that("vector input works with time", {
  result <- convert_unit_co2(
    co2_value = c(100, 102, 98),
    time = c(0, 1.5, 3.2),
    flow_rate = 1300
  )
  expect_length(result, 3)
  expect_true(all(!is.na(result)))
})

test_that("temperature correction works", {
  base_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 30,
    flow_rate = 200
  )

  corrected_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 30,
    flow_rate = 200,
    from_temperature = 15,
    to_temperature = 25
  )

  # Q10 = 2 means doubling every 10°C
  expect_gt(corrected_result, base_result)
})

test_that("unit conversions are correct", {
  # Test umol to uL conversion (factor of 22.4)
  umol_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 2,
    flow_rate = 1300,
    unit_output_vol = "umol"
  )

  ul_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 2,
    flow_rate = 1300,
    unit_output_vol = "uL"
  )

  expect_equal(ul_result, umol_result * 22.4)

  # Test time unit conversions
  min_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 2,
    flow_rate = 1300,
    unit_output_time = "min"
  )

  hr_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 2,
    flow_rate = 1300,
    unit_output_time = "hr"
  )

  sec_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 2,
    flow_rate = 1300,
    unit_output_time = "sec"
  )

  expect_equal(hr_result, min_result * 60)
  expect_equal(sec_result, min_result / 60)
})

test_that("invalid inputs throw appropriate errors", {
  expect_error(
    convert_unit_co2(
      co2_value = 100,
      flow_rate = 1300,
      unit_output_time = "invalid"
    ),
    "Output time units must be one of:"
  )

  expect_error(
    convert_unit_co2(
      co2_value = 100,
      flow_rate = 1300,
      from_temperature = 20
    ),
    "Both from_temperature and to_temperature must be provided"
  )

  expect_error(
    convert_unit_co2(
      co2_value = c(100, 102),
      time = c(0),
      flow_rate = 1300
    ),
    "Length of time must match length of co2_value"
  )
})

test_that("flow rate units are handled correctly", {
  min_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 2,
    flow_rate = 60,
    unit_flowrate = "mL/min"
  )

  hr_result <- convert_unit_co2(
    co2_value = 100,
    sampling_rate = 2,
    flow_rate = 60,
    unit_flowrate = "mL/hr"
  )

  expect_equal(hr_result, min_result / 60)
})
