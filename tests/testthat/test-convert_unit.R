library(testthat)

test_that("basic conversion works", {
  result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 1300
  )
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gt(result, 0)
})


test_that("temperature correction works", {
  base_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 200
  )

  corrected_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 200,
    from_temperature = 15,
    to_temperature = 25
  )

  # Q10 = 2 means doubling every 10°C
  expect_gt(corrected_result, base_result)
})

test_that("unit conversions are correct", {
  # Test mL to μL conversion (direct volume conversion)
  ml_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 1300,
    unit_output_vol = "mL"
  )

  ul_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 1300,
    unit_output_vol = "uL"
  )

  # Direct mL to μL conversion (1000x)
  expect_equal(ul_result, ml_result * 1000)

  # Remove temperature requirement test for μL
  # Time unit conversions remain unchanged
  min_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 1300,
    unit_output_time = "min"
  )

  h_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 1300,
    unit_output_time = "h"
  )

  sec_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 1300,
    unit_output_time = "s"
  )

  expect_equal(h_result, min_result * 60)
  expect_equal(sec_result, min_result / 60)
})

test_that("invalid inputs thow appropriate errors", {
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
      to_temperature = 20
    ),
    "Both from_temperature and to_temperature must be provided"
  )
})

test_that("flow rate units are handled correctly", {
  min_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 60,
    unit_flowrate = "mL/min"
  )

  h_result <- convert_unit_co2(
    co2_value = 100,
    flow_rate = 60,
    unit_flowrate = "mL/h"
  )

  expect_equal(h_result, min_result / 60)
})
