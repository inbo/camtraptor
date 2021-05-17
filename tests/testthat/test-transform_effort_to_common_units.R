library(lubridate)

test_that("transform_effort_to_common_units returns error if multiple units are given", {
  expect_error(transform_effort_to_common_units(
    effort = duration("1day"),
    unit = c("day", "hour")
  ))
})


test_that("transform_effort_to_common_units returns error if wrong unit is given", {

  # century is not allowed, maximal unit: year
  expect_error(transform_effort_to_common_units(
    effort = duration("1day"),
    unit = c("century")
  ))
})

test_that("transform_effort_to_common_units transforms correctly", {
  expect_equal(transform_effort_to_common_units(
    effort = duration("1day"),
    unit = "day"
  ), 1)
  expect_equal(transform_effort_to_common_units(
    effort = duration("1hour"),
    unit = "hour"
  ), 1)
})
