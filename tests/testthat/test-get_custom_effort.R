test_that("get_custom_effort returns error for invalid group_by value", {
  expect_error(get_custom_effort(mica, group_by = "bad_value"))
})

test_that("get_custom_effort returns error for start not a Date", {
  expect_error(get_custom_effort(mica, start = "2021-01-01"))
  # no datetime allowed
  expect_error(get_custom_effort(mica, start = lubridate::as_datetime("2021-12-05 22:25:01 CET")))
})

test_that("get_custom_effort returns error for end not a Date", {
  expect_error(get_custom_effort(mica, end = "2021-01-01"))
  # no datetime allowed
  expect_error(get_custom_effort(mica, end = lubridate::as_datetime("2021-12-05 22:25:01 CET")))
})

test_that("get_custom_effort returns error if end earlier than start", {
  expect_error(get_custom_effort(mica,
                                 start = as.Date("2021-01-01"),
                                 end = as.Date("1990-01-01")))
})

test_that("get_custom_effort returns error for invalid effort units", {
  expect_error(get_custom_effort(mica, unit = "second"))
  expect_error(get_custom_effort(mica, unit = "year"))
})

test_that("right columns, cols types, right relative number of rows", {
  # right cols and col types: no groups
  tot_effort <- get_custom_effort(mica)
  expect_true(all(colnames(tot_effort) == c("begin", "effort", "unit")))
  expect_equal(class(tot_effort$begin), "Date")
  expect_equal(class(tot_effort$effort), "numeric")
  expect_equal(class(tot_effort$unit), "character")

  # right cols and col types: group by year
  effort_by_year<- get_custom_effort(mica, group_by = "year")
  expect_true(
    all(colnames(effort_by_year) == c("begin", "effort", "unit", "year"))
  )
  expect_equal(class(effort_by_year$year), "numeric")

  # right cols and col types: group by month
  effort_by_month<- get_custom_effort(mica, group_by = "month")
  expect_true(
    all(
      colnames(effort_by_month) == c("begin", "effort", "unit", "year", "month")
    )
  )
  expect_equal(class(effort_by_month$month), "numeric")

  # right cols and col types: group by week
  effort_by_week<- get_custom_effort(mica, group_by = "week")
  expect_true(
    all(
      colnames(effort_by_week) == c("begin", "effort", "unit", "year", "week")
    )
  )
  expect_equal(class(effort_by_week$week), "numeric")

  # right cols and col types: group by day
  effort_by_day<- get_custom_effort(mica, group_by = "day")
  expect_true(
    all(
      colnames(effort_by_day) == c("begin",
                                    "effort",
                                    "unit",
                                    "year",
                                    "month",
                                    "day")
    )
  )
  expect_equal(class(effort_by_day$day), "integer")

  # number of rows is equal to 1 if group_by is NULL
  expect_equal(nrow(tot_effort), 1)

  # number of rows with grouping by year is equal to number of years in
  # deployments
  first_year <- lubridate::year(min(mica$deployments$start))
  last_year <- lubridate::year(max(mica$deployments$end))
  n_years <- last_year - first_year + 1
  expect_equal(nrow(effort_by_year), n_years)

  # number of rows with grouping by year is equal to number of months from start
  # very first deployment and end of the very last deployment
  first_month <- lubridate::month(min(mica$deployments$start, na.rm = TRUE))
  last_month <- lubridate::month(max(mica$deployments$end, na.rm = TRUE))
  n_months <- max(last_year - first_year - 1, 0) * 12 +
    (12-first_month + 1) + last_month
  expect_equal(nrow(effort_by_month), n_months)

  # number of rows for weekly groups is higher than for monthly groups
  expect_gte(nrow(effort_by_week), nrow(effort_by_month))

  # number of rows for daily groups is higher than for weekly groups
  expect_gte(nrow(effort_by_day), nrow(effort_by_week))

  # number of rows with start defined lower than for entire datapackage
  set_start <- get_custom_effort(mica,
                                 start = as.Date("2021-01-01"),
                                 group_by = "month")
  expect_lt(nrow(set_start), nrow(effort_by_month))

  # number of rows with end defined lower than for entire datapackage
  set_end <- get_custom_effort(mica,
                               end = as.Date("2021-01-01"),
                               group_by = "month")
  expect_lt(nrow(set_end), nrow(effort_by_month))

  # number of rows with both specific start and end is the lowest
  set_start_end <- get_custom_effort(mica,
                                     start= as.Date("2020-08-01"),
                                     end = as.Date("2021-01-01"),
                                     group_by = "month")
  expect_lt(nrow(set_end), nrow(effort_by_month))
})

test_that("check effort and unit values", {
  tot_effort <- get_custom_effort(mica)
  # filtering deployments reduces effort value
  filter_deploys <- get_custom_effort(mica,
                                      pred_gte("latitude", 51.18),
                                      group_by = "year")
  expect_lt(filter_deploys$effort, tot_effort$effort)

  # effort in hours is higher than effort in days
  tot_effort_days <- get_custom_effort(mica, unit = "day")
  expect_gt(tot_effort$effort, tot_effort_days$effort)

  # unit value is equal to hour if default unit value is used
  expect_equal(unique(tot_effort$unit), "hour")
  # unit value is equal to day if unit value is set to "day"
  expect_equal(unique(tot_effort_days$unit), "day")
})
