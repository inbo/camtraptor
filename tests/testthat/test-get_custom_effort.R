test_that("get_custom_effort returns error for invalid group_by value", {
  expect_error(get_custom_effort(mica, group_by = "bad_value"),
               regexp = paste0("Invalid value for group_by parameter: ",
                               "bad_value.\n",
                               "Valid inputs are: NULL, day, week, month ",
                               "and year."),
               fixed = TRUE
  )
})

test_that("get_custom_effort returns error for start not a Date", {
  expect_error(get_custom_effort(mica, start = "2021-01-01"))
  # no datetime allowed
  expect_error(
    get_custom_effort(
      mica, 
      start = lubridate::as_datetime("2021-12-05 22:25:01 CET")),
    regexp = paste0("`start` must be `NULL` or an object of class Date. ",
                    "Did you forget to convert a string to Date with ",
                    "`as.Date()`?"),
    fixed = TRUE
  )
})

test_that("get_custom_effort returns error for end not a Date", {
  expect_error(get_custom_effort(mica, end = "2021-01-01"))
  # no datetime allowed
  expect_error(
    get_custom_effort(mica,
                      end = lubridate::as_datetime("2021-12-05 22:25:01 CET")),
    regexp = paste0("`end` must be `NULL` or an object of class Date. ",
                    "Did you forget to convert a string to Date with ",
                    "`as.Date()`?"),
    fixed = TRUE
  )
})

test_that("get_custom_effort returns error if end earlier than start", {
  expect_error(
    get_custom_effort(mica,
                      start = as.Date("2021-01-01"),
                      end = as.Date("1990-01-01")),
    regexp = paste0("`end` value is set too early. `end` value must be not ",
                    "earlier than the start of the earliest deployment: ",
                    "2019-10-09."),
    fixed = TRUE
  )
})

test_that(
  "get_custom_effort returns error if start later than end of latest deployment", {
  expect_error(get_custom_effort(mica, start = as.Date("2030-01-01")),
               regexp = paste0(
                 "`start` value is set too late. ",
                 "`start` value must be not later than the end of the latest ",
                 "deployment: 2021-04-18."
                 ),
               fixed = TRUE
               )
})

test_that(
  "get_custom_effort returns error if end earlier than begin of first deployment", {
    expect_error(get_custom_effort(mica, end = as.Date("1900-04-05")),
                 regexp = paste0(
                   "`end` value is set too early. ",
                   "`end` value must be not earlier than the start of the ",
                   "earliest deployment: 2019-10-09."),
                 fixed = TRUE
    )
  })

test_that("get_custom_effort returns error for invalid effort units", {
  expect_error(get_custom_effort(mica, unit = "second"),
               regexp = paste0("Invalid value for unit parameter: second.\n",
                               "Valid inputs are: hour and day."),
               fixed = TRUE
  )
  expect_error(get_custom_effort(mica, unit = "year"),
               regexp = paste0("Invalid value for unit parameter: year.\n",
                               "Valid inputs are: hour and day."),
               fixed = TRUE
  )
})

test_that("get_custom_effort returns warning if start set too early", {
  start_too_early <- evaluate_promise(
    get_custom_effort(mica,
      start = as.Date("1900-01-01"),
      group_by = "day"
    )
  )
  expect_identical(
    start_too_early$warnings,
    paste0(
      "`start` value is set too early. ",
      "`start` authomatically set to start date of earliest ",
      "deployment: 2019-10-09."
    )
  )
  expect_identical(
    start_too_early$result$begin[1],
    lubridate::as_date(min(mica$data$deployments$start))
  )
})

test_that("get_custom_effort returns warning if end set too late", {
  end_too_late <- evaluate_promise(
    get_custom_effort(mica,
      end = as.Date("2100-01-01"),
      group_by = "day"
    )
  )
  expect_identical(
    end_too_late$warnings,
    paste0(
      "`end` value is set too late. ",
      "`end` authomatically set to end date of latest deployment: 2021-04-18."
    )
  )
  expect_identical(
    end_too_late$result$begin[nrow(end_too_late$result)],
    lubridate::as_date(max(mica$data$deployments$end))
  )
})


test_that("right columns, cols types, right relative number of rows", {
  # right cols and col types: no groups
  tot_effort <- get_custom_effort(mica)
  expect_named(tot_effort, expected = c("begin", "effort", "unit"))
  expect_s3_class(tot_effort$begin, "Date")
  expect_type(tot_effort$effort, "double")
  expect_type(tot_effort$unit, "character")

  # right cols and col types: group by year
  effort_by_year <- get_custom_effort(mica, group_by = "year")
  expect_true(
    all(colnames(effort_by_year) == c("begin", "effort", "unit"))
  )

  # right cols and col types: group by month
  effort_by_month <- get_custom_effort(mica, group_by = "month")
  expect_named(effort_by_month, expected = c("begin", "effort", "unit"))

  # right cols and col types: group by week
  effort_by_week <- get_custom_effort(mica, group_by = "week")
  expect_named(effort_by_week, expected = c("begin", "effort", "unit"))

  # right cols and col types: group by day
  effort_by_day <- get_custom_effort(mica, group_by = "day")
  expect_named(effort_by_day, expected = c("begin", "effort", "unit"))

  # number of rows is equal to 1 if group_by is NULL
  expect_identical(nrow(tot_effort), 1L)

  # number of rows with grouping by year is equal to number of calendar years
  first_day <- min(mica$data$deployments$start)
  last_day <- max(mica$data$deployments$end)
  n_years <- length(seq(
    lubridate::floor_date(first_day, unit = "years"),
    lubridate::floor_date(last_day, unit = "years"),
    by = "years")
  )
  expect_identical(nrow(effort_by_year), n_years)

  # number of rows with grouping by month is equal to number of calendar months
  n_months <- length(seq(
    lubridate::floor_date(first_day, unit = "months"),
    lubridate::floor_date(last_day, unit = "months"),
    by = "months")
  )
  expect_identical(nrow(effort_by_month), n_months)

  # number of rows with grouping by week is equal to number of calendar weeks
  n_weeks <- length(seq(
    lubridate::floor_date(first_day, unit = "weeks"),
    lubridate::floor_date(last_day, unit = "weeks"),
    by = "weeks")
  )
  expect_identical(nrow(effort_by_week), n_weeks)

  # number of rows for daily groups is higher than for weekly groups
  expect_gte(nrow(effort_by_day), nrow(effort_by_week))
  # number of rows for weekly groups is higher than for monthly groups
  expect_gte(nrow(effort_by_week), nrow(effort_by_month))
  # number of rows for monthly groups is higher than for yearly groups
  expect_gte(nrow(effort_by_month), nrow(effort_by_year))

  # number of rows with start not NULL is lower than with start = NULL
  set_start <- get_custom_effort(mica,
    start = as.Date("2020-08-01"),
    group_by = "month"
  )
  expect_lt(nrow(set_start), nrow(effort_by_month))

  # number of rows with end not NULL is lower than with end = NULL
  set_end <- get_custom_effort(mica,
    end = as.Date("2021-01-01"),
    group_by = "month"
  )
  expect_lt(nrow(set_end), nrow(effort_by_month))

  # number of rows with both specific start and end is the lowest
  set_start_end <- get_custom_effort(mica,
    start = as.Date("2020-08-01"),
    end = as.Date("2021-01-01"),
    group_by = "month"
  )
  expect_lt(nrow(set_start_end), nrow(set_end))
  expect_lt(nrow(set_start_end), nrow(set_start))
})

test_that("check effort and unit values", {
  tot_effort <- get_custom_effort(mica)
  # filtering deployments reduces effort value
  filter_deploys <- suppressMessages(
    get_custom_effort(mica,
      pred_gte("latitude", 51.18),
      group_by = "year"
    )
  )
  expect_lt(filter_deploys$effort, tot_effort$effort)

  # effort in hours is higher than effort in days
  tot_effort_days <- get_custom_effort(mica, unit = "day")
  expect_gt(tot_effort$effort, tot_effort_days$effort)

  # unit value is equal to hour if default unit value is used
  expect_identical(unique(tot_effort$unit), "hour")
  # unit value is equal to day if unit value is set to "day"
  expect_identical(unique(tot_effort_days$unit), "day")
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_custom_effort(datapkg = mica)
    ),
    regexp = "The `datapkg` argument of `get_custom_effort()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})
