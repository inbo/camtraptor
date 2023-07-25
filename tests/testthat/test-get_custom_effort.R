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
    end = as.Date("1990-01-01")
  ))
})

test_that("get_custom_effort returns error for invalid effort units", {
  expect_error(get_custom_effort(mica, unit = "second"))
  expect_error(get_custom_effort(mica, unit = "year"))
})

test_that("get_custom_effort returns warning if start set too early", {
  expect_warning(get_custom_effort(mica, start = as.Date("1990-01-01")))
})

test_that("get_custom_effort returns warning if start set too early", {
  start_too_early <- evaluate_promise(
    get_custom_effort(mica,
      start = as.Date("1900-01-01"),
      group_by = "day"
    )
  )
  expect_equal(
    start_too_early$warnings,
    paste0(
      "`start` is set too early. Earliest deployment start date: 2019-10-09. ",
      "With the given `group_by` value the earliest start possible is ",
      "2019-10-09. `start` is set to start date of earliest deployment: ",
      "2019-10-09."
    )
  )
  expect_equal(
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
  expect_equal(
    end_too_late$warnings,
    paste0(
      "`end` set too late. Latest deployment end date: 2021-04-18. ",
      "With the given `group_by` value the latest end possible is ",
      "2021-04-18. `end` is set to end date of latest deployment: ",
      "2021-04-18."
    )
  )
  expect_equal(
    end_too_late$result$begin[nrow(end_too_late$result)],
    lubridate::as_date(max(mica$data$deployments$end))
  )
})


test_that("right columns, cols types, right relative number of rows", {
  # right cols and col types: no groups
  tot_effort <- get_custom_effort(mica)
  expect_true(all(colnames(tot_effort) == c("begin", "effort", "unit")))
  expect_equal(class(tot_effort$begin), "Date")
  expect_equal(class(tot_effort$effort), "numeric")
  expect_equal(class(tot_effort$unit), "character")

  # right cols and col types: group by year
  effort_by_year <- get_custom_effort(mica, group_by = "year")
  expect_true(
    all(colnames(effort_by_year) == c("begin", "effort", "unit"))
  )

  # right cols and col types: group by month
  effort_by_month <- get_custom_effort(mica, group_by = "month")
  expect_true(
    all(colnames(effort_by_month) == c("begin", "effort", "unit"))
  )

  # right cols and col types: group by week
  effort_by_week <- get_custom_effort(mica, group_by = "week")
  expect_true(
    all(
      colnames(effort_by_week) == c("begin", "effort", "unit")
    )
  )

  # right cols and col types: group by day
  effort_by_day <- get_custom_effort(mica, group_by = "day")
  expect_true(
    all(
      colnames(effort_by_day) == c(
        "begin",
        "effort",
        "unit"
      )
    )
  )

  # number of rows is equal to 1 if group_by is NULL
  expect_equal(nrow(tot_effort), 1)

  # number of rows with grouping by year is equal to number of days divided by
  # 365
  first_day <- min(mica$data$deployments$start)
  last_day <- max(mica$data$deployments$end)
  n_years <- as.numeric(last_day - first_day) %/% 365 + 1
  expect_equal(nrow(effort_by_year), n_years)

  # number of rows with grouping by month is equal to number of days divided by
  # 30
  n_months <- as.numeric(last_day - first_day) %/% 30 + 1
  expect_equal(nrow(effort_by_month), n_months)

  # number of rows with grouping by week is equal to number of days divided by 7
  n_weeks <- as.numeric(last_day - first_day) %/% 7 + 1
  expect_equal(nrow(effort_by_week), n_weeks)

  # number of rows for daily groups is higher than for weekly groups
  expect_gte(nrow(effort_by_day), nrow(effort_by_week))

  # number of rows with start defined lower than for entire datapackage
  set_start <- get_custom_effort(mica,
    start = as.Date("2021-01-01"),
    group_by = "month"
  )
  expect_lt(nrow(set_start), nrow(effort_by_month))

  # number of rows with end defined lower than for entire datapackage
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
  expect_lt(nrow(set_end), nrow(effort_by_month))
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
  expect_equal(unique(tot_effort$unit), "hour")
  # unit value is equal to day if unit value is set to "day"
  expect_equal(unique(tot_effort_days$unit), "day")
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
