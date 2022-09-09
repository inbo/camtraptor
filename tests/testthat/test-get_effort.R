testthat::test_that("get_effort returns error for invalid effort units", {
  testthat::expect_error(
    get_effort(mica, unit = "bad_unit"),
    "Invalid value for unit parameter: bad_unit.
Valid inputs are: second, minute, hour, day, month and year.")
  testthat::expect_error(
    get_effort(mica, unit = NULL),
    "Invalid value for unit parameter: NULL.
Valid inputs are: second, minute, hour, day, month and year.")
})

testthat::test_that("get_effort returns error for invalid datapackage", {
  testthat::expect_error(get_effort(mica$data$deployments))
})


testthat::test_that("values in column unit are all the same", {
  effort_df <- get_effort(mica)
  distinct_efffort_unit_values <- unique(effort_df$unit)
  testthat::expect_equal(length(distinct_efffort_unit_values), 1)
})

testthat::test_that("column effort_duration is of class 'Duration'", {
  effort_df <- get_effort(mica)
  testthat::expect_equal(class(effort_df$effort_duration)[1], "Duration")
  testthat::expect_equal(
    attr(class(effort_df$effort_duration), which = "package"),
    "lubridate"
  )
})

testthat::test_that("column unit is always equal to argument unit", {
  unit_to_test <- c("second", "minute", "hour", "day", "month", "year")
  for (chosen_unit in unit_to_test) {
    effort_df <- get_effort(mica, unit = chosen_unit)
    efffort_unit_value <- unique(effort_df$unit)
    testthat::expect_equal(efffort_unit_value, chosen_unit)
  }
})


testthat::test_that("get_effort returns the right dataframe", {
  effort_df <- get_effort(mica)

  # type list
  testthat::expect_type(effort_df, "list")

  # class tibble data.frame
  testthat::expect_equal(
    class(effort_df),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deploymentID, effort, unit and effort_duration only
  testthat::expect_equal(
    names(effort_df),
    c(
      "deploymentID",
      "effort",
      "unit",
      "effort_duration"
    )
  )
})


testthat::test_that("get_effort returns the right number of rows", {
  effort_df <- get_effort(mica)
  all_deployments <- unique(mica$data$deployments$deploymentID)
  n_all_deployments <- length(all_deployments)

  # number of rows should be equal to number of deployments
  testthat::expect_equal(
    nrow(effort_df),
    n_all_deployments
  )
})

testthat::test_that("Argument datapkg is deprecated: warning returned", {
  testthat::expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_effort(datapkg = mica)
    )
  )
})
