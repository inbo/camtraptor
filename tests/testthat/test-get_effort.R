test_that("get_effort returns error if species is specified", {
  expect_error(get_effort(mica, species = "Mallard"))
  expect_error(get_effort(mica, species = character(0)))
})

test_that("get_effort returns error for invalid effort units", {
  expect_error(get_effort(mica, unit = "bad_unit"))
  expect_error(get_effort(mica, unit = NULL))
})

test_that("get_effort returns error for invalid datapackage", {
  expect_error(get_effort(mica$data$deployments))
})


test_that("values in column unit are all the same", {
  effort_df <- get_effort(mica)
  distinct_efffort_unit_values <- unique(effort_df$unit)
  expect_equal(length(distinct_efffort_unit_values), 1)
})

test_that("column effort_duration is of class 'Duration'", {
  effort_df <- get_effort(mica)
  expect_equal(class(effort_df$effort_duration)[1], "Duration")
  expect_equal(attr(class(effort_df$effort_duration),
                   which = "package"),
              "lubridate")
})

test_that("column unit is always equal to argument unit", {
  unit_to_test <- c("second", "minute", "hour", "day", "month", "year")
  for (chosen_unit in unit_to_test) {
    effort_df <- get_effort(mica, unit = chosen_unit)
    efffort_unit_value <- unique(effort_df$unit)
    expect_equal(efffort_unit_value, chosen_unit)
  }
})


test_that("get_effort returns the right dataframe", {
  effort_df <- get_effort(mica)

  # type list
  expect_type(effort_df, "list")

  # class tibble data.frame
  expect_equal(
    class(effort_df),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deploymentID, effort, unit and effort_duration only
  expect_equal(
    names(effort_df),
    c(
      "deploymentID",
      "effort",
      "unit",
      "effort_duration"
    )
  )
})


test_that("get_effort returns the right number of rows", {
  effort_df <- get_effort(mica)
  all_deployments <- unique(mica$data$deployments$deploymentID)
  n_all_deployments <- length(all_deployments)

  # number of rows should be equal to number of deployments
  expect_equal(
    nrow(effort_df),
    n_all_deployments
  )
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_effort(datapkg = mica)
    )
  )
})
