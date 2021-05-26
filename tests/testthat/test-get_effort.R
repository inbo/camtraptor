test_that("get_effort returns error if species is specified", {
  expect_error(get_effort(camtrapdp, species = "Mallard"))
  expect_error(get_effort(camtrapdp, species = character(0)))
})

test_that("get_effort returns error for invalid effort units", {
  expect_error(get_effort(camtrapdp, unit = "bad_unit"))
})

test_that("get_effort returns error for invalid datapackage", {
  expect_error(get_effort(camtrapdp$deployments))
})


test_that("values in column effort_unit are all the same", {
  effort_df <- get_effort(camtrapdp)
  distinct_efffort_unit_values <- unique(effort_df$effort_unit)
  expect_equal(length(distinct_efffort_unit_values), 1)
})

test_that("column effort_unit is equal to 'Duration' if unit is NULL", {
  effort_df <- get_effort(camtrapdp)
  efffort_unit_value <- unique(effort_df$effort_unit)
  expect_equal(efffort_unit_value, "Duration")
})

test_that("column effort_unit is always equal to unit if unit is not NULL", {
  unit_to_test <- c("second", "minute", "hour", "day", "month", "year")
  for (chosen_unit in unit_to_test) {
    effort_df <- get_effort(camtrapdp, unit = chosen_unit)
    efffort_unit_value <- unique(effort_df$effort_unit)
    expect_equal(efffort_unit_value, chosen_unit)
  }
})


test_that("get_effort returns the right dataframe", {
  effort_df <- get_effort(camtrapdp)

  # type list
  expect_type(effort_df, "list")

  # class tibble data.frame
  expect_equal(
    class(effort_df),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deployment_id effort and effort_unit only
  expect_equal(
    names(effort_df),
    c(
      "deployment_id",
      "effort",
      "effort_unit"
    )
  )
})


test_that("get_effort returns the right number of rows", {
  effort_df <- get_effort(camtrapdp)
  all_deployments <- unique(camtrapdp$deployments$deployment_id)
  n_all_deployments <- length(all_deployments)

  # number of rows should be equal to number of deployments
  expect_equal(
    nrow(effort_df),
    n_all_deployments
  )
})
