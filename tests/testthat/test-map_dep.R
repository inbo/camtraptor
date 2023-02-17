test_that("map_dep() returns error when feature is missing", {
  expect_error(map_dep(mica),
               regexp = 'argument "feature" is missing, with no default',
               fixed = TRUE
  )
})

test_that("map_dep() returns error for invalid feature", {
  valid_inputs <-
    c(
      "n_species",
      "n_obs",
      "n_individuals",
      "rai",
      "rai_individuals",
      "effort"
    )
  valid_input_string <-
    sub(",([^,]*)$", " and\\1", paste(valid_inputs, collapse = ", "))
  no_feature <- "not a feature"
  # invalid feature
  expect_error(
    map_dep(mica, feature = no_feature),
    regexp = glue::glue("Invalid value for feature parameter: {no_feature}.",
                        "Valid inputs are: {valid_input_string}.",
                        .sep = "\n"
    ),
    fixed = TRUE
  )
  # more than one feature
  expect_error(
    map_dep(mica, feature = valid_inputs[1:2]),
    regexp = "`feature` must have length 1",
    fixed = TRUE
  )
  
})

test_that("map_dep() can handle combinations of arguments",{
  expect_warning(
    map_dep(mica, feature = "n_species", effort_unit = "month"),
    regexp = "`effort_unit` ignored for `feature = n_species`.",
    fixed = TRUE
  )
  expect_warning(
    map_dep(mica, feature = "n_species", sex = "male"),
    regexp = "`sex` ignored for `feature = n_species`.",
    fixed = TRUE
  )
  expect_warning(
    map_dep(mica, feature = "n_species", life_stage = "subadult"),
    regexp = "`life_stage` ignored for `feature = n_species`.",
    fixed = TRUE
  )
})

test_that("map_dep() can toggle showing deployments with zero values",{
  # expect an error when the toggle is not TRUE or FALSE
  # expect_error(map_dep(mica, feature = "n_obs" ,zero_values_show = "dax"))
  # expect a message when an url/size is provided but the toggle is off
  expect_message(
    map_dep(mica, feature = "n_obs", zero_values_show = FALSE),
    regexp = "`zero_values_show` is FALSE: `zero_values_icon_url` ignored.",
    fixed = TRUE
    
  )
  expect_message(
    map_dep(mica, feature = "n_obs", zero_values_show = FALSE),
    regexp = "`zero_values_show` is FALSE: `zero_values_icon_size` is ignored.",
    fixed = TRUE
  )
})
