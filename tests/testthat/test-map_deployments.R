test_that("map_deployments() returns error when feature is missing", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(map_deployments(x),
    'argument "feature" is missing, with no default',
    fixed = TRUE
  )
})

test_that("map_deployments() returns error for invalid feature", {
  skip_if_offline()
  x <- example_dataset()
  valid_inputs <-
    c(
      "n_species",
      "n_obs",
      "n_individuals",
      "rai",
      "rai_individuals",
      "effort"
    )
  no_feature <- "not_a_feature"
  # invalid feature
  expect_error(
    map_deployments(x, feature = no_feature),
    paste0(
      "Invalid value for feature parameter: not_a_feature.\n",
      "Valid inputs are: n_species, n_obs, n_individuals, rai, ",
      "rai_individuals and effort"
    ),
    fixed = TRUE
  )
  # more than one feature
  expect_error(
    map_deployments(x, feature = valid_inputs[1:2]),
    "`feature` must have length 1",
    fixed = TRUE
  )
})

test_that("map_deployments() can handle combinations of arguments", {
  skip_if_offline()
  x <- example_dataset()
  expect_warning(
    map_deployments(x, feature = "n_species", effort_unit = "month"),
    "`effort_unit` ignored for `feature = n_species`.",
    fixed = TRUE
  )
})

test_that("map_deployments() can toggle showing deployments with zero values", {
  skip_if_offline()
  x <- example_dataset()
  # expect an error when the toggle has length > 1
  expect_error(map_deployments(x, feature = "n_obs",
                       zero_values_show = c(TRUE, TRUE)),
               "zero_values_show must be a logical: TRUE or FALSE.",
               fixed = TRUE)
  # expect an error when the toggle is not TRUE or FALSE
  expect_error(map_deployments(x, feature = "n_obs",
                       zero_values_show = "dax"),
               "zero_values_show must be a logical: TRUE or FALSE.",
               fixed = TRUE)
  expect_error(map_deployments(x, feature = "n_obs",
                       zero_values_show = NA),
               "zero_values_show must be a logical: TRUE or FALSE.",
               fixed = TRUE)
  expect_error(map_deployments(x, feature = "n_obs",
                       zero_values_show = NULL),
               "zero_values_show must be a logical: TRUE or FALSE.",
               fixed = TRUE)
  # expect a message when an url/size is provided but the toggle is off
  suppressMessages(expect_message(
    map_deployments(x, feature = "n_obs", zero_values_show = FALSE),
    "`zero_values_show` is FALSE: `zero_values_icon_url` ignored.",
    fixed = TRUE
  ))
  
  suppressMessages(expect_message(
    map_deployments(x, feature = "n_obs", zero_values_show = FALSE),
    "`zero_values_show` is FALSE: `zero_values_icon_size` is ignored.",
    fixed = TRUE
  ))
  
  expect_no_message(
    map_deployments(x,feature = "n_species", zero_values_show = TRUE)
  )
})

test_that("map_deployments() can toggle showing deployments with NA values", {
  skip_if_offline()
  x <- example_dataset()
  # expect an error when the toggle has length > 1
  expect_error(map_deployments(x, feature = "n_obs",
                       na_values_show = c(TRUE, TRUE)),
               "na_values_show must be a logical: TRUE or FALSE.",
               fixed = TRUE)
  # expect an error when the toggle is not TRUE or FALSE
  expect_error(map_deployments(x, feature = "n_obs",
                       na_values_show = "dax"),
               "na_values_show must be a logical: TRUE or FALSE.",
               fixed = TRUE)
  expect_error(map_deployments(x, feature = "n_obs",
                       na_values_show = NA),
               "na_values_show must be a logical: TRUE or FALSE.",
               fixed = TRUE)
  expect_error(map_deployments(x, feature = "n_obs",
                       na_values_show = NULL),
               "na_values_show must be a logical: TRUE or FALSE.",
               fixed = TRUE)
  # expect a message when an url/size is provided but the toggle is off
  suppressMessages(expect_message(
    map_deployments(x, feature = "n_obs", na_values_show = FALSE),
    "`na_values_show` is FALSE: `na_values_icon_url` ignored.",
    fixed = TRUE
  ))
  
  suppressMessages(expect_message(
    map_deployments(x, feature = "n_obs", na_values_show = FALSE),
    "`na_values_show` is FALSE: `na_values_icon_size` is ignored.",
    fixed = TRUE
  ))
  
  expect_no_message(
    map_deployments(x,feature = "n_species", na_values_show = TRUE)
  )
  
})

test_that("map_deployments() can calculate and get feature values", {
  skip_if_offline()
  x <- example_dataset()
  no_obs_deployments <-
    c(
      "577b543a-2cf1-4b23-b6d2-cda7e2eac372",
      "62c200a9-0e03-4495-bcd8-032944f6f5a1",
      "7ca633fa-64f8-4cfc-a628-6b0c419056d7"
    )
  no_obs_deployments_str <-
    sub(
      ",([^,]*)$",
      " and\\1",
      paste(no_obs_deployments, collapse = ", ")
    )
  suppressMessages(expect_message(
    map_deployments(x, feature = "rai", species = "krakeend"),
    glue::glue("There are 3 deployments without observations: {no_obs_deployments_str}"),
    fixed = TRUE
  ))
  
  suppressMessages(expect_message(
    map_deployments(x, feature = "rai_individuals", species = "krakeend"),
    glue::glue("There are 3 deployments without observations: {no_obs_deployments_str}"),
    fixed = TRUE
  ))
  expect_warning(
    map_deployments(x, feature = "n_species", species = "krakeend"),
    "`species` ignored for `feature = n_species`",
    fixed = TRUE
  )
})

test_that("map_deployments() allows for scale modifications", {
  skip_if_offline()
  x <- example_dataset()
  expect_no_warning(map_deployments(
    x,
    feature = "n_species",
    max_scale = 0,
    relative_scale = FALSE
  ))
  expect_warning(map_deployments(x, feature = "effort", max_scale = 0),
    "Relative scale used: max_scale value ignored.",
    fixed = TRUE
  )
  expect_error(map_deployments(x, feature = "effort", relative_scale = FALSE),
    "If you use an absolute scale, `max_scale` must be a number, not `NULL`.",
    fixed = TRUE
  )
})

test_that("map_deployments() allows disabling of hover columns", {
  skip_if_offline()
  x <- example_dataset()
  map_no_hover <- map_deployments(x, feature = "effort", hover_columns = NULL)

  expect_true(
    all(
      is.na(map_no_hover$x$calls[[3]]$args[[11]])
    )
  )
  map_hover <-
    map_deployments(x, feature = "n_individuals", hover_columns = "scientificName")
  expect_true(
    any(
      !is.na(map_hover$x$calls[[3]]$args[[11]])
    )
  )
})

test_that("map_deployments() returns a leaflet", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(map_deployments(x, feature = "n_species"), c("leaflet", "htmlwidget"))
  expect_no_warning(map_deployments(x, feature = "n_species"))
  expect_no_error(map_deployments(x, feature = "n_species"))
  expect_no_message(map_deployments(x, feature = "n_species"))
})

test_that("map_dep() is deprecated", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_deprecated(map_dep(x, "n_species"))
})

test_that("output of map_dep() is the same as map_deployments()", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(
    suppressWarnings(map_dep(x, "n_species")),
    map_deployments(x, "n_species")
  )
})

test_that("Check all three deprecations: function and args sex and life_stage", {
  skip_if_offline()
  x <- example_dataset()
  warnings <- list()
  # Using the (superseded) `capture_warnings()` doesn't retain the
  # class of the warnings. Still, not worth to go more complex
  warnings <- capture_warnings(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      suppressMessages(
        map_dep(x, "n_species", sex = "female", life_stage = "adult")
      )
    )
  )
  
  # Test length of the warnings
  expect_length(warnings, 3)
  
  # Test the content of the warnings (partial match is enough)
  expect_match(
    warnings[[1]],
    "`map_dep\\(\\)` was deprecated in camtraptor 1.0.0.",
    all = FALSE
  )
  expect_match(
    warnings[[2]],
    paste0("The `sex` argument of `map_dep\\(\\)` ", 
           "is deprecated as of camtraptor 1.0.0."),
    all = FALSE
  )
  expect_match(
    warnings[[3]],
    paste0("The `life_stage` argument of `map_dep\\(\\)` ",
           "is deprecated as of camtraptor 1.0.0."),
    all = FALSE
  )
})

