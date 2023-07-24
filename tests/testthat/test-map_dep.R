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

test_that("map_dep() can handle combinations of arguments", {
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

test_that("map_dep() can toggle showing deployments with zero values", {
  # expect an error when the toggle has length > 1
  expect_error(map_dep(mica, feature = "n_obs",
                       zero_values_show = c(TRUE, TRUE)),
               regexp = "zero_values_show must be a logical: TRUE or FALSE.")
  # expect an error when the toggle is not TRUE or FALSE
  expect_error(map_dep(mica, feature = "n_obs",
                       zero_values_show = "dax"),
               regexp = "zero_values_show must be a logical: TRUE or FALSE.")
  expect_error(map_dep(mica, feature = "n_obs",
                       zero_values_show = NA),
               regexp = "zero_values_show must be a logical: TRUE or FALSE.")
  # expect a message when an url/size is provided but the toggle is off
  suppressMessages(expect_message(
    map_dep(mica, feature = "n_obs", zero_values_show = FALSE),
    regexp = "`zero_values_show` is FALSE: `zero_values_icon_url` ignored.",
    fixed = TRUE
  ))
  
  suppressMessages(expect_message(
    map_dep(mica, feature = "n_obs", zero_values_show = FALSE),
    regexp = "`zero_values_show` is FALSE: `zero_values_icon_size` is ignored.",
    fixed = TRUE
  ))
  
  expect_no_message(
    map_dep(mica,feature = "n_species", zero_values_show = TRUE)
  )
})

test_that("map_dep() can toggle showing deployments with NA values", {
  # expect an error when the toggle has length > 1
  expect_error(map_dep(mica, feature = "n_obs",
                       na_values_show = c(TRUE, TRUE)),
               regexp = "na_values_show must be a logical: TRUE or FALSE.")
  # expect an error when the toggle is not TRUE or FALSE
  expect_error(map_dep(mica, feature = "n_obs",
                       na_values_show = "dax"),
               regexp = "na_values_show must be a logical: TRUE or FALSE.")
  expect_error(map_dep(mica, feature = "n_obs",
                       na_values_show = NA),
               regexp = "na_values_show must be a logical: TRUE or FALSE.")
  # expect a message when an url/size is provided but the toggle is off
  suppressMessages(expect_message(
    map_dep(mica, feature = "n_obs", na_values_show = FALSE),
    regexp = "`na_values_show` is FALSE: `na_values_icon_url` ignored.",
    fixed = TRUE
  ))
  
  suppressMessages(expect_message(
    map_dep(mica, feature = "n_obs", na_values_show = FALSE),
    regexp = "`na_values_show` is FALSE: `na_values_icon_size` is ignored.",
    fixed = TRUE
  ))
  
  expect_no_message(
    map_dep(mica,feature = "n_species", na_values_show = TRUE)
  )
  
})

test_that("map_dep() can calculate and get feature values", {
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
    map_dep(mica, feature = "rai", species = "krakeend"),
    regexp =
      glue::glue("There are 3 deployments without observations: {no_obs_deployments_str}"),
    fixed = TRUE
  ))
  
  suppressMessages(expect_message(
    map_dep(mica, feature = "rai_individuals", species = "krakeend"),
    regexp =
      glue::glue("There are 3 deployments without observations: {no_obs_deployments_str}"),
    fixed = TRUE
  ))
  expect_warning(
    map_dep(mica, feature = "n_species", species = "krakeend"),
    regexp = "`species` ignored for `feature = n_species`",
    fixed = TRUE
  )
})

test_that("map_dep() allows for scale modifications", {
  expect_no_warning(map_dep(
    mica,
    feature = "n_species",
    max_scale = 0,
    relative_scale = FALSE
  ))
  expect_warning(map_dep(mica, feature = "effort", max_scale = 0),
    regexp = "Relative scale used: max_scale value ignored.",
    fixed = TRUE
  )
  expect_error(map_dep(mica, feature = "effort", relative_scale = FALSE),
    regexp = "If you use an absolute scale, `max_scale` must be a number, not `NULL`.",
    fixed = TRUE
  )
})

test_that("map_dep() allows disabling of hover columns", {
  map_no_hover <- map_dep(mica, feature = "effort", hover_columns = NULL)

  expect_true(
    all(
      is.na(map_no_hover$x$calls[[3]]$args[[11]])
    )
  )
  map_hover <-
    map_dep(mica, feature = "n_individuals", hover_columns = "scientificName")
  expect_true(
    any(
      !is.na(map_hover$x$calls[[3]]$args[[11]])
    )
  )
  
  
})

test_that("map_dep() allows filtering by predicates", {
  # expect_no_error(
  #   map_dep(mica,
  #           pred("scientificName", "Anas platyrhynchos"),
  #           feature = "n_species")
  #   )
  
  expect_message(
    map_dep(mica, pred_gt("latitude", 51.18), feature = "n_species"),
    regexp = "df %>% dplyr::filter((latitude > 51.18))",
    fixed = TRUE)
  
  suppressMessages(expect_message(
    map_dep(mica, pred_gt("latitude", 90), feature = "n_species"),
    regexp = "No deployments left.",
    fixed = TRUE))
  
  suppressMessages(expect_message(
    map_dep(mica, pred_gt("latitude", 90), feature = "n_species"),
    regexp = "df %>% dplyr::filter((latitude > 90))",
    fixed = TRUE))
})

test_that("map_dep() returns a leaflet", {
  expect_s3_class(map_dep(mica, feature = "n_species"), c("leaflet", "htmlwidget"))
  expect_no_warning(map_dep(mica, feature = "n_species"))
  expect_no_error(map_dep(mica, feature = "n_species"))
  expect_no_message(map_dep(mica, feature = "n_species"))
})
