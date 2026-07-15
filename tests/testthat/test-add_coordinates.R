test_that("add_coordinates returns the expected data.frame", {
  skip_if_offline()
  x <- example_dataset()
  original_obs <- observations(x)
  x <- add_coordinates(x)
  # New observations df has same class as the original observations
  expect_identical(class(observations(x)), class(original_obs))
  # New observations df has the same number of rows as the original observations
  expect_identical(nrow(observations(x)), nrow(original_obs))
  # New observations contains all columns of original observations plus the
  # columns `latitude` and `longitude`
  expect_identical(
    colnames(observations(x)),
    c(colnames(original_obs), "latitude", "longitude")
  )
})

test_that("add_coordinates doesn't add coordinates if already present", {
  skip_if_offline()
  x <- example_dataset()
  # Add coordinates twice and check that the result is identical
  expect_identical(
    add_coordinates(x),
    suppressWarnings(
      add_coordinates(x) %>%
        add_coordinates()
    )
  )
  
  expect_warning(
    add_coordinates(x) %>% add_coordinates(),
    regexp = "Coordinates are not added because they already present in observations.",
    fixed = TRUE
  )
})

test_that("add_coordinates doesn't add coordinates if one is already present", {
  x <- example_dataset()
  # Add coordinates and remove the `longitude` column from the observations
  x_with_obs_lat <- x %>%
    add_coordinates()
  observations(x_with_obs_lat) <- observations(x_with_obs_lat) %>%
    dplyr::select(-"longitude")
  
  # Add coordinates again and check that the result is identical
  expect_identical(
    x_with_obs_lat,
    suppressWarnings(
      add_coordinates(x_with_obs_lat)
    )
  )
  
  # Check the warning message
  expect_warning(
    add_coordinates(x_with_obs_lat),
    regexp = "Coordinates are not added because latitude is already present in observations.",
    fixed = TRUE
  )
})
