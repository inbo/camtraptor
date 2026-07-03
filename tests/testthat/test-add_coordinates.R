test_that("add_coordinates returns the expected data.frame", {
  skip_if_offline()
  x <- example_dataset()
  original_obs <- observations(x)
  x <- add_coordinates(x)
  # New bservations df has same class as the original observations
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
    regexp = "Coordinates already present in observations. Returning x.",
    fixed = TRUE
  )
})
