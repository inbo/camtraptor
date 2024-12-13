test_that("n_species returns the right dataframe", {
  skip_if_offline()
  x <- example_dataset()
  output_get_n_species <- n_species(x)

  # type list
  expect_type(output_get_n_species, "list")

  # class tibble data.frame
  expect_equal(
    class(output_get_n_species),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deploymentID and n only
  expect_equal(
    names(output_get_n_species),
    c(
      "deploymentID",
      "n"
    )
  )
})

test_that("n_species returns 0 for obs without recognized species", {
  skip_if_offline()
  x <- example_dataset()
  # Set scientificName equal to NA for all observations = unrecognized species
  observations(x) <- observations(x) %>% dplyr::mutate(scientificName = NA)
  expect_true(all(dplyr::pull(n_species(x), "n") == 0))
})

test_that("n_species returns NA for deployments without observations", {
  skip_if_offline()
  x <- example_dataset()
  # Create data package with no observations
  observations(x) <- observations(x)[0,]
  n_sp <- suppressMessages(n_species(x))
  expect_true(all(is.na(dplyr::pull(n_sp, "n"))))
})

test_that("n_species returns NA for deployments with only media-based observations", {
  skip_if_offline()
  x <- example_dataset()
  # Create data package without event-based observations
  observations(x) <- x %>%
    filter_observations(.data$observationLevel == "media") %>%
    observations()
  n_sp <- suppressMessages(n_species(x))
  expect_true(all(is.na(dplyr::pull(n_sp, "n"))))
})

test_that("get_n_species() is deprecated and calls n_species()", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_deprecated(get_n_species(x),
                               "was deprecated in camtraptor 1.0.0.",
                               fixed = TRUE)
})

test_that("output of get_n_species() is the same as n_species()", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(suppressWarnings(get_n_species(x)), n_species(x))
})
