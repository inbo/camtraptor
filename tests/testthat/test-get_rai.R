test_that("get_rai returns error if no species is specified", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(get_rai(x, species = NULL))
  expect_error(get_rai(x, species = character(0)))
})

test_that("get_rai returns the right dataframe", {
  skip_if_offline()
  x <- example_dataset()
  output_anas_platyrhyncos <- suppressMessages(
    get_rai(x,
      species = "Anas platyrhynchos"
    )
  )

  # type list
  expect_type(output_anas_platyrhyncos, "list")

  # class tibble data.frame
  expect_equal(
    class(output_anas_platyrhyncos),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deploymentID scientificName and rai only
  expect_equal(
    names(output_anas_platyrhyncos),
    c(
      "deploymentID",
      "scientificName",
      "rai"
    )
  )
})

test_that("get_rai returns the right number of rows: all species selected", {
  skip_if_offline()
  x <- example_dataset()
  all_species <- get_species(x)
  all_deployments <- unique(purrr::pluck(deployments(x), "deploymentID"))

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate rai for all species
  output_all_species <- get_rai(x,
    species = all_species$scientificName
  )

  # number of rows should be equal to number of species by number of deployments
  expect_equal(
    nrow(output_all_species),
    n_all_species * n_all_deployments
  )
})

test_that("get_rai returns the same if 'all' is used instead of vector with all species", {
  skip_if_offline()
  x <- example_dataset()
  all_species <- get_species(x)
  all_deployments <- unique(purrr::pluck(deployments(x), "deploymentID"))

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate rai for all species using default "all" value
  output_all_species_default <- get_rai(x, species = "all")
  # calculate rai for all species specifying the species
  output_all_species <- get_rai(x,
    species = all_species$scientificName
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  skip_if_offline()
  x <- example_dataset()
  expect_equal(
    suppressMessages(get_rai(x, species = "Anas platyrhynchos")),
    suppressMessages(get_rai(x, species = toupper("Anas platyrhynchos")))
  )
})
