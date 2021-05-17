test_that("get_rai returns error if no species is specified", {
  expect_error(get_rai(camtrapdp, species = NULL))
  expect_error(get_rai(camtrapdp, species = character(0)))
})

test_that("get_rai returns the right dataframe", {
  output_anas_platyrhyncos <- get_rai(camtrapdp,
    species = "Anas platyrhynchos"
  )

  # type list
  expect_type(output_anas_platyrhyncos, "list")

  # class tibble data.frame
  expect_equal(
    class(output_anas_platyrhyncos),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deployment_id scientific_name and rai only
  expect_equal(
    names(output_anas_platyrhyncos),
    c(
      "deployment_id",
      "scientific_name",
      "rai"
    )
  )
})

test_that("get_rai returns the right number of rows: all species selected", {
  all_species <- get_species(camtrapdp)
  all_deployments <- unique(camtrapdp$deployments$deployment_id)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate rai for all species
  output_all_species <- get_rai(camtrapdp,
    species = all_species$scientific_name
  )

  # number of rows should be equal to number of species by number of deployments
  expect_equal(
    nrow(output_all_species),
    n_all_species * n_all_deployments
  )
})

test_that("get_rai returns the same if 'all' is used instead of vector with all species", {
  all_species <- get_species(camtrapdp)
  all_deployments <- unique(camtrapdp$deployments$deployment_id)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate rai for all species using default "all" value
  output_all_species_default <- get_rai(camtrapdp, species = "all")
  # calculate rai for all species specifying the species
  output_all_species <- get_rai(camtrapdp,
    species = all_species$scientific_name
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  expect_equal(
    get_rai(camtrapdp, species = "Anas platyrhynchos"),
    get_rai(camtrapdp, species = toupper("Anas platyrhynchos"))
  )
})
