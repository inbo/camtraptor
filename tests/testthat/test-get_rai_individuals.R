test_that("get_rai_individuals returns error if no species is specified", {
  expect_error(get_rai_individuals(mica, species = NULL))
  expect_error(get_rai_individuals(mica, species = character(0)))
})

test_that("get_rai_individuals returns the right dataframe", {
  output_anas_platyrhyncos <- suppressMessages(
    get_rai_individuals(mica,
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

test_that("get_rai_individuals returns the right number of rows: all species selected", {
  all_species <- get_species(mica)
  all_deployments <- unique(mica$data$deployments$deploymentID)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate rai for all species
  output_all_species <- get_rai_individuals(mica,
    species = all_species$scientificName
  )

  # number of rows should be equal to number of species by number of deployments
  expect_equal(
    nrow(output_all_species),
    n_all_species * n_all_deployments
  )
})

test_that("get_rai_individuals returns the same if 'all' is used instead of vector with all species", {
  all_species <- get_species(mica)
  all_deployments <- unique(mica$deployments$deploymentID)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate rai for all species using default "all" value
  output_all_species_default <- get_rai_individuals(mica, species = "all")
  # calculate rai for all species specifying the species
  output_all_species <- get_rai_individuals(mica,
    species = all_species$scientificName
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  expect_equal(
    suppressMessages(
      get_rai_individuals(mica, species = "Anas platyrhynchos")
    ),
    suppressMessages(
      get_rai_individuals(mica, species = toupper("Anas platyrhynchos"))
    )
  )
})

test_that("sex filters data correctly", {
  sex_value <- "female"
  n_obs_females <- suppressMessages(
    get_n_obs(mica, species = "Mallard", sex = sex_value)
  )
  rai_females <- suppressMessages(
    get_rai_individuals(mica, species = "Mallard", sex = sex_value)
  )
  # same first two cols as in get_n_obs
  expect_equal(names(n_obs_females)[1:2], names(rai_females)[1:2])
  expect_equal(nrow(n_obs_females), nrow(rai_females))
  expect_equal(n_obs_females[, 1:2], rai_females[, 1:2],
    ignore_attr = TRUE
  )
})

test_that("life_stage filters data correctly", {
  life_stage_value <- "subadult"
  n_obs_subadult <- suppressMessages(
    get_n_obs(mica, species = "Mallard", life_stage = life_stage_value)
  )
  rai_subadult <- suppressMessages(
    get_rai_individuals(mica, species = "Mallard", life_stage = life_stage_value)
  )
  # same first two cols as in get_n_obs
  expect_equal(names(n_obs_subadult)[1:2], names(rai_subadult)[1:2])
  expect_equal(nrow(n_obs_subadult), nrow(rai_subadult))
  expect_equal(n_obs_subadult[, 1:2], rai_subadult[, 1:2],
    ignore_attr = TRUE
  )
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_rai_individuals(datapkg = mica)
    ),
    "The `datapkg` argument of `get_rai_individuals()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})
