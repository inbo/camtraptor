test_that("get_n_individuals returns the right structure of dataframe", {
  skip_if_offline()
  x <- example_dataset()
  # Species arg specified
  output_anas_platyrhyncos <- suppressMessages(get_n_individuals(x,
    species = "Anas platyrhynchos"
  ))

  # Type list
  expect_type(output_anas_platyrhyncos, "list")

  # Class tibble data.frame
  expect_equal(
    class(output_anas_platyrhyncos),
    c("tbl_df", "tbl", "data.frame")
  )

  # Columns deploymentID, scientificName and n
  expect_equal(
    names(output_anas_platyrhyncos),
    c(
      "deploymentID",
      "scientificName",
      "n"
    )
  )

  # Species arg is NULL
  output_general <- get_n_individuals(x, species = NULL)

  # Columns deploymentID and n
  expect_equal(
    names(output_general),
    c(
      "deploymentID",
      "n"
    )
  )
})

test_that("get_n_individuals returns the right number of rows: all species selected", {
  skip_if_offline()
  x <- example_dataset()
  all_species <- get_species(x)
  all_deployments <- unique(deployments(x)$deploymentID)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # Calculate number of individuals for all species
  output_all_species <- get_n_individuals(x)

  # Number of rows should be equal to number of species by number of deployments
  expect_equal(
    nrow(output_all_species),
    n_all_species * n_all_deployments
  )
})

test_that(paste(
  "get_n_individuals returns always the right number of rows:",
  "species undetected in one deployment"
), {
  skip_if_offline()
  x <- example_dataset()
  deployments <- unique(deployments(x)$deploymentID)

  n_deployments <- length(deployments)

  # Calculate get_n_individuals for a species undetected in one deployment
  output_martes_foina <- suppressMessages(get_n_individuals(x,
    species = "Martes foina"
  ))

  # Number of rows should be equal to number of deployments
  expect_equal(nrow(output_martes_foina), n_deployments)
})

test_that(
  "get_n_individuals returns rows ordered by the original order of deployments",
  {
    skip_if_offline()
    x <- example_dataset()
    # Get the original order of deployment IDs
    deploymentIDs <- unique(deployments(x)$deploymentID)

    # Apply function
    n_individuals <- get_n_individuals(x)
    deployments_in_n_individuals <- unique(n_individuals$deploymentID)
    expect_equal(deployments_in_n_individuals, deploymentIDs)
  }
)

test_that("species = 'all' returns the same of using a vector with all species", {
  skip_if_offline()
  x <- example_dataset()
  all_species <- get_species(x)
  all_deployments <- unique(deployments(x)$deploymentID)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # Calculate number of individuals for all species using default "all" value
  output_all_species_default <- get_n_individuals(x, species = "all")
  # Calculate number of individuals for all species specifying the species
  output_all_species <- get_n_individuals(x,
    species = all_species$scientificName
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  skip_if_offline()
  x <- example_dataset()
  expect_equal(
    suppressMessages(get_n_individuals(x, species = "Anas platyrhynchos")),
    suppressMessages(get_n_individuals(x, species = toupper("Anas platyrhynchos")))
  )
})

test_that(paste(
  "species accepts use of common names and return",
  "the same as using scientic name"
), {
  skip_if_offline()
  x <- example_dataset()
  # Define scientific name
  scn <- "Anas platyrhynchos"
  # Define correspondent vernacular name
  vn <- "Mallard"

  # Get number of individuals for both cases
  output_anas_platyrhyncos <- suppressMessages(get_n_individuals(x, species = scn))
  output_mallard <- suppressMessages(get_n_individuals(x, species = vn))

  # Same outputs
  expect_equal(output_anas_platyrhyncos, output_mallard)
})

test_that("if subset of species is specified, less individuals are returned", {
  skip_if_offline()
  x <- example_dataset()
  output_all_species <- get_n_individuals(x)
  output_anas_platyrhyncos <- suppressMessages(get_n_individuals(x,
    species = "Anas platyrhynchos"
  ))

  expect_true(sum(output_all_species$n) >= sum(output_anas_platyrhyncos$n))
})

test_that(paste(
  "species is NULL returns an equal or higher number of",
  "individuals than species = 'all'"
), {
  skip_if_offline()
  x <- example_dataset()
  output_all_species_collapsed <- get_n_individuals(x, species = NULL)
  output_anas_platyrhyncos <- get_n_individuals(x, species = "all") # default

  expect_true(
    sum(output_all_species_collapsed$n) >= sum(output_anas_platyrhyncos$n)
  )
})

test_that("get_n_individuals returns a warning if 'all' is used with other values", {
  skip_if_offline()
  x <- example_dataset()
  all_species <- get_species(x)

  # Use 'all' with other species
  expect_warning(get_n_individuals(x,
    species = c("all", all_species[1])
  ))
})

test_that("number of individuals is equal to sum of counts", {
  skip_if_offline()
  x <- example_dataset()
  deploy_id <- "29b7d356-4bb4-4ec4-b792-2af5cc32efa8"
  species <- "Anas platyrhynchos"
  n_individuals_via_count <-
    observations(x) %>%
    dplyr::filter(deploymentID == deploy_id) %>%
    dplyr::filter(scientificName == species) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals <- suppressMessages(get_n_individuals(
    filter_deployments(x, deploymentID == deploy_id) %>%
      filter_observations(scientificName == species)
  ))
  expect_equal(n_individuals$n, n_individuals_via_count)
})

test_that("sex filters data correctly", {
  skip_if_offline()
  x <- example_dataset()
  sex_value <- "unknown"
  n_individuals_via_count <-
    observations(x) %>%
    dplyr::filter(sex == sex_value) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals_females <-
    suppressMessages(get_n_individuals(x, species = NULL, sex = sex_value))
  tot_n_individuals_females <- sum(n_individuals_females$n)
  expect_equal(tot_n_individuals_females, n_individuals_via_count)
  expect_equal(nrow(n_individuals_females), nrow(deployments(x)))
})

test_that("multiple sex values allowed", {
  skip_if_offline()
  x <- example_dataset()
  sex_value <- c("female", "unknown")
  n_individuals_females_undefined_via_count <-
    observations(x) %>%
    dplyr::filter(sex %in% sex_value) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals_females_undefined <- suppressMessages(get_n_individuals(x,
    species = NULL,
    sex = sex_value
  ))
  tot_n_individuals_females_undefined <- sum(n_individuals_females_undefined$n)
  expect_equal(
    tot_n_individuals_females_undefined,
    n_individuals_females_undefined_via_count
  )
  expect_equal(
    nrow(n_individuals_females_undefined),
    nrow(deployments(x))
  )
})

test_that("life stage filters data correctly", {
  skip_if_offline()
  x <- example_dataset()
  life_stage_value <- "subadult"
  n_individuals_juvenile_via_count <-
    observations(x) %>%
    dplyr::filter(lifeStage == life_stage_value) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals_juvenile <- suppressMessages(
    get_n_individuals(x, species = NULL, life_stage = life_stage_value)
  )
  tot_n_individuals_juvenile <- sum(n_individuals_juvenile$n)
  expect_equal(tot_n_individuals_juvenile, n_individuals_juvenile_via_count)
  expect_equal(nrow(n_individuals_juvenile), nrow(deployments(x)))
})

test_that("multiple age values allowed", {
  skip_if_offline()
  x <- example_dataset()
  life_stage_value <- c("subadult", "adult")
  n_individuals_juvenile_adult_via_count <-
    observations(x) %>%
    dplyr::filter(lifeStage %in% life_stage_value) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals_juvenile_adult <- suppressMessages(
    get_n_individuals(x,
      species = NULL,
      life_stage = life_stage_value
    )
  )
  tot_n_individuals_juvenile_adult <- sum(n_individuals_juvenile_adult$n)
  expect_equal(
    tot_n_individuals_juvenile_adult,
    n_individuals_juvenile_adult_via_count
  )
  expect_equal(nrow(n_individuals_juvenile_adult), nrow(deployments(x)))
})

test_that("error returned if life stage or sex is not present", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(get_n_individuals(x, life_stage = "bad"))
  expect_error(get_n_individuals(x, sex = "bad"))
})

test_that("Argument datapkg is deprecated: warning returned", {
  skip_if_offline()
  x <- example_dataset()
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_n_individuals(datapkg = x)
    ),
    "The `datapkg` argument of `get_n_individuals()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})
