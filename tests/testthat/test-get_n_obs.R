test_that("get_n_obs returns the right structure of dataframe", {
  skip_if_offline()
  x <- example_dataset()
  # Species arg specified
  output_anas_platyrhyncos <- suppressMessages(
    get_n_obs(x,
      species = "Anas platyrhynchos"
    )
  )

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

  # species arg is NULL
  output_general <- get_n_obs(x, species = NULL)

  # columns deploymentID and n
  expect_equal(
    names(output_general),
    c(
      "deploymentID",
      "n"
    )
  )
})


test_that("get_n_obs returns the right number of rows: all species selected", {
  skip_if_offline()
  x <- example_dataset()
  
  all_species <- get_species(x)
  all_deployments <- unique(purrr::pluck(deployments(x), "deploymentID"))

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # Calculate number of observations for all species
  output_all_species <- get_n_obs(x)

  # Number of rows should be equal to number of species by number of deployments
  expect_equal(
    nrow(output_all_species),
    n_all_species * n_all_deployments
  )
})

test_that(paste(
  "get_n_obs returns always the right number of rows:",
  "species undetected in one deployment"
), {
  skip_if_offline()
  x <- example_dataset()
  
  deployments <- unique(purrr::pluck(deployments(x), "deploymentID"))

  n_deployments <- length(deployments)

  # Calculate get_n_obs for a species undetected in one deployment
  output_ondatra_zibethicus <- suppressMessages(
    get_n_obs(x,
      species = "Anas strepera"
    )
  )

  # Number of rows should be equal to number of deployments
  expect_equal(nrow(output_ondatra_zibethicus), n_deployments)
})

test_that(
  "get_n_obs returns rows ordered by the original order of deployments",
  {
    skip_if_offline()
    x <- example_dataset()
    
    # Get the original order of deployment IDs
    deployment_ids <- unique(purrr::pluck(deployments(x), "deploymentID"))

    # Apply function
    n_obs <- get_n_obs(x)
    deployments_in_n_obs <- unique(n_obs$deploymentID)
    expect_equal(deployments_in_n_obs, deployment_ids)
  }
)

test_that("species = 'all' returns the same of using a vector with all species", {
  skip_if_offline()
  x <- example_dataset()
  all_species <- get_species(x)
  all_deployments <- unique(purrr::pluck(deployments(x), "deploymentID"))

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of observations for all species using default "all" value
  output_all_species_default <- get_n_obs(x, species = "all")
  # calculate number of observations for all species specifying the species
  output_all_species <- get_n_obs(x,
    species = all_species$scientificName
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  skip_if_offline()
  x <- example_dataset()
  expect_equal(
    suppressMessages(
      get_n_obs(x, species = "Anas platyrhynchos")
    ),
    suppressMessages(get_n_obs(x, species = toupper("ANAS platYrhyncHOS")))
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
  # define correspondent vernacular name
  vn <- "Mallard"

  # Get number of observations for both cases
  output_anas_platyrhyncos <- suppressMessages(get_n_obs(x, species = scn))
  output_mallard <- suppressMessages(get_n_obs(x, species = vn))

  # Same outputs
  expect_equal(output_anas_platyrhyncos, output_mallard)
})

test_that("if subset of species is specified, less observations are returned", {
  skip_if_offline()
  x <- example_dataset()
  
  output_all_species <- get_n_obs(x)
  output_anas_platyrhyncos <- suppressMessages(
    get_n_obs(x,
      species = "Anas platyrhynchos"
    )
  )

  expect_true(sum(output_all_species$n) >= sum(output_anas_platyrhyncos$n))
})

test_that(paste(
  "species is NULL returns an equal or higher number of",
  "observations than species = 'all'"
), {
  skip_if_offline()
  x <- example_dataset()
  
  output_all_species_collapsed <- get_n_obs(x, species = NULL)
  output_anas_platyrhyncos <- get_n_obs(x, species = "all") # default

  expect_true(
    sum(output_all_species_collapsed$n) >= sum(output_anas_platyrhyncos$n)
  )
})

test_that("get_n_obs returns a warning if 'all' is used with other values", {
  skip_if_offline()
  x <- example_dataset()
  
  all_species <- get_species(x)

  # Use 'all' with other species
  expect_warning(get_n_obs(x,
    species = c("all", all_species[1])
  ))
})

test_that(paste(
  "number of observations is equal to number of",
  "distinct sequenceID  values"
), {
  skip_if_offline()
  x <- example_dataset()
  deploy_id <- "29b7d356-4bb4-4ec4-b792-2af5cc32efa8"
  species <- "Anas platyrhynchos"
  n_obs_via_sequence_id <-
    observations(x) %>%
    dplyr::filter(.data$deploymentID == deploy_id) %>%
    dplyr::filter(.data$scientificName == species) %>%
    dplyr::pull(.data$sequenceID) %>%
    dplyr::n_distinct()
  # one sequenceID  linked to two observations (different age, sex and count)
  n_obs <- suppressMessages(
  get_n_obs(
    filter_observations(x, deploymentID == deploy_id),
    species = "Mallard"
  )
)
  expect_equal(n_obs$n, n_obs_via_sequence_id)
})

test_that(paste(
  "scientific_name column contains all specified species,",
  "even if all 0s are returned"
), {
  skip_if_offline()
  x <- example_dataset()
  species_value <- "Anas platyrhynchos"
  n_obs <- suppressMessages(
    get_n_obs(x, species = species_value)
  )
  expect_true(all(n_obs$scientificName %in% species_value))
  expect_true(all(species_value %in% n_obs$scientificName))
})
