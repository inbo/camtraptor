test_that("get_n_obs returns the right structure of dataframe", {
  # species arg specified
  output_anas_platyrhyncos <- suppressMessages(
    get_n_obs(mica,
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

  # columns deploymentID, scientificName and n
  expect_equal(
    names(output_anas_platyrhyncos),
    c(
      "deploymentID",
      "scientificName",
      "n"
    )
  )

  # species arg is NULL
  output_general <- get_n_obs(mica, species = NULL)

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
  all_species <- get_species(mica)
  all_deployments <- unique(mica$data$deployments$deploymentID)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of observations for all species
  output_all_species <- get_n_obs(mica)

  # number of rows should be equal to number of species by number of deployments
  expect_equal(
    nrow(output_all_species),
    n_all_species * n_all_deployments
  )
})

test_that(paste(
  "get_n_obs returns always the right number of rows:",
  "species undetected in one deployment"
), {
  deployments <- unique(mica$data$deployments$deploymentID)

  n_deployments <- length(deployments)

  # calculate get_n_obs for a species undetected in one deployment
  output_ondatra_zibethicus <- suppressMessages(
    get_n_obs(mica,
      species = "Anas strepera"
    )
  )

  # number of rows should be equal to number of deployments
  expect_equal(nrow(output_ondatra_zibethicus), n_deployments)
})

test_that(
  "get_n_obs returns rows ordered by the original order of deployments",
  {
    # get the original order of deployment IDs
    deployment_ids <- unique(mica$data$deployments$deploymentID)

    # apply function
    n_obs <- get_n_obs(mica)
    deployments_in_n_obs <- unique(n_obs$deploymentID)
    expect_equal(deployments_in_n_obs, deployment_ids)
  }
)

test_that("species = 'all' returns the same of using a vector with all species", {
  all_species <- get_species(mica)
  all_deployments <- unique(mica$data$deployments$deploymentID)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of observations for all species using default "all" value
  output_all_species_default <- get_n_obs(mica, species = "all")
  # calculate number of observations for all species specifying the species
  output_all_species <- get_n_obs(mica,
    species = all_species$scientificName
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  expect_equal(
    suppressMessages(
      get_n_obs(mica, species = "Anas platyrhynchos")
    ),
    suppressMessages(get_n_obs(mica, species = toupper("ANAS platYrhyncHOS")))
  )
})

test_that(paste(
  "species accepts use of common names and return",
  "the same as using scientic name"
), {
  # define scientific name
  scn <- "Anas platyrhynchos"
  # define correspondent vernacular name
  vn <- "Mallard"

  # get number of observations for both cases
  output_anas_platyrhyncos <- suppressMessages(get_n_obs(mica, species = scn))
  output_mallard <- suppressMessages(get_n_obs(mica, species = vn))

  # same outputs
  expect_equal(output_anas_platyrhyncos, output_mallard)
})

test_that("if subset of species is specified, less observations are returned", {
  output_all_species <- get_n_obs(mica)
  output_anas_platyrhyncos <- suppressMessages(
    get_n_obs(mica,
      species = "Anas platyrhynchos"
    )
  )

  expect_true(sum(output_all_species$n) >= sum(output_anas_platyrhyncos$n))
})

test_that(paste(
  "species is NULL returns an equal or higher number of",
  "observations than species = 'all'"
), {
  output_all_species_collapsed <- get_n_obs(mica, species = NULL)
  output_anas_platyrhyncos <- get_n_obs(mica, species = "all") # default

  expect_true(
    sum(output_all_species_collapsed$n) >= sum(output_anas_platyrhyncos$n)
  )
})

test_that("get_n_obs returns a warning if 'all' is used with other values", {
  all_species <- get_species(mica)

  # use 'all' with other species
  expect_warning(get_n_obs(mica,
    species = c("all", all_species[1])
  ))
})

test_that(paste(
  "number of observations is equal to number of",
  "distinct sequenceID  values"
), {
  deploy_id <- "29b7d356-4bb4-4ec4-b792-2af5cc32efa8"
  species <- "Anas platyrhynchos"
  n_obs_via_sequence_id <-
    mica$data$observations %>%
    dplyr::filter(.data$deploymentID == deploy_id) %>%
    dplyr::filter(.data$scientificName == species) %>%
    dplyr::pull(.data$sequenceID) %>%
    dplyr::n_distinct()
  # one sequenceID  linked to two observations (different age, sex and count)
  n_obs <- suppressMessages(
  get_n_obs(
    filter_observations(mica, deploymentID == deploy_id),
    species = "Mallard"
  )
)
  expect_equal(n_obs$n, n_obs_via_sequence_id)
})

test_that("sex filters data correctly", {
  sex_value <- "female"
  n_obs_females <- suppressMessages(
    get_n_obs(mica, species = NULL, sex = sex_value)
  )
  tot_n_obs_females <- sum(n_obs_females$n)
  expect_equal(tot_n_obs_females, 1)
  expect_equal(nrow(n_obs_females), nrow(mica$data$deployments))
})

test_that("multiple sex values allowed", {
  sex_value <- c("female", "unknown")
  n_obs_females_unknown <- suppressMessages(get_n_obs(mica,
    species = NULL,
    sex = sex_value
  ))
  tot_n_obs_females_unknown <- sum(n_obs_females_unknown$n)
  expect_equal(
    tot_n_obs_females_unknown,
    mica$data$observations %>%
      dplyr::filter(.data$sex %in% sex_value) %>%
      dplyr::distinct(.data$sequenceID) %>%
      nrow()
  )
  expect_equal(nrow(n_obs_females_unknown), nrow(mica$data$deployments))
})

test_that("life_stage filters data correctly", {
  life_stage_value <- "subadult"
  n_obs_subadult_via_distinct <-
    mica$data$observations %>%
    dplyr::filter(.data$lifeStage %in% life_stage_value) %>%
    dplyr::distinct(.data$sequenceID) %>%
    nrow()
  n_obs_subadult <- suppressMessages(
    get_n_obs(mica, species = NULL, life_stage = life_stage_value)
  )
  tot_n_obs_subadult <- sum(n_obs_subadult$n)
  expect_equal(tot_n_obs_subadult, n_obs_subadult_via_distinct)
  expect_equal(nrow(n_obs_subadult), nrow(mica$data$deployments))
})

test_that("multiple age values allowed", {
  life_stage_value <- c("subadult", "adult")
  n_obs_subadult_adult <- suppressMessages(
    get_n_obs(mica, species = NULL, life_stage = life_stage_value)
  )
  tot_n_obs_subadult_adult <- sum(n_obs_subadult_adult$n)
  n_obs_subadult_adult_calculate <-
    mica$data$observations %>%
    dplyr::filter(.data$lifeStage %in% life_stage_value) %>%
    nrow()
  expect_equal(tot_n_obs_subadult_adult, n_obs_subadult_adult_calculate)
  expect_equal(nrow(n_obs_subadult_adult), nrow(mica$data$deployments))
})

test_that("error returned if life stage or sex is not present", {
  expect_error(get_n_obs(mica, life_stage = "bad"))
  expect_error(get_n_obs(mica, sex = "bad"))
})

test_that(paste(
  "scientific_name column contains all specified species,",
  "even if all 0s are returned"
), {
  species_value <- "Anas platyrhynchos"
  sex_value <- "female"
  n_obs <- suppressMessages(
    get_n_obs(mica, species = species_value, sex = sex_value)
  )
  expect_true(all(n_obs$scientificName %in% species_value))
  expect_true(all(species_value %in% n_obs$scientificName))
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_n_obs(datapkg = mica)
    ),
    "The `datapkg` argument of `get_n_obs()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})
