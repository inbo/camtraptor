test_that("get_n_individuals returns the right structure of dataframe", {
  # species arg specified
  output_anas_platyrhyncos <- suppressMessages(get_n_individuals(mica,
    species = "Anas platyrhynchos"
  ))

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
  output_general <- get_n_individuals(mica, species = NULL)

  # columns deploymentID and n
  expect_equal(
    names(output_general),
    c(
      "deploymentID",
      "n"
    )
  )
})


test_that("get_n_individuals returns the right number of rows: all species selected", {
  all_species <- get_species(mica)
  all_deployments <- unique(mica$data$deployments$deploymentID)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of individuals for all species
  output_all_species <- get_n_individuals(mica)

  # number of rows should be equal to number of species by number of deployments
  expect_equal(
    nrow(output_all_species),
    n_all_species * n_all_deployments
  )
})

test_that(paste(
  "get_n_individuals returns always the right number of rows:",
  "species undetected in one deployment"
), {
  deployments <- unique(mica$data$deployments$deploymentID)

  n_deployments <- length(deployments)

  # calculate get_n_individuals for a species undetected in one deployment
  output_martes_foina <- suppressMessages(get_n_individuals(mica,
    species = "Martes foina"
  ))

  # number of rows should be equal to number of deployments
  expect_equal(nrow(output_martes_foina), n_deployments)
})

test_that(
  "get_n_individuals returns rows ordered by the original order of deployments",
  {
    # get the original order of deployment IDs
    deploymentIDs <- unique(mica$data$deployments$deploymentID)

    # apply function
    n_individuals <- get_n_individuals(mica)
    deployments_in_n_individuals <- unique(n_individuals$deploymentID)
    expect_equal(deployments_in_n_individuals, deploymentIDs)
  }
)

test_that("species = 'all' returns the same of using a vector with all species", {
  all_species <- get_species(mica)
  all_deployments <- unique(mica$data$deployments$deploymentID)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of individuals for all species using default "all" value
  output_all_species_default <- get_n_individuals(mica, species = "all")
  # calculate number of individuals for all species specifying the species
  output_all_species <- get_n_individuals(mica,
    species = all_species$scientificName
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  expect_equal(
    suppressMessages(get_n_individuals(mica, species = "Anas platyrhynchos")),
    suppressMessages(get_n_individuals(mica, species = toupper("Anas platyrhynchos")))
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

  # get number of individuals for both cases
  output_anas_platyrhyncos <- suppressMessages(get_n_individuals(mica, species = scn))
  output_mallard <- suppressMessages(get_n_individuals(mica, species = vn))

  # same outputs
  expect_equal(output_anas_platyrhyncos, output_mallard)
})

test_that("if subset of species is specified, less individuals are returned", {
  output_all_species <- get_n_individuals(mica)
  output_anas_platyrhyncos <- suppressMessages(get_n_individuals(mica,
    species = "Anas platyrhynchos"
  ))

  expect_true(sum(output_all_species$n) >= sum(output_anas_platyrhyncos$n))
})

test_that(paste(
  "species is NULL returns an equal or higher number of",
  "individuals than species = 'all'"
), {
  output_all_species_collapsed <- get_n_individuals(mica, species = NULL)
  output_anas_platyrhyncos <- get_n_individuals(mica, species = "all") # default

  expect_true(
    sum(output_all_species_collapsed$n) >= sum(output_anas_platyrhyncos$n)
  )
})

test_that("get_n_individuals returns a warning if 'all' is used with other values", {
  all_species <- get_species(mica)

  # use 'all' with other species
  expect_warning(get_n_individuals(mica,
    species = c("all", all_species[1])
  ))
})

test_that("number of individuals is equal to sum of counts", {
  deploy_id <- "29b7d356"
  species <- "Anas platyrhynchos"
  n_individuals_via_count <-
    mica$data$observations %>%
    dplyr::filter(deploymentID == deploy_id) %>%
    dplyr::filter(scientificName == species) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals <- suppressMessages(get_n_individuals(mica,
    species = "Mallard",
    pred("deploymentID", deploy_id)
  ))
  expect_equal(n_individuals$n, n_individuals_via_count)
})

test_that("sex filters data correctly", {
  sex_value <- "female"
  n_individuals_female_via_count <-
    mica$data$observations %>%
    dplyr::filter(sex == sex_value) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals_females <-
    suppressMessages(get_n_individuals(mica, species = NULL, sex = sex_value))
  tot_n_individuals_females <- sum(n_individuals_females$n)
  expect_equal(tot_n_individuals_females, n_individuals_female_via_count)
  expect_equal(nrow(n_individuals_females), nrow(mica$data$deployments))
})

test_that("multiple sex values allowed", {
  sex_value <- c("female", NULL)
  n_individuals_females_undefined_via_count <-
    mica$data$observations %>%
    dplyr::filter(sex %in% sex_value) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals_females_undefined <- suppressMessages(get_n_individuals(mica,
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
    nrow(mica$data$deployments)
  )
})

test_that("life stage filters data correctly", {
  life_stage_value <- "subadult"
  n_individuals_juvenile_via_count <-
    mica$data$observations %>%
    dplyr::filter(lifeStage == life_stage_value) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals_juvenile <- suppressMessages(
    get_n_individuals(mica, species = NULL, life_stage = life_stage_value)
  )
  tot_n_individuals_juvenile <- sum(n_individuals_juvenile$n)
  expect_equal(tot_n_individuals_juvenile, n_individuals_juvenile_via_count)
  expect_equal(nrow(n_individuals_juvenile), nrow(mica$data$deployments))
})

test_that("multiple age values allowed", {
  life_stage_value <- c("subadult", "adult")
  n_individuals_juvenile_adult_via_count <-
    mica$data$observations %>%
    dplyr::filter(lifeStage %in% life_stage_value) %>%
    dplyr::pull(count) %>%
    sum()
  n_individuals_juvenile_adult <- suppressMessages(
    get_n_individuals(mica,
      species = NULL,
      life_stage = life_stage_value
    )
  )
  tot_n_individuals_juvenile_adult <- sum(n_individuals_juvenile_adult$n)
  expect_equal(
    tot_n_individuals_juvenile_adult,
    n_individuals_juvenile_adult_via_count
  )
  expect_equal(nrow(n_individuals_juvenile_adult), nrow(mica$data$deployments))
})

test_that("error returned if life stage or sex is not present", {
  expect_error(get_n_individuals(mica, life_stage = "bad"))
  expect_error(get_n_individuals(mica, sex = "bad"))
})
