test_that("get_n_obs returns the right structure of dataframe", {

  # species arg specified
  output_anas_platyrhyncos <- get_n_obs(camtrapdp,
    species = "Anas platyrhynchos"
  )

  # type list
  expect_type(output_anas_platyrhyncos, "list")

  # class tibble data.frame
  expect_equal(
    class(output_anas_platyrhyncos),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deployment_id, scientific_name and n
  expect_equal(
    names(output_anas_platyrhyncos),
    c(
      "deployment_id",
      "scientific_name",
      "n"
    )
  )

  # species arg is NULL
  output_general <- get_n_obs(camtrapdp, species = NULL)

  # columns deployment_id and n
  expect_equal(
    names(output_general),
    c(
      "deployment_id",
      "n"
    )
  )
})


test_that("get_n_obs returns the right number of rows: all species selected", {
  all_species <- get_species(camtrapdp)
  all_deployments <- unique(camtrapdp$deployments$deployment_id)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of observations for all species
  output_all_species <- get_n_obs(camtrapdp)

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
  deployments <- unique(camtrapdp$deployments$deployment_id)

  n_deployments <- length(deployments)

  # calculate get_n_obs for a species undetected in one deployment
  output_ondatra_zibethicus <- get_n_obs(camtrapdp,
    species = "Ondatra zibethicus"
  )

  # number of rows should be equal to number of deployments
  expect_equal(nrow(output_ondatra_zibethicus), n_deployments)
})

test_that(
  "get_n_obs returns rows ordered by the original order of deployments",
  {
    # get the original order of deployment IDs
    deployment_ids <- unique(camtrapdp$deployments$deployment_id)

    # apply function
    n_obs <- get_n_obs(camtrapdp)
    deployments_in_n_obs <- unique(n_obs$deployment_id)
    expect_equal(deployments_in_n_obs, deployment_ids)
  }
)

test_that("species = 'all' returns the same of using a vector with all species", {
  all_species <- get_species(camtrapdp)
  all_deployments <- unique(camtrapdp$deployments$deployment_id)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of observations for all species using default "all" value
  output_all_species_default <- get_n_obs(camtrapdp, species = "all")
  # calculate number of observations for all species specifying the species
  output_all_species <- get_n_obs(camtrapdp,
    species = all_species$scientific_name
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  expect_equal(
    get_n_obs(camtrapdp, species = "Anas platyrhynchos"),
    get_n_obs(camtrapdp, species = toupper("ANAS platYrhyncHOS"))
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
  output_anas_platyrhyncos <- get_n_obs(camtrapdp, species = scn)
  output_mallard <- get_n_obs(camtrapdp, species = vn)

  # same outputs
  expect_equal(output_anas_platyrhyncos, output_mallard)
})

test_that("if subset of species is specified, less observations are returned", {
  output_all_species <- get_n_obs(camtrapdp)
  output_anas_platyrhyncos <- get_n_obs(camtrapdp,
    species = "Anas platyrhynchos"
  )

  expect_true(sum(output_all_species$n) >= sum(output_anas_platyrhyncos$n))
})

test_that(paste(
  "species is NULL returns an equal or higher number of",
  "observations than species = 'all'"
), {
  output_all_species_collapsed <- get_n_obs(camtrapdp, species = NULL)
  output_anas_platyrhyncos <- get_n_obs(camtrapdp, species = "all") # default

  expect_true(
    sum(output_all_species_collapsed$n) >= sum(output_anas_platyrhyncos$n)
  )
})

test_that("get_n_obs returns a warning if 'all' is used with other values", {
  all_species <- get_species(camtrapdp)

  # use 'all' with other species
  expect_warning(get_n_obs(camtrapdp,
    species = c("all", all_species[1])
  ))
})

test_that(paste(
  "number of observations is equal to number of",
  "distinct sequence_id values"
), {
  deploy_id <- "fff2f46e-8163-453c-9044-61fb77587f5d"
  species <- "Anas platyrhynchos"
  n_obs_via_sequence_id <-
    camtrapdp$observations %>%
    filter(deployment_id == deploy_id) %>%
    filter(scientific_name == species) %>%
    pull(sequence_id) %>%
    n_distinct()
  # one sequence_id linked to two observations (different age, sex and count)
  n_obs <- get_n_obs(camtrapdp,
    species = "Mallard",
    pred("deployment_id", deploy_id)
  )
  expect_equal(n_obs$n, n_obs_via_sequence_id)
})

test_that("sex filters data correctly", {
  sex_value <- "female"
  n_obs_females <- get_n_obs(camtrapdp, species = NULL, sex = sex_value)
  tot_n_obs_females <- sum(n_obs_females$n)
  expect_equal(tot_n_obs_females, 1)
  expect_equal(nrow(n_obs_females), nrow(camtrapdp$deployments))
})

test_that("multiple sex values allowed", {
  sex_value <- c("female", "undefined")
  n_obs_females_undefined <- get_n_obs(camtrapdp,
    species = NULL,
    sex = sex_value
  )
  tot_n_obs_females_undefined <- sum(n_obs_females_undefined$n)
  expect_equal(
    tot_n_obs_females_undefined,
    camtrapdp$observations %>%
      filter(sex %in% sex_value) %>%
      distinct(sequence_id) %>%
      nrow()
  )
  expect_equal(nrow(n_obs_females_undefined), nrow(camtrapdp$deployments))
})

test_that("age filters data correctly", {
  age_value <- "juvenile"
  n_obs_juvenile_via_distinct <-
    camtrapdp$observations %>%
    filter(age %in% age_value) %>%
    distinct(sequence_id) %>%
    nrow()
  n_obs_juvenile <- get_n_obs(camtrapdp, species = NULL, age = age_value)
  tot_n_obs_juvenile <- sum(n_obs_juvenile$n)
  expect_equal(tot_n_obs_juvenile, n_obs_juvenile_via_distinct)
  expect_equal(nrow(n_obs_juvenile), nrow(camtrapdp$deployments))
})

test_that("multiple age values allowed", {
  age_value <- c("juvenile", "adult")
  n_obs_juvenile_adult <- get_n_obs(camtrapdp, species = NULL, age = age_value)
  tot_n_obs_juvenile_adult <- sum(n_obs_juvenile_adult$n)
  expect_equal(tot_n_obs_juvenile_adult, 252)
  expect_equal(nrow(n_obs_juvenile_adult), nrow(camtrapdp$deployments))
})

test_that("error returned if age or sex is not present", {
  expect_error(get_n_obs(camtrapdp, age = "bad"))
  expect_error(get_n_obs(camtrapdp, sex = "bad"))
})
