test_that("get_n_individuals returns the right structure of dataframe", {

  # species arg specified
  output_anas_platyrhyncos <- get_n_individuals(mica,
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
  output_general <- get_n_individuals(mica, species = NULL)

  # columns deployment_id and n
  expect_equal(
    names(output_general),
    c(
      "deployment_id",
      "n"
    )
  )
})


test_that("get_n_individuals returns the right number of rows: all species selected", {
  all_species <- get_species(mica)
  all_deployments <- unique(mica$deployments$deployment_id)

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
  deployments <- unique(mica$deployments$deployment_id)

  n_deployments <- length(deployments)

  # calculate get_n_individuals for a species undetected in one deployment
  output_ondatra_zibethicus <- get_n_individuals(mica,
    species = "Ondatra zibethicus"
  )

  # number of rows should be equal to number of deployments
  expect_equal(nrow(output_ondatra_zibethicus), n_deployments)
})

test_that(
  "get_n_individuals returns rows ordered by the original order of deployments",
  {
    # get the original order of deployment IDs
    deployment_ids <- unique(mica$deployments$deployment_id)

    # apply function
    n_individuals <- get_n_individuals(mica)
    deployments_in_n_individuals <- unique(n_individuals$deployment_id)
    expect_equal(deployments_in_n_individuals, deployment_ids)
  }
)

test_that("species = 'all' returns the same of using a vector with all species", {
  all_species <- get_species(mica)
  all_deployments <- unique(mica$deployments$deployment_id)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of individuals for all species using default "all" value
  output_all_species_default <- get_n_individuals(mica, species = "all")
  # calculate number of individuals for all species specifying the species
  output_all_species <- get_n_individuals(mica,
    species = all_species$scientific_name
  )

  expect_equal(output_all_species, output_all_species_default)
})

test_that("species is case insensitive", {
  expect_equal(
    get_n_individuals(mica, species = "Anas platyrhynchos"),
    get_n_individuals(mica, species = toupper("Anas platyrhynchos"))
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
  output_anas_platyrhyncos <- get_n_individuals(mica, species = scn)
  output_mallard <- get_n_individuals(mica, species = vn)

  # same outputs
  expect_equal(output_anas_platyrhyncos, output_mallard)
})

test_that("if subset of species is specified, less individuals are returned", {
  output_all_species <- get_n_individuals(mica)
  output_anas_platyrhyncos <- get_n_individuals(mica,
    species = "Anas platyrhynchos"
  )

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

test_that(paste(
  "number of individuals is equal to sum of counts"
), {
  deploy_id <- "fff2f46e-8163-453c-9044-61fb77587f5d"
  species <- "Anas platyrhynchos"
  n_individuals_via_count <-
    mica$observations %>%
    filter(deployment_id == deploy_id) %>%
    filter(scientific_name == species) %>%
    pull(count) %>%
    sum()
  n_individuals <- get_n_individuals(mica,
    species = "Mallard",
    pred("deployment_id", deploy_id)
  )
  expect_equal(n_individuals$n, n_individuals_via_count)
})

test_that("sex filters data correctly", {
  deploy_id <- "fff2f46e-8163-453c-9044-61fb77587f5d"
  species <- "Anas platyrhynchos"
  sex_value <- "female"
  n_individuals_via_count <-
    mica$observations %>%
    filter(deployment_id == deploy_id) %>%
    filter(scientific_name == species) %>%
    filter(sex == sex_value) %>%
    pull(count) %>%
    sum()
  n_individuals_females <- get_n_individuals(mica, species = NULL, sex = sex_value)
  tot_n_individuals_females <- sum(n_individuals_females$n)
  expect_equal(tot_n_individuals_females, n_individuals_via_count)
  expect_equal(nrow(n_individuals_females), nrow(mica$deployments))
})

test_that("multiple sex values allowed", {
  sex_value <- c("female", "undefined")
  n_individuals_females_undefined_via_count <-
    mica$observations %>%
    filter(sex %in% sex_value) %>%
    pull(count) %>%
    sum()
  n_individuals_females_undefined <- get_n_individuals(mica,
    species = NULL,
    sex = sex_value
  )
  tot_n_individuals_females_undefined <- sum(n_individuals_females_undefined$n)
  expect_equal(
    tot_n_individuals_females_undefined,
    n_individuals_females_undefined_via_count
  )
  expect_equal(
    nrow(n_individuals_females_undefined),
    nrow(mica$deployments)
  )
})

test_that("age filters data correctly", {
  age_value <- "juvenile"
  n_individuals_juvenile_via_count <-
    mica$observations %>%
    filter(age == age_value) %>%
    pull(count) %>%
    sum()
  n_individuals_juvenile <- get_n_individuals(mica, species = NULL, age = age_value)
  tot_n_individuals_juvenile <- sum(n_individuals_juvenile$n)
  expect_equal(tot_n_individuals_juvenile, n_individuals_juvenile_via_count)
  expect_equal(nrow(n_individuals_juvenile), nrow(mica$deployments))
})

test_that("multiple age values allowed", {
  age_value <- c("juvenile", "adult")
  n_individuals_juvenile_adult_via_count <-
    mica$observations %>%
    filter(age %in% age_value) %>%
    pull(count) %>%
    sum()
  n_individuals_juvenile_adult <- get_n_individuals(mica,
    species = NULL,
    age = age_value
  )
  tot_n_individuals_juvenile_adult <- sum(n_individuals_juvenile_adult$n)
  expect_equal(
    tot_n_individuals_juvenile_adult,
    n_individuals_juvenile_adult_via_count
  )
  expect_equal(nrow(n_individuals_juvenile_adult), nrow(mica$deployments))
})

test_that("error returned if age or sex is not present", {
  expect_error(get_n_individuals(mica, age = "bad"))
  expect_error(get_n_individuals(mica, sex = "bad"))
})
