test_that("get_n_obs returns the right structure of dataframe", {

  # species arg specified
  output_anas_platyrhyncos <- get_n_obs(camtrapdp,
                                        species = "Anas platyrhynchos")

  # type list
  expect_type(output_anas_platyrhyncos, "list")

  # class tibble data.frame
  expect_equal(class(output_anas_platyrhyncos),
               c("tbl_df", "tbl", "data.frame"))

  # columns deployment_id, scientific_name and n
  expect_equal(names(output_anas_platyrhyncos),
               c("deployment_id",
                 "scientific_name",
                 "n"))

  # species arg is NULL
  output_general <- get_n_obs(camtrapdp, species = NULL)

  # columns deployment_id and n
  expect_equal(names(output_general),
               c("deployment_id",
                 "n"))

})


test_that("get_n_obs returns the right number of rows: all species selected", {

  all_species <- get_species(camtrapdp)
  all_deployments <- unique(camtrapdp$deployments$deployment_id)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of observations for all species
  output_all_species <- get_n_obs(camtrapdp)

  # number of rows should be equal to number of species by number of deployments
  expect_equal(nrow(output_all_species),
               n_all_species * n_all_deployments)
})

test_that("get_n_obs returns always the right number of rows", {

  deployments <- unique(camtrapdp$deployments$deployment_id)

  n_deployments <- length(deployments)

  # calculate get_n_obs for a species undetected in one deployment
  output_ondatra_zibethicus <- get_n_obs(camtrapdp,
                                         species = "Ondatra zibethicus")

  # number of rows should be equal to number of deployments
  expect_equal(nrow(output_ondatra_zibethicus), n_deployments)

})

test_that("species = 'all' returns the same of using a vector with all species", {

  all_species <- get_species(camtrapdp)
  all_deployments <- unique(camtrapdp$deployments$deployment_id)

  n_all_species <- nrow(all_species)
  n_all_deployments <- length(all_deployments)

  # calculate number of observations for all species using default "all" value
  output_all_species_default <- get_n_obs(camtrapdp, species = "all")
  # calculate number of observations for all species specifying the species
  output_all_species <- get_n_obs(camtrapdp,
                                  species = all_species$scientific_name)

  expect_equal(output_all_species, output_all_species_default)

})

test_that("species is case insensitive", {

  expect_equal(get_n_obs(camtrapdp, species = "Anas platyrhynchos"),
               get_n_obs(camtrapdp, species = toupper("Anas platyrhynchos")))
})

test_that("species accepts use of common names and return the same as using scientic name", {

  # define scientific name
  scn <- "Anas platyrhynchos"
  # define correspondent vernacular name
  vn <- "Mallard"

  # get number of observations for both cases
  output_anas_platyrhyncos <- get_n_obs(camtrapdp,species = scn)
  output_mallard <- get_n_obs(camtrapdp, species = vn)

  # same outputs
  expect_equal(output_anas_platyrhyncos, output_mallard)

})

test_that("if subset of species is specified, less observations are returned", {

  output_all_species <- get_n_obs(camtrapdp)
  output_anas_platyrhyncos <- get_n_obs(camtrapdp,
                                        species = "Anas platyrhynchos")

  expect_true(sum(output_all_species$n) >=  sum(output_anas_platyrhyncos$n))

})

test_that(paste("species is NULL returns an equal or higher number of",
                "observations than species = 'all'"), {

  output_all_species_collapsed <- get_n_obs(camtrapdp, species = NULL)
  output_anas_platyrhyncos <- get_n_obs(camtrapdp, species = "all") # default

  expect_true(
    sum(output_all_species_collapsed$n) >=  sum(output_anas_platyrhyncos$n)
  )

})

test_that("get_n_obs returns a warning if 'all' is used with other values", {

  all_species <- get_species(camtrapdp)

  # use 'all' with other species
  expect_warning(get_n_obs(camtrapdp,
                         species = c("all", all_species[1])))

})
