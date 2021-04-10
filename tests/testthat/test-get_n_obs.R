test_that("get_n_obs returns the right structure of dataframe", {

  # species arg specified
  output_anas_platyrhyncos <- get_n_obs(camtrapdp, "Anas platyrhynchos")

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

  # species arg is not specified
  output_general <- get_n_obs(camtrapdp)

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

  # calculate nnumber of observations for all species
  output_all_species <- get_n_obs(camtrapdp,
                                  species = all_species$scientific_name)

  # number of rows should be equal to number of species by number of deployments
  expect_equal(nrow(output_all_species),
               n_all_species * n_all_deployments)
})

test_that("get_n_obs returns always the right number of rows", {

  deployments <- unique(camtrapdp$deployments$deployment_id)

  n_deployments <- length(deployments)

  # calculate get_n_obs for a species undetected in one deployment
  output_ondatra_zibethicus <- get_n_obs(camtrapdp, "Ondatra zibethicus")

  # number of rows should be equal to number of deployments
  expect_equal(nrow(output_ondatra_zibethicus), n_deployments)

})


test_that("species is case insensitive", {

  expect_equal(get_n_obs(camtrapdp, "Anas platyrhynchos"),
               get_n_obs(camtrapdp, toupper("Anas platyrhynchos")))
})

test_that("if species are specified, less observations are returned", {

  output_all_species <- get_n_obs(camtrapdp)
  output_anas_platyrhyncos <- get_n_obs(camtrapdp, "Anas platyrhynchos")

  expect_true(sum(output_all_species$n) >=  sum(output_anas_platyrhyncos$n))

})
