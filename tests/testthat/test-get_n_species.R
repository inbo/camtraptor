test_that("get_n_species returns the right dataframe", {
  output_get_n_species <- get_n_species(mica)

  # type list
  expect_type(output_get_n_species, "list")

  # class tibble data.frame
  expect_equal(
    class(output_get_n_species),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deploymentID and n only
  expect_equal(
    names(output_get_n_species),
    c(
      "deploymentID",
      "n"
    )
  )
})

test_that("get_n_species returns 0 for obs without recognized species", {
  # create data package with one deployment with 0 obs and one deployment with
  # observations of unknown species
  unknown_species <- mica
  unknown_species$data$observations <- 
    # deployments have detected only unknown species
    unknown_species$data$observations %>% 
    dplyr::filter(is.na(.data$scientificName))
  n_species <- get_n_species(package = unknown_species)
  n_deploys_in_obs <- length(unique(unknown_species$data$observations$deploymentID))
  expect_equal(
    n_species$n, 
    rep(0, n_deploys_in_obs)
  )
})

test_that("get_n_species returns NA for deployments without observations", {
  # create data package with one deployment with 0 obs and one deployment with
  # observations of unknown species
  no_obs <- mica
  obs <- no_obs$data$observations
  dep_no_obs <- "29b7d356"
  obs <- obs[obs$deploymentID != dep_no_obs,]
  no_obs$data$observations <- obs
  n_species <- suppressMessages(get_n_species(package = no_obs))
  expect_true(is.na(n_species[n_species$deploymentID == dep_no_obs,]$n))
})
