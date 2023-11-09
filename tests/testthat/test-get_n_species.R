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
  # create data package with one deployment with 0 obs and one delpoyment with
  # observations of unknown species
  unknown_species <- mica
  unknown_species$data$observations <- 
    unknown_species$data$observations %>% 
    # a deployment has detected only unknown species
    dplyr::filter(is.na(.data$scientificName) | 
             .data$scientificName != "Homo sapiens")
  n_species <- get_n_species(package = unknown_species)
  expect_equal(n_species[n_species$n == 0,]$n, 0)
})

test_that("get_n_species returns NA for deployments without observations", {
  # create data package with one deployment with 0 obs and one delpoyment with
  # observations of unknown species
  no_obs <- mica
  obs <- no_obs$data$observations
  dep_no_obs <- "29b7d356-4bb4-4ec4-b792-2af5cc32efa8"
  obs <- obs[obs$deploymentID != dep_no_obs,]
  no_obs$data$observations <- obs
  n_species <- suppressMessages(get_n_species(package = no_obs))
  expect_true(is.na(n_species[n_species$deploymentID == dep_no_obs,]$n))
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_n_species(datapkg = mica)
    ),
    "The `datapkg` argument of `get_n_species()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})
