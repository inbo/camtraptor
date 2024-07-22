test_that("n_species returns the right dataframe", {
  skip_if_offline()
  x <- example_dataset()
  output_get_n_species <- n_species(x)

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

test_that("n_species returns 0 for obs without recognized species", {
  skip_if_offline()
  x <- example_dataset()
  # create data package with one deployment with 0 obs and one delpoyment with
  # observations of unknown species
  unknown_species <- x
  unknown_species$data$observations <- 
    observations(unknown_species) %>% 
    # a deployment has detected only unknown species
    dplyr::filter(is.na(.data$scientificName) | 
             .data$scientificName != "Homo sapiens")
  n_species <- n_species(package = unknown_species)
  expect_equal(n_species[n_species$n == 0,]$n, 0)
})

test_that("n_species returns NA for deployments without observations", {
  skip_if_offline()
  x <- example_dataset()
  # create data package with one deployment with 0 obs and one delpoyment with
  # observations of unknown species
  no_obs <- x
  obs <- observations(no_obs)
  dep_no_obs <- "29b7d356-4bb4-4ec4-b792-2af5cc32efa8"
  obs <- obs[obs$deploymentID != dep_no_obs,]
  no_obs$data$observations <- obs
  n_species <- suppressMessages(n_species(package = no_obs))
  expect_true(is.na(n_species[n_species$deploymentID == dep_no_obs,]$n))
})

test_that("Argument datapkg is deprecated: warning returned", {
  skip_if_offline()
  x <- example_dataset()
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      n_species(datapkg = x)
    ),
    "The `datapkg` argument of `n_species()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})
