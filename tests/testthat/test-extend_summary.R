test_that("extend_summary() checks inputs", {
  x <- camtrapdp::example_dataset()
  s <- summarize_observations(x)
  
  # check that an error is returned if x is not a camtrapdp object
  expect_error(
    extend_summary(s, "not_a_camtrapdp_object")
  )
  
  # check that an error is returned if grouped_df is not a valid summary object
  expect_error(
    extend_summary(data.frame(a = 1), x),
    "The summary must be a grouped tibble data frame."
  )
})

test_that("extend_summary() works as expected with different groupings", {
  # create a camtrap DP object
  x <- camtrapdp::example_dataset()

  # Filter observations for a specific species
  x_filtered_species <- x %>%
    filter_observations(scientificName == "Anas strepera")

  # Create a summary of observations on the filtered dataset
  s <- summarize_observations(
    x_filtered_species, # Use the filtered object here
    group_by = c("deploymentID", "scientificName")
  )
  # Extend summary using the *original* full dataset x,
  # so it can extend with all possible species/deployments
  s_extended <- extend_summary(s, x)

  # The extended summary is a valid summary
  expect_no_error(
    check_summary(s_extended)
  )
  
  # The extended summary has the same columns as the original summary
  expect_equal(
    colnames(s_extended),
    colnames(s)
  )
  
  # All species are present in the extended summary
  all_species_in_x <- unique(
    observations(x) %>% purrr::pluck("scientificName")
  )
  expect_equal(
    sort(unique(s_extended$scientificName)),
    sort(all_species_in_x)
  )
  
  # All deployments are present in the extended summary
  all_deployments_in_x <- unique(
    deployments(x) %>% purrr::pluck("deploymentID")
  )
  expect_equal(
    sort(unique(s_extended$deploymentID)),
    sort(all_deployments_in_x)
  )

  # Check that the number of rows is correct
  expect_equal(
    nrow(s_extended),
    length(all_species_in_x) * length(all_deployments_in_x)
  )

  # Check that `n_scientificName` is NA for newly added rows
  expect_true(all(
    is.na(
      s_extended[
        s_extended$scientificName != "Anas strepera",
        "n_scientificName"
      ]
    )
  ))
  
  # Check that `n_events` is 0 (integer) for newly added rows
  expect_identical(
    s_extended %>%
      dplyr::filter(scientificName != "Anas strepera") %>%
      dplyr::pull("n_events") %>%
      unique(),
    0L
  )
  
  # Check that `n_observations` is 0 (integer) for newly added rows
  expect_identical(
    s_extended %>%
      dplyr::filter(scientificName != "Anas strepera") %>%
      dplyr::pull("n_observations") %>%
      unique(),
    0L
  )
  
  # Check that `sum_count` is 0 (integer) for newly added rows
  expect_identical(
    s_extended %>%
      dplyr::filter(scientificName != "Anas strepera") %>%
      dplyr::pull("n_observations") %>%
      unique(),
    0L
  )
  
  # Check that `rai_observations` is 0 (double) for newly added rows
  expect_identical(
    s_extended %>%
      dplyr::filter(scientificName != "Anas strepera") %>%
      dplyr::pull("rai_observations") %>%
      unique(),
    0
  )

  # Check that `rai_count` is 0 (double) for newly added rows
  expect_identical(
    s_extended %>%
      dplyr::filter(scientificName != "Anas strepera") %>%
      dplyr::pull("rai_count") %>%
      unique(),
    0
  )
  
  expect_true(all(
    s_extended[
      s_extended$scientificName == "Anas strepera" &
      s_extended$deploymentID == "https://example.com/deployments/dep3",
      "n_observations"
    ] == 0
  ))

  # Check that sum_count is 0 for newly added rows
  expect_identical(
    s_extended %>%
      dplyr::filter(scientificName != "Anas strepera") %>%
      dplyr::pull("sum_count") %>%
      unique(),
    0L
  )
  expect_true(all(
    s_extended[
      s_extended$scientificName == "Anas strepera" &
      s_extended$deploymentID == "https://example.com/deployments/dep3",
      "sum_count"
    ] == 0
  ))
})

test_that("extend_summary() preserves grouping", {
  x <- camtrapdp::example_dataset()

  s <- summarize_observations(
    x,
    group_by = c("deploymentID", "scientificName")
  )
  
  s_extended <- extend_summary(s, x)
  expect_equal(dplyr::group_vars(s), dplyr::group_vars(s_extended))
})
