test_that("summarize_observations() returns error for invalid datapackage", {
  skip_if_offline()
  x <- example_dataset()
  x$data <- NULL
  # Expect error: control of specific error message left to `read_camtrapdp()`
  expect_error(summarize_observations(x))
})

test_that("summarize_observations() returns error for invalid group_by", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    summarize_observations(x, group_by = "invalid"),
    paste0("Invalid value for group_by parameter: invalid.\n",
           "Valid inputs are: deploymentID, locationID, locationName, ",
           "deploymentTags, scientificName, lifeStage, sex and behavior"
    )
  )
})

test_that("summarize_observations() returns error for invalid group_time_by", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    summarize_observations(x, group_time_by = "invalid"),
    paste0("Invalid value for group_time_by parameter: invalid.\n",
           "Valid inputs are: day, week, month, year and NULL"
    )
  )
})

testthat::test_that(
  paste0(
    "summarize_observations() returns correct summary for grouping by ",
    "deploymentID and scientificName (default)"), {
      skip_if_offline()
      x <- example_dataset()  
      summary <- summarize_observations(x)
      # Check that the `summary` has the expected columns
      expect_equal(
        c("deploymentID", "scientificName", "n_observations", "sum_count"),
        names(summary)
      )
      # All deployments are present
      expect_true(all(purrr::pluck(deployments(x), "deploymentID") %in%
                        summary$deploymentID)
      )
      # All scientific names are present
      x <- x %>% filter_observations(.data$observationLevel == "event")
      expect_true(all(
        unique(purrr::pluck(observations(x), "scientificName")) %in%
          summary$scientificName
      ))
      # Correct type of columns
      expect_true(is.character(summary$deploymentID))
      expect_true(is.character(summary$scientificName))
      expect_true(is.integer(summary$n_observations))
      expect_true(is.integer(summary$sum_count))
    })

testthat::test_that(
  "summarize_observations() takes into account only event-based observations", {
    skip_if_offline()
    x <- example_dataset()
    summary <- summarize_observations(x)
    x_event_obs <- x %>%
      filter_observations(.data$observationLevel == "event")
    summary_event_obs <- summarize_observations(
      x_event_obs
    )
    expect_identical(
      summary,
      summary_event_obs
    )
})

test_that(
  paste0("summarize_observations() returns correct summary for grouping by ",
         "deploymentID, i.e. no grouping by observations columns"), {
    skip_if_offline()
    x <- example_dataset()
    summary <- summarize_observations(x, group_by = "deploymentID")
    
    # The returned summary is of type list
    expect_type(summary, "list")
    # The returned summary is a tibble data.frame
    expect_equal(
      class(summary),
      c("tbl_df", "tbl", "data.frame")
    )
    # Check that the `summary` has the expected columns
    expect_equal(c("deploymentID", "n_observations", "sum_count"), names(summary))
    
    # Check that `deploymentID` is a character
    expect_true(is.character(summary$deploymentID))
    # Check that `n_observations` is a number
    expect_true(is.numeric(summary$n_observations))
    # Check that `n_observations` is a number
    expect_true(is.numeric(summary$n_observations))
    
    # Check that the number of returned deployments matches the number of
    # deployments in the dataset
    expect_equal(nrow(summary), nrow(deployments(x)))
    
    # Right `deploymentID` values
    expect_equal(
      summary$deploymentID,
      purrr::pluck(deployments(x),"deploymentID")
    )

    # Number of observations returned is correct
    n_observations <- example_dataset() %>%
      filter_observations(.data$observationLevel == "event") %>%
      observations() %>%
      dplyr::group_by(deploymentID) %>%
      dplyr::summarise(n_obs = dplyr::n_distinct(.data$observationID)) %>%
      dplyr::ungroup() %>%
      dplyr::pull(n_obs)
    expect_identical(summary$n_observations, n_observations)
    # Sum of individual counts returned is correct
    sum_individual_counts <- x %>%
      filter_observations(.data$observationLevel == "event") %>%
      observations() %>%
      dplyr::group_by(deploymentID) %>%
      dplyr::summarise(sum_count = as.integer(
        sum(.data$count, na.rm = TRUE))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(sum_count)
    expect_identical(summary$sum_count, sum_individual_counts)
})

testthat::test_that(
  "Deployments without observations are not included in the summary", {
    skip_if_offline()
    x <- example_dataset()
    o <- observations(x)
    o_with_no_obs <- o %>% dplyr::filter(deploymentID == "00a2c20d")
    x_with_obs_one_deploy <- x
    observations(x_with_obs_one_deploy) <- o_with_no_obs
    summary_one_deploy <- summarize_observations(
      x_with_obs_one_deploy,
      group_by = "deploymentID"
    )
    expect_equal(nrow(summary_one_deploy), 1)
    expect_identical(summary_one_deploy$deploymentID, "00a2c20d")
    expect_identical(summary_one_deploy$n_observations, 14L)
    expect_identical(summary_one_deploy$sum_count, 26L)
})


testthat::test_that(
  paste0(
    "summarize_observations() returns correct summary for grouping by ",
    "lifeStage, i.e. no grouping by deployments columns"), {
    skip_if_offline()
    x <- example_dataset()
    summary <- summarize_observations(x, group_by = "lifeStage")
    # Check that the `summary` has the expected columns
    expect_equal(
      c("lifeStage", "n_observations", "sum_count"),
      names(summary)
    )
  # No deployments info is present
  expect_true(!"deploymentID" %in% names(summary))
  # All life stages are present
  x <- x %>% filter_observations(.data$observationLevel == "event")
  expect_true(all(
    unique(purrr::pluck(observations(x), "lifeStage")) %in%
      summary$lifeStage
  ))
  
  # Correct type of columns
  expect_true(is.factor(summary$lifeStage))
  expect_true(is.integer(summary$n_observations))
  expect_true(is.integer(summary$sum_count))
  
  # Total number of observations is preserved
  expect_true(
    sum(summary$n_observations) ==
      nrow(observations(x))
  )
  # Total number of individuals is preserved
  expect_true(
    sum(summary$sum_count) ==
      sum(purrr::pluck(observations(x), "count"), na.rm = TRUE)
  )
})

testthat::test_that(
  "summarize_observations() returns correct summary when grouping by time", {
    skip_if_offline()
    x <- example_dataset()
    summary <- summarize_observations(x, group_time_by = "day")
    
    # Check that the `summary` has the expected columns
    expect_equal(
      c("deploymentID", "scientificName", "day", "n_observations", "sum_count"),
      names(summary)
    )
    
    # All dates are present
    x_events <- x %>% filter_observations(.data$observationLevel == "event")
    expect_true(all(
      unique(
        lubridate::as_datetime(
          lubridate::as_date(purrr::pluck(observations(x_events), "eventStart"))
        ) %in%
        summary$day
      )
    ))
    
    # Correct type of columns
    expect_true(is.character(summary$deploymentID))
    expect_true(is.character(summary$scientificName))
    expect_true(lubridate::is.timepoint(summary$day))
    expect_true(is.integer(summary$n_observations))
    expect_true(is.integer(summary$sum_count))
    
    # Total number of observations is preserved
    expect_true(
      sum(summary$n_observations) ==
        nrow(observations(x_events))
    )
    
    # Total number of individuals is preserved
    expect_true(
      sum(summary$sum_count) ==
        sum(purrr::pluck(observations(x_events), "count"), na.rm = TRUE)
    )
})