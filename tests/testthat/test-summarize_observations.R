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
        names(summary),
        c("deploymentID",
          "scientificName",
          "n_scientificName",
          "n_events",
          "n_observations",
          "sum_count",
          "rai_observations",
          "rai_count")
      )
      # All deployments are present
      expect_true(all(dplyr::pull(deployments(x), "deploymentID") %in%
                        summary$deploymentID)
      )
      # All scientific names are present
      x <- x %>% filter_observations(.data$observationLevel == "event")
      expect_true(all(
        unique(dplyr::pull(observations(x), "scientificName")) %in%
          summary$scientificName
      ))
      # Correct type of columns
      expect_true(is.character(summary$deploymentID))
      expect_true(is.character(summary$scientificName))
      expect_true(is.integer(summary$n_scientificName))
      expect_true(is.integer(summary$n_events))
      expect_true(is.integer(summary$n_observations))
      expect_true(is.integer(summary$sum_count))
      expect_true(is.numeric(summary$rai_observations))
      expect_true(is.numeric(summary$rai_count))
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
         "deploymentID, i.e. no grouping by observations columns"
  ), {
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
    expect_equal(names(summary),
                 c("deploymentID",
                   "n_scientificName",
                   "n_events",
                   "n_observations",
                   "sum_count",
                   "rai_observations",
                   "rai_count")
    )
    
    # Check that `deploymentID` is a character
    expect_true(is.character(summary$deploymentID))
    # Check that `n_scientificName` is an integer
    expect_true(is.integer(summary$n_scientificName))
    # Check that `n_events` is an integer
    expect_true(is.integer(summary$n_events))
    # Check that `n_observations` is an integer
    expect_true(is.integer(summary$n_observations))
    # Check that `sum_count` is an integer
    expect_true(is.integer(summary$sum_count))
    # Check that `rai_observations` is a number
    expect_true(is.numeric(summary$rai_observations))
    # Check that `rai_count` is a number
    expect_true(is.numeric(summary$rai_count))
    
    # Check that the number of returned deployments matches the number of
    # deployments in the dataset
    expect_equal(nrow(summary), nrow(deployments(x)))
    
    # Right `deploymentID` values
    expect_equal(
      summary$deploymentID,
      dplyr::pull(deployments(x),"deploymentID")
    )
    
    # Number of scientific names returned is correct
    n_species_df <- x %>%
      filter_observations(.data$observationLevel == "event") %>%
      observations() %>%
      dplyr::group_by(deploymentID) %>%
      dplyr::summarise(
        n_scientificName = dplyr::n_distinct(.data$scientificName, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    expect_identical(summary$n_scientificName,
                     n_species_df$n_scientificName
    )
    # Number of events returned is correct
    n_events_df <- x %>%
      filter_observations(.data$observationLevel == "event") %>%
      observations() %>%
      dplyr::group_by(deploymentID) %>%
      dplyr::summarise(n_events = dplyr::n_distinct(.data$eventID)) %>%
      dplyr::ungroup()
    expect_identical(summary$n_events, n_events_df$n_events)
    # Number of observations returned is correct
    n_obs_df <- x %>%
      filter_observations(.data$observationLevel == "event") %>%
      observations() %>%
      dplyr::group_by(deploymentID) %>%
      dplyr::summarise(n_obs = dplyr::n_distinct(.data$observationID)) %>%
      dplyr::ungroup()
    expect_identical(summary$n_observations, n_obs_df$n_obs)
    # Sum of individual counts returned is correct
    sum_individual_counts_df <- x %>%
      filter_observations(.data$observationLevel == "event") %>%
      observations() %>%
      dplyr::group_by(deploymentID) %>%
      dplyr::summarise(sum_count = as.integer(
        sum(.data$count, na.rm = TRUE))
      ) %>%
      dplyr::ungroup()
    expect_identical(summary$sum_count,
                     sum_individual_counts_df$sum_count
    )
    # rai_observations returned is correct
    rai_observations_df <- n_obs_df %>%
      dplyr::left_join(
        x %>%
          filter_observations(.data$observationLevel == "event") %>%
          deployments() %>%
          dplyr::mutate(effort_duration = lubridate::as.duration(
            .data$deploymentEnd - .data$deploymentStart
          )) %>%
          dplyr::select("deploymentID", "effort_duration"),
        by = "deploymentID"
      ) %>%
      dplyr::mutate(
        rai_observations = 
          100 * .data$n_obs / (.data$effort_duration/lubridate::ddays(1))
      )
    expect_identical(summary$rai_observations,
                     rai_observations_df$rai_observations)
    # rai_count returned is correct
    rai_sum_count_df <- sum_individual_counts_df %>%
      dplyr::left_join(
        x %>%
          filter_observations(.data$observationLevel == "event") %>%
          deployments() %>%
          dplyr::mutate(effort_duration = lubridate::as.duration(
            .data$deploymentEnd - .data$deploymentStart
          )) %>%
          dplyr::select("deploymentID", "effort_duration"),
        by = "deploymentID"
      ) %>%
      dplyr::mutate(
        rai_count = 
          100 * .data$sum_count / 
          (.data$effort_duration / lubridate::ddays(1))
      )
    expect_identical(summary$rai_count,
                     rai_sum_count_df$rai_count)
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
    expect_identical(
      summary_one_deploy,
      summarize_observations(x, group_by = "deploymentID") %>%
        dplyr::filter(deploymentID == "00a2c20d")
    )
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
        names(summary),
        c("lifeStage",
          "n_scientificName",
          "n_events",
          "n_observations",
          "sum_count",
          "rai_observations",
          "rai_count")
      )
      # No deployments info is present
      expect_true(!"deploymentID" %in% names(summary))
      # All life stages are present
      x <- x %>% filter_observations(.data$observationLevel == "event")
      expect_true(all(
        unique(dplyr::pull(observations(x), "lifeStage")) %in%
          summary$lifeStage
      ))
      
      # Correct type of columns
      expect_true(is.factor(summary$lifeStage))
      expect_true(is.integer(summary$n_scientificName))
      expect_true(is.integer(summary$n_events))
      expect_true(is.integer(summary$n_observations))
      expect_true(is.integer(summary$sum_count))
      expect_true(is.numeric(summary$rai_observations))
      expect_true(is.numeric(summary$rai_count))
      
      # Sum of n_scientificName in summary is equal or greater than the number
      # of distinct scientific names in observations.
      expect_true(
        sum(summary$n_scientificName) >=
          dplyr::n_distinct(
            dplyr::pull(observations(x), "scientificName"),
            na.rm = TRUE
          )
      )
      # Sum of number of events in summary is equal or greater than the number
      # of distinct events in observations = same event can lead to multiple
      # observations (multiple animals spotted)
      expect_true(
        sum(summary$n_events) >=
        dplyr::n_distinct(
          dplyr::pull(observations(x), "eventID"),
          na.rm = TRUE
        )
      )
      # Total number of observations is preserved
      expect_identical(
        sum(summary$n_observations),
        nrow(observations(x))
      )
      # Total number of individuals is preserved
      expect_identical(
        sum(summary$sum_count),
        as.integer(sum(dplyr::pull(observations(x), "count"), na.rm = TRUE))
      )
      # RAI cannot be calculated without grouping by deployments columns:
      # rai_observations and rai_count are NA
      expect_true(all(is.na(summary$rai_observations)))
      expect_true(all(is.na(summary$rai_count)))
    })

testthat::test_that(
  "summarize_observations() returns correct summary when grouping by time", {
    skip_if_offline()
    x <- example_dataset()
    summary <- summarize_observations(x, group_time_by = "day")
    
    # Check that the `summary` has the expected columns
    expect_equal(
      names(summary),
      c("deploymentID",
        "scientificName",
        "day",
        "n_scientificName",
        "n_events",
        "n_observations",
        "sum_count",
        "rai_observations",
        "rai_count")
    )
    # All dates are present
    x_events <- x %>% filter_observations(.data$observationLevel == "event")
    expect_true(all(
      unique(
        lubridate::as_datetime(
          lubridate::as_date(dplyr::pull(observations(x_events), "eventStart"))
        ) %in%
          summary$day
      )
    ))
    
    # Correct type of columns
    expect_true(is.character(summary$deploymentID))
    expect_true(is.character(summary$scientificName))
    expect_true(lubridate::is.timepoint(summary$day))
    expect_true(is.integer(summary$n_scientificName))
    expect_true(is.integer(summary$n_events))
    expect_true(is.integer(summary$n_observations))
    expect_true(is.integer(summary$sum_count))
    expect_true(is.numeric(summary$rai_observations))
    expect_true(is.numeric(summary$rai_count))
    
    # Sum of n_scientificName in summary is equal or greater than the number
    # of distinct scientific names in observations.
    expect_true(
      sum(summary$n_scientificName) >=
        dplyr::n_distinct(
          dplyr::pull(observations(x_events), "scientificName"),
          na.rm = TRUE
        )
    )
    # Sum of number of events in summary is equal or greater than the number
    # of distinct events in observations = same event can lead to multiple
    # observations (multiple animals spotted)
    expect_true(
      sum(summary$n_events) >=
        dplyr::n_distinct(
          dplyr::pull(observations(x_events), "eventID"), na.rm = TRUE)
    )
    # Total number of observations is preserved
    expect_true(
      sum(summary$n_observations) ==
        nrow(observations(x_events))
    )
    # Total number of individuals is preserved
    expect_true(
      sum(summary$sum_count) ==
        sum(dplyr::pull(observations(x_events), "count"), na.rm = TRUE)
    )
    # RAI is equal n_observation * 100 for full days (day != deploymentStart,
    # deploymentEnd of deployment)
    full_days_summary <- 
      summary %>%
      dplyr::left_join(
        # deployments start and end dates (as datetime objects)
        deployments(x_events) %>%
          dplyr::select(deploymentID, deploymentStart, deploymentEnd) %>%
          dplyr::mutate(
            deploymentStart = lubridate::as_datetime(
              lubridate::as_date(deploymentStart)
            ),
            deploymentEnd = lubridate::as_datetime(
              lubridate::as_date(deploymentEnd)
            )
          ),
        by = "deploymentID"
      ) %>%
      dplyr::group_by(deploymentID) %>%
      dplyr::filter(day != deploymentStart & day != deploymentEnd)
    expect_identical(
      full_days_summary$rai_observations,
      full_days_summary$n_observations * 100
    )
    expect_identical(
      full_days_summary$rai_count,
      full_days_summary$sum_count * 100
    )
  })

test_that("get_n_species() and some of its args are deprecated", {
  skip_if_offline()
  x <- example_dataset()
  # Single deprecation without ellipses
  lifecycle::expect_deprecated(get_n_species(x),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Check double deprecation when ellipses are used
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_species(x, ... = "bla"),
      regex = paste0(
        "Arguments passed via `...` are deprecated as of ",
        "camtraptor 1.0.0"
      )
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check error when ellipses is a filtering predicate function
  expect_error(
    lifecycle::expect_deprecated(
      get_n_obs(x, pred_gte("latitude", 51.28)),
      regex = "was deprecated in camtraptor 1.0.0"
    ),
    "was deprecated in camtraptor 1.0.0 and is now defunct"
  )
})

test_that("get_n_species() returns the right output", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  summary_n_species <- get_n_species(x)
  # Right columns
  expect_equal(
    names(summary_n_species),
    c("deploymentID", "n")
  )
  # Right types
  expect_true(is.character(summary_n_species$deploymentID))
  expect_true(is.integer(summary_n_species$n))
  # Same output as summary_observations() with grouping by deploymentID
  expect_identical(
    summary_n_species,
    summarize_observations(x, group_by = "deploymentID") %>%
      dplyr::rename(n = n_scientificName) %>%
      dplyr::select(deploymentID, n)
  )
})

test_that("get_n_obs() and some of its args are deprecated", {
  skip_if_offline()
  x <- example_dataset()
  # Single deprecation with `species` = "all" (default)
  lifecycle::expect_deprecated(get_n_obs(x, species = "all"),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Single deprecation with `species` = NULL
  lifecycle::expect_deprecated(get_n_obs(x, species = NULL),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Check double deprecation when `species` is not NULL or not "all"
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_obs(x, species = c("Anas strepera", "Martes foina")),
      regex = "Argument `species` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when `life_stage` is given
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_obs(x, life_stage = c("adult", "subadult")),
      regex = "Argument `life_stage` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when `sex` is given
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_obs(x, sex = c("female", "male")),
      regex = "Argument `sex` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when ellipses are used
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_obs(x, ... = "bla"),
      regex = paste0(
        "Arguments passed via `...` are deprecated as of ",
        "camtraptor 1.0.0"
      )
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check error when ellipses is a filtering predicate function
  expect_error(
    lifecycle::expect_deprecated(
      get_n_obs(x, pred_gte("latitude", 51.28)),
      regex = "was deprecated in camtraptor 1.0.0"
    ),
    "was deprecated in camtraptor 1.0.0 and is now defunct"
  )
})

test_that("get_n_obs() returns the right output", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  summary_n_obs <- get_n_obs(x)
  # Right columns
  expect_equal(
    names(summary_n_obs),
    c("deploymentID", "scientificName", "n")
  )
  # Right types
  expect_true(is.character(summary_n_obs$deploymentID))
  expect_true(is.character(summary_n_obs$scientificName))
  expect_true(is.integer(summary_n_obs$n))
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName
  expect_identical(
    summary_n_obs,
    summarize_observations(x,
                           group_by = c("deploymentID", "scientificName")) %>%
      dplyr::rename(n = n_observations) %>%
      dplyr::select(deploymentID, scientificName, n)
  )
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName and when filtering by species
  summary_n_obs <- get_n_obs(x, species = c("Anas strepera"))
  expect_identical(
    summary_n_obs,
    summarize_observations(
      x %>% filter_observations(scientificName == "Anas strepera"),
      group_by = c("deploymentID", "scientificName")
    ) %>%
      dplyr::rename(n = n_observations) %>%
      dplyr::select(deploymentID, scientificName, n)
  )
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName and when filtering by lifeStage and sex
  life_stages <- c("adult", "subadult")
  sex_values <- c("female", "male")
  summary_n_obs <- get_n_obs(x, sex = sex_values, life_stage = life_stages)
  expect_identical(
    summary_n_obs,
    summarize_observations(
      x %>% 
        filter_observations(sex %in% sex_values, lifeStage %in% life_stages),
      group_by = c("deploymentID", "scientificName")
    ) %>%
      dplyr::rename(n = n_observations) %>%
      dplyr::select(deploymentID, scientificName, n)
  )
})

test_that("get_n_individuals() and some of its args are deprecated", {
  skip_if_offline()
  x <- example_dataset()
  # Single deprecation with `species` = "all" (default)
  lifecycle::expect_deprecated(get_n_individuals(x, species = "all"),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Single deprecation with `species` = NULL
  lifecycle::expect_deprecated(get_n_individuals(x, species = NULL),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Check double deprecation when `species` is not NULL or not "all"
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_individuals(x, species = c("Anas strepera", "Martes foina")),
      regex = "Argument `species` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when `life_stage` is given
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_individuals(x, life_stage = c("adult", "subadult")),
      regex = "Argument `life_stage` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when `sex` is given
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_individuals(x, sex = c("female", "male")),
      regex = "Argument `sex` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when ellipses are used
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_n_individuals(x, "bla"),
      regex = paste0(
        "Arguments passed via `...` are deprecated as of ",
        "camtraptor 1.0.0"
      )
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check error when ellipses is a filtering predicate function
  expect_error(
    lifecycle::expect_deprecated(
      get_n_individuals(x, pred_gte("latitude", 51.28)),
      regex = "was deprecated in camtraptor 1.0.0"
    ),
    "was deprecated in camtraptor 1.0.0 and is now defunct"
  )
})

test_that("get_n_individuals() returns the right output", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  summary_n_individuals <- get_n_individuals(x)
  # Right columns
  expect_equal(
    names(summary_n_individuals),
    c("deploymentID", "scientificName", "n")
  )
  # Right types
  expect_true(is.character(summary_n_individuals$deploymentID))
  expect_true(is.character(summary_n_individuals$scientificName))
  expect_true(is.integer(summary_n_individuals$n))
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName
  expect_identical(
    summary_n_individuals,
    summarize_observations(x,
                           group_by = c("deploymentID", "scientificName")) %>%
      dplyr::rename(n = sum_count) %>%
      dplyr::select(deploymentID, scientificName, n)
  )
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName and when filtering by species
  summary_n_individuals <- get_n_individuals(x, species = c("Anas strepera"))
  expect_identical(
    summary_n_individuals,
    summarize_observations(
      x %>% filter_observations(scientificName == "Anas strepera"),
      group_by = c("deploymentID", "scientificName")
    ) %>%
      dplyr::rename(n = sum_count) %>%
      dplyr::select(deploymentID, scientificName, n)
  )
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName and when filtering by lifeStage and sex
  life_stages <- c("adult", "subadult")
  sex_values <- c("female", "male")
  summary_n_individuals <- get_n_individuals(
    x,
    sex = sex_values,
    life_stage = life_stages
  )
  expect_identical(
    summary_n_individuals,
    summarize_observations(
      x %>%
        filter_observations(sex %in% sex_values, lifeStage %in% life_stages),
      group_by = c("deploymentID", "scientificName")
    ) %>%
      dplyr::rename(n = sum_count) %>%
      dplyr::select(deploymentID, scientificName, n)
  )
})

test_that("get_rai() and some of its args are deprecated", {
  skip_if_offline()
  x <- example_dataset()
  # Single deprecation with `species` = "all" (default)
  lifecycle::expect_deprecated(get_rai(x, species = "all"),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Single deprecation with `species` = NULL
  lifecycle::expect_deprecated(get_rai(x, species = NULL),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Check double deprecation when `species` is not NULL or not "all"
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_rai(x, species = c("Anas strepera", "Martes foina")),
      regex = "Argument `species` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when `life_stage` is given
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_rai(x, life_stage = c("adult", "subadult")),
      regex = "Argument `life_stage` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when `sex` is given
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_rai(x, sex = c("female", "male")),
      regex = "Argument `sex` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when ellipses are used
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_rai(x, "bla"),
      regex = paste0(
        "Arguments passed via `...` are deprecated as of ",
        "camtraptor 1.0.0"
      )
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check error when ellipses is a filtering predicate function
  expect_error(
    lifecycle::expect_deprecated(
      get_rai(x, pred_gte("latitude", 51.28)),
      regex = "was deprecated in camtraptor 1.0.0"
    ),
    "was deprecated in camtraptor 1.0.0 and is now defunct"
  )
})

test_that("get_rai() returns the right output", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  summary_ray <- get_rai(x)
  # Right columns
  expect_equal(
    names(summary_ray),
    c("deploymentID", "scientificName", "rai")
  )
  # Right types
  expect_true(is.character(summary_ray$deploymentID))
  expect_true(is.character(summary_ray$scientificName))
  expect_true(is.numeric(summary_ray$rai))
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName
  expect_identical(
    summary_ray,
    summarize_observations(x,
                           group_by = c("deploymentID", "scientificName")) %>%
      dplyr::rename(rai = rai_observations) %>%
      dplyr::select(deploymentID, scientificName, rai)
  )
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName and when filtering by species
  summary_ray <- get_rai(x, species = c("Anas strepera"))
  expect_identical(
    summary_ray,
    summarize_observations(
      x %>% filter_observations(scientificName == "Anas strepera"),
      group_by = c("deploymentID", "scientificName")
    ) %>%
      dplyr::rename(rai = rai_observations) %>%
      dplyr::select(deploymentID, scientificName, rai)
  )
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName and when filtering by lifeStage and sex
  life_stages <- c("adult", "subadult")
  sex_values <- c("female", "male")
  summary_ray <- get_rai(x, sex = sex_values, life_stage = life_stages)
  expect_identical(
    summary_ray,
    summarize_observations(
      x %>% 
        filter_observations(sex %in% sex_values, lifeStage %in% life_stages),
      group_by = c("deploymentID", "scientificName")
    ) %>%
      dplyr::rename(rai = rai_observations) %>%
      dplyr::select(deploymentID, scientificName, rai)
  )
})

test_that("get_rai_individuals() and some of its args are deprecated", {
  skip_if_offline()
  x <- example_dataset()
  # Single deprecation with `species` = "all" (default)
  lifecycle::expect_deprecated(get_rai_individuals(x, species = "all"),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Single deprecation with `species` = NULL
  lifecycle::expect_deprecated(get_rai_individuals(x, species = NULL),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Check double deprecation when `species` is not NULL or not "all"
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_rai_individuals(x, species = c("Anas strepera", "Martes foina")),
      regex = " Argument `species` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when `life_stage` is given
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_rai_individuals(x, life_stage = c("adult", "subadult")),
      regex = "Argument `life_stage` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when `sex` is given
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_rai_individuals(x, sex = c("female", "male")),
      regex = "Argument `sex` is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when ellipses are used
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_rai_individuals(x, ... = "bla"),
      regex = paste0(
        "Arguments passed via `...` are deprecated as of ",
        "camtraptor 1.0.0"
      )
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check error when ellipses is a filtering predicate function
  expect_error(
    lifecycle::expect_deprecated(
      get_rai_individuals(x, pred_gte("latitude", 51.28)),
      regex = "was deprecated in camtraptor 1.0.0"
    ),
    "was deprecated in camtraptor 1.0.0 and is now defunct"
  )
})

test_that("get_rai_individuals() returns the right output", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  summary_rai_individuals <- get_rai_individuals(x)
  # Right columns
  expect_equal(
    names(summary_rai_individuals),
    c("deploymentID", "scientificName", "rai")
  )
  # Right types
  expect_true(is.character(summary_rai_individuals$deploymentID))
  expect_true(is.character(summary_rai_individuals$scientificName))
  expect_true(is.numeric(summary_rai_individuals$rai))
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName
  expect_identical(
    summary_rai_individuals,
    summarize_observations(x,
                           group_by = c("deploymentID", "scientificName")) %>%
      dplyr::rename(rai = rai_count) %>%
      dplyr::select(deploymentID, scientificName, rai)
  )
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName and when filtering by species
  summary_rai_individuals <- get_rai_individuals(x, species = c("Anas strepera")
  )
  expect_identical(
    summary_rai_individuals,
    summarize_observations(
      x %>% filter_observations(scientificName == "Anas strepera"),
      group_by = c("deploymentID", "scientificName")
    ) %>%
      dplyr::rename(rai = rai_count) %>%
      dplyr::select(deploymentID, scientificName, rai)
  )
  # Same output as summary_observations() with grouping by deploymentID and
  # scientificName and when filtering by lifeStage and sex
  life_stages <- c("adult", "subadult")
  sex_values <- c("female", "male")
  summary_rai_individuals <- get_rai_individuals(
    x,
    sex = sex_values,
    life_stage = life_stages
  )
  expect_identical(
    summary_rai_individuals,
    summarize_observations(
      x %>%
        filter_observations(sex %in% sex_values, lifeStage %in% life_stages),
      group_by = c("deploymentID", "scientificName")
    ) %>%
      dplyr::rename(rai = rai_count) %>%
      dplyr::select(deploymentID, scientificName, rai)
  )
})
