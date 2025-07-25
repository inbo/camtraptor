test_that("summarize_deployments() returns error for invalid datapackage", {
  skip_if_offline()
  x <- example_dataset()
  x$data <- NULL
  # Expect error: control of specific error message left to `read_camtrapdp()`
  expect_error(summarize_deployments(x))
})

test_that("summarize_deployments() returns error for invalid group_by", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    summarize_deployments(x, group_by = "invalid"),
    paste0("Invalid value for group_by parameter: invalid.\n",
           "Valid inputs are: deploymentID, locationID, locationName ",
           "and deploymentTags"
    )
  )
})

test_that("summarize_deployments() returns error for invalid group_time_by", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    summarize_deployments(x, group_time_by = "invalid"),
    paste0("Invalid value for group_time_by parameter: invalid.\n",
           "Valid inputs are: day, week, month, year and NULL"
    )
  )
})

test_that(
  "summarize_deployments() returns correct summary with default arguments", {
    skip_if_offline()
    x <- example_dataset()
    summary <- summarize_deployments(x)
    
    # The returned summary is of type list
    expect_type(summary, "list")
    # The returned summary is a tibble data.frame
    expect_equal(
      class(summary),
      c("tbl_df", "tbl", "data.frame")
    )
    # Check that the `summary` has the expected columns
    expect_equal(
      c("deploymentID", "effort_duration"), names(summary)
    )
    
    # Check that the number of returned deployments matches the number of
    # deployments in the dataset
    expect_equal(nrow(summary), nrow(deployments(x)))
    
    # Check that the order of deployments is the same as in the dataset
    expect_equal(
      summary$deploymentID,
      purrr::pluck(deployments(x),"deploymentID")
    )
    # Check that `deploymentID` is a character
    expect_true(is.character(summary$deploymentID))
    # Check that `effort_duration` is a duration object from lubridate
    expect_true(
      class(summary$effort_duration) == class(lubridate::duration(1))
    )
    
    # Effort is equal to `end` - `start`
    starts <- purrr::pluck(deployments(x), "deploymentStart")
    ends <- purrr::pluck(deployments(x), "deploymentEnd")
    deployment_durations <- lubridate::as.duration(ends - starts)
    expect_equal(summary$effort_duration, deployment_durations)
  })

test_that(
  "summarize_deployments() returns correct summary using `group_by`", {
    skip_if_offline()
    x <- example_dataset()
    deployments <- deployments(x)
    
    # Default summary is the same as summary with `group_by = "deploymentID"`
    summary <- summarize_deployments(x, group_by = "deploymentID")
    expect_identical(summary, summarize_deployments(x))
    
    # Use `group_by = c("locationID", "deploymentID")`
    summary_location_id <- summarize_deployments(
      x,
      group_by = c("locationID", "deploymentID")
    )
    # The summary has the expected columns
    expect_identical(
      names(summary_location_id),
      c("locationID", "deploymentID", "effort_duration")
    )
    # The summary has the same effort duration
    expect_identical(
      summary_location_id %>%
        dplyr::arrange(.data$deploymentID) %>%
        dplyr::pull(effort_duration),
      summary$effort_duration
    )
    # The summary has the same values in `locationID` column as in deployments
    expect_identical(
      summary_location_id$locationID,
      deployments %>%
        dplyr::arrange(locationID) %>%
        dplyr::pull(locationID)
    )
    
    # Group by `locationID` only. If `locationID` values are not unique, the
    # effort is summed up
    deploys <- deployments
    deploys$locationID[1] <- deploys$locationID[2]
    deploys$locationID[3] <- deploys$locationID[4]
    deployments(x) <- deploys
    summary_location_id <- summarize_deployments(
      x, group_by = "locationID"
    )
    
    # The summary has the expected columns
    expect_identical(
      names(summary_location_id), c("locationID", "effort_duration")
    )
    # The summary has two rows less than the deployments
    expect_equal(
      nrow(summary_location_id), nrow(deployments(x)) - 2
    )
    # The summary has the right location IDs
    expect_identical(
      summary_location_id$locationID,
      unique(purrr::pluck(deployments(x), "locationID"))
    )
    # The returned effort is the sum of the effort of deployments
    expect_identical(
      summary_location_id$effort_duration,
      c(summary$effort_duration[1] + summary$effort_duration[2],
        summary$effort_duration[3] + summary$effort_duration[4]
      )
    )
  })

test_that(
  "summarize_deployments() returns correct summary using `group_time_by`", {
    skip_if_offline()
    x <- example_dataset()
    deployments <- deployments(x)
    summary <- summarize_deployments(x, group_time_by = "day")
    
    # The returned summary is of type list
    expect_type(summary, "list")
    # The returned summary is a tibble data.frame
    expect_equal(
      class(summary),
      c("tbl_df", "tbl", "data.frame")
    )
    
    # Check that the `summary` has the expected columns
    expect_equal(
      c("deploymentID", "day", "effort_duration"), names(summary)
    )
    
    # Check that `effort_duration` is a duration object from lubridate
    expect_true(
      class(summary$effort_duration) == class(lubridate::duration(1))
    )
    
    # Column `day` contains only dates between deployment start and end
    # Check for one deployment
    deployment_id <- purrr::pluck(deployments(x), "deploymentID")[1]
    start <- purrr::pluck(deployments(x), "deploymentStart")[1]
    end <- purrr::pluck(deployments(x), "deploymentEnd")[1]
    start_day <- lubridate::floor_date(start, "day")
    end_day <- lubridate::floor_date(end, "day")
    days <- lubridate::as_datetime(
      seq.Date(lubridate::as_date(start_day),
               lubridate::as_date(end_day),
               by = "days"
      )
    )
    summary_days <- summary %>%
      dplyr::filter(.data$deploymentID == deployment_id) %>%
      dplyr::pull(day)
    expect_identical(days, summary_days)
    
    # Duration is equal to a full day except for the start and end day
    # Check for one deployment
    n_full_days <- length(days) - 2
    effort_full_days <- rep(
      lubridate::duration(1, units = "days"),
      n_full_days
    )
    summary_effort_full_days <- summary %>%
      dplyr::filter(.data$deploymentID == deployment_id) %>%
      dplyr::filter(day > days[1] & day < days[length(days)]) %>%
      dplyr::pull(effort_duration)
    testthat::expect_identical(effort_full_days,
                               summary_effort_full_days
    )
    # Duration is correct during the days of `deploymentStart` and
    # `deploymentEnd` days.
    # Check for one deployment
    effort_start_day <- lubridate::as.duration(
      lubridate::ceiling_date(start, "day") - start
    )
    effort_end_day <- lubridate::as.duration(
      lubridate::ceiling_date(end, "day") - end
    )
    summary_effort_start <- summary %>%
      dplyr::filter(.data$deploymentID == deployment_id) %>%
      dplyr::filter(.data$day == lubridate::floor_date(start, unit = "days")) %>%
      dplyr::pull(effort_duration)
    testthat::expect_identical(effort_start_day,
                               summary_effort_start)
  })

# Check that output of `summarise_deployments()` is the same as
# `summarize_deployments()`
test_that(
  paste0("The synonyms summarize_deployments() and summarise_deployments() ",
         "return the same output"
  ), {
    skip_if_offline()
    x <- example_dataset()
    expect_identical(
      summarize_deployments(x),
      summarise_deployments(x)
    )
  })

test_that("get_effort() is deprecated", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_deprecated(get_effort(x, unit = NULL),
                               regex = "was deprecated in camtraptor 1.0.0.")
  # Check double deprecation when `unit` is not NULL
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_effort(x, unit = "day"),
      regex = "is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when ellipses are used and are not defunct
  # filtering predicates
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_effort(x, unit = NULL, ... = "bla"),
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
      get_effort(x, unit = NULL, pred_gte("latitude", 51.28)),
      regex = "was deprecated in camtraptor 1.0.0"
    ),
    "was deprecated in camtraptor 1.0.0 and is now defunct"
  )
})

test_that("get_effort() returns the right output", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  summary_effort <- get_effort(x, unit = NULL)
  # Right columns
  expect_equal(
    names(summary_effort),
    c("deploymentID", "effort_duration")
  )
  # Right types
  expect_true(is.character(summary_effort$deploymentID))
  expect_true(lubridate::is.duration(summary_effort$effort_duration))
  # Same output as summary_deployments() with grouping by deploymentID
  expect_identical(
    summary_effort,
    summarize_deployments(x, group_by = "deploymentID")
  )
})

test_that("get_custom_effort() is deprecated", {
  skip_if_offline()
  x <- example_dataset() 
  lifecycle::expect_deprecated(
    get_custom_effort(x,
                      start = NULL,
                      end = NULL, 
                      group_by = "month",
                      unit = NULL),
    regex = "was deprecated in camtraptor 1.0.0.")
  # Check double deprecation when `unit` is not NULL
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_custom_effort(x,
                        start = NULL,
                        end = NULL, 
                        group_by = "month",
                        unit = "day"),
      regex = "is deprecated as of camtraptor 1.0.0"
    ),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  # Check double deprecation when ellipses are used and are not defunct
  # filtering predicates
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      get_custom_effort(x, unit = NULL, ... = "bla"),
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
      get_custom_effort(x, unit = NULL, pred_gte("latitude", 51.28)),
      regex = "was deprecated in camtraptor 1.0.0"
    ),
    "was deprecated in camtraptor 1.0.0 and is now defunct"
  )
})

test_that("get_custom_effort() returns the right output", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  summary_custom_effort <- get_custom_effort(
    x, group_by = "month", unit = NULL
  )
  # Right columns
  expect_equal(
    names(summary_custom_effort),
    c("deploymentID", "month", "effort_duration")
  )
  # Right types
  expect_true(is.character(summary_custom_effort$deploymentID))
  expect_true(lubridate::is.timepoint(summary_custom_effort$month))
  expect_true(lubridate::is.duration(summary_custom_effort$effort_duration))
  # Same output as summary_deployments() with grouping by deploymentID and
  # grouping time by month
  expect_identical(
    summary_custom_effort,
    summarize_deployments(x, group_by = "deploymentID", group_time_by = "month")
  )
  
  # Same output as get_effort() if no group_by is specified
  expect_identical(
    get_custom_effort(x, unit = NULL),
    get_effort(x, unit = NULL)
  )
})
