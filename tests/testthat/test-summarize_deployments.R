test_that("summarize_deployments() returns error for invalid datapackage", {
  skip_if_offline()
  x <- example_dataset()
  x$data <- NULL
  # Expect error: control of specific error message left to `read_camtrapdp()`
  expect_error(summarize_deployments(x))
})

test_that(
  "summarize_deployments() returns correct summary with default arguments", {
    skip_if_offline()
    x <- example_dataset()
    summary <- summarize_deployments(x)
    
    # Type list
    expect_type(summary, "list")
    # Class tibble data.frame
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
    
    # Check that the order of dpeloyments is the same as in the dataset
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
  "summarize_deployments() returns correct summary using `group_time_by`", {
    skip_if_offline()
    x <- example_dataset()
    deployments <- deployments(x)
    summary <- summarize_deployments(x, group_time_by = "day")
    
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
