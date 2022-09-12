test_that("input camtrap dp is checked properly", {
  # character instead of datapackage
  expect_error(get_cam_op("aaa"))
  # numeric instead of datapackage
  expect_error(get_cam_op(1))
  # station_col value is not a column of deployments
  expect_error(get_cam_op(mica, station_col = "bla"))
  # use_prefix must be TRUE or FALSE
  expect_error(get_cam_op(mica, use_prefix = "bla"))
  expect_error(get_cam_op(mica, use_prefix = NA))
})

test_that("output is a matrix", {
  cam_op_matrix <- get_cam_op(mica)
  expect_true("matrix" %in% class(cam_op_matrix))
})

test_that("output matrix has locations as rownames", {
  cam_op_matrix <- get_cam_op(mica)
  locations <- mica$data$deployments$locationName
  n_locations <- length(mica$data$deployments$locationName)
  expect_equal(nrow(cam_op_matrix), n_locations)
  expect_equal(row.names(cam_op_matrix), locations)
})

test_that("output matrix has Station prefix in rownames", {
  cam_op_matrix <- get_cam_op(mica, use_prefix = TRUE)
  locations <- paste0("Station", mica$data$deployments$locationName)
  n_locations <- length(mica$data$deployments$locationName)
  expect_equal(nrow(cam_op_matrix), n_locations)
  expect_equal(row.names(cam_op_matrix), locations)
})

test_that("output matrix has specified location column as rownames", {
  cam_op_matrix <- get_cam_op(mica, station_col = "locationID")
  locations <- mica$data$deployments$locationID
  n_locations <- length(mica$data$deployments$locationID)
  expect_equal(nrow(cam_op_matrix), n_locations)
  expect_equal(row.names(cam_op_matrix), locations)
})

test_that("output matrix has all deployment days as colnames", {
  cam_op_matrix <- get_cam_op(mica)
  days_activity <- seq(as.Date(min(mica$data$deployments$start)),
    as.Date(max(mica$data$deployments$end)),
    by = "days"
  )
  days_activity <- as.character(days_activity)
  n_days <- length(days_activity)
  expect_equal(ncol(cam_op_matrix), n_days)
  expect_equal(colnames(cam_op_matrix), days_activity)
})

test_that("daily effort is > 0 for fully active days, NA for inactive days", {
  cam_op_matrix <- get_cam_op(mica)
  location <- mica$data$deployments$locationName[4]
  deployment_start <- mica$data$deployments %>%
    dplyr::filter(locationName == location) %>%
    dplyr::pull(start)
  deployment_end <- mica$data$deployments %>%
    dplyr::filter(locationName == location) %>%
    dplyr::pull(end)
  cols_activity <- seq(as.Date(deployment_start) + lubridate::ddays(1),
    as.Date(deployment_end) - lubridate::ddays(1),
    by = "days"
  )
  cols_activity <- as.character(cols_activity)

  cols_inactivity <- seq(as.Date(deployment_end + lubridate::ddays(1)),
    as.Date(max(mica$data$deployments$end)),
    by = "days"
  )
  cols_inactivity <- as.character(cols_inactivity)
  expect_true(all(cam_op_matrix[4, cols_activity] > 0))
  expect_true(all(is.na(cam_op_matrix[4, cols_inactivity])))
})

test_that("daily effort is > 0 and < 1 for partial active days (start/end)", {
  cam_op_matrix <- get_cam_op(mica)
  location <- mica$data$deployments$locationName[4]
  start <- as.character(as.Date(mica$data$deployments$start[4]))
  end <- as.character(as.Date(mica$data$deployments$end[4]))
  expect_true(cam_op_matrix[4, start] > 0)
  expect_true(cam_op_matrix[4, start] < 1)
  expect_true(cam_op_matrix[4, end] > 0)
  expect_true(cam_op_matrix[4, end] < 1)
})

test_that(
  "effort is > 1 for locations with multiple deployments active at same time",
  {
    mica1 <- mica
    mica1$data$deployments$start[2] <- lubridate::as_datetime("2020-07-30 21:00:00")
    mica1$data$deployments$end[2] <- lubridate::as_datetime("2020-08-07 21:00:00")
    mica1$data$deployments$locationName[2] <- mica1$data$deployments$locationName[1]
    cam_op_matrix <- get_cam_op(mica1)

    first_full_day_two_deps <- as.character(
      as.Date(mica1$data$deployments$start[2]) + lubridate::ddays(1)
    )
    last_full_day_two_deps <- as.character(
      as.Date(mica1$data$deployments$end[2]) - lubridate::ddays(1)
    )
    # as many rows as locations
    expect_true(
      nrow(cam_op_matrix) == length(unique(mica1$data$deployments$locationName))
    )
    expect_true(cam_op_matrix[1, first_full_day_two_deps] > 1)
    expect_true(cam_op_matrix[1, last_full_day_two_deps] > 1)
  }
)

test_that(
  "0<effort<=1 for locations with multiple deployments not simultaneously active",
  {
    mica1 <- mica
    mica1$data$deployments$locationName[2] <- mica1$data$deployments$locationName[1]
    cam_op_matrix1 <- get_cam_op(mica1)
    cam_op_matrix <- get_cam_op(mica)
    start_date1 <- as.character(as.Date(mica$data$deployments$start[1]))
    start_date2 <- as.character(as.Date(mica$data$deployments$start[2]))
    end_date1 <- as.character(as.Date(mica$data$deployments$end[1]))
    end_date2 <- as.character(as.Date(mica$data$deployments$end[2]))
    col_idx_start1 <- which(colnames(cam_op_matrix1) == start_date1)
    col_idx_end1 <- which(colnames(cam_op_matrix1) == end_date1)
    col_idx_start2 <- which(colnames(cam_op_matrix1) == start_date2)
    col_idx_end2 <- which(colnames(cam_op_matrix1) == end_date2)

    # all values are greater than 0 (not allowed at the moment) and less or
    # equal 1
    expect_true(all(cam_op_matrix1[1, ] <= 1, na.rm = TRUE))

    # the non NAs values are exactly the same as the ones in the matrix with two
    # deployments apart
    expect_true(all(cam_op_matrix1[1, col_idx_start1:col_idx_end1] ==
      cam_op_matrix[1, col_idx_start1:col_idx_end1]))
    expect_true(all(cam_op_matrix1[1, col_idx_start2:col_idx_end2] ==
      cam_op_matrix[2, col_idx_start2:col_idx_end2]))
  }
)

test_that("filtering predicates are allowed and work well", {
  filtered_cam_op_matrix <- get_cam_op(mica, pred_lt("longitude", 4.0))
  expect_equal(rownames(filtered_cam_op_matrix), "Mica Viane")
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_cam_op(datapkg = mica)
    )
  )
})
