test_that("input camtrap dp is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  # Character instead of datapackage
  expect_error(camtrapR_cameraOperation("aaa"))
  # Numeric instead of datapackage
  expect_error(camtrapR_cameraOperation(1))
  # Station_col is not NA
  expect_error(
    camtrapR_cameraOperation(x, station_col = NA),
    "station_col is not a string (a length one character vector).",
    fixed = TRUE)
  # Station_col is length 1
  expect_error(
    camtrapR_cameraOperation(x, station_col = c("locationID","locationName")),
    "station_col is not a string (a length one character vector).",
    fixed = TRUE)
  # Station_col value is not a column of deployments
  expect_error(
    camtrapR_cameraOperation(x, station_col = "bla"),
    paste0(
      "Station column name (`bla`) is not valid: ", 
      "it must be one of the deployments column names."
    ),
    fixed = TRUE
  )
  # Column specified by station_col contains empty values
  x_empty_location_name <- x
  x_empty_location_name$data$deployments$locationName[2:3] <- NA
  expect_error(camtrapR_cameraOperation(x_empty_location_name),
               "Column `locationName` must be non-empty: 2 NAs found."
  )
  # Camera_col is not NA
  expect_error(
    camtrapR_cameraOperation(x, camera_col = NA),
    "camera_col is not a string (a length one character vector).",
    fixed = TRUE)
  # Camera_col is length 1
  expect_error(
    camtrapR_cameraOperation(x, camera_col = c("locationID","locationName")),
    "camera_col is not a string (a length one character vector).",
    fixed = TRUE)
  # Station_col value is not a column of deployments
  expect_error(
    camtrapR_cameraOperation(x, camera_col = "bla"),
    paste0(
      "Camera column name (`bla`) is not valid: ", 
      "it must be one of the deployments column names."
    ),
    fixed = TRUE
  )
  # Session_col is not NA
  expect_error(
    camtrapR_cameraOperation(x, session_col = NA),
    "session_col is not a string (a length one character vector).",
    fixed = TRUE)
  # Session_col is length 1
  expect_error(
    camtrapR_cameraOperation(x, session_col = c("locationID","locationName")),
    "session_col is not a string (a length one character vector).",
    fixed = TRUE)
  # Session_col value is not a column of deployments
  expect_error(
    camtrapR_cameraOperation(x, session_col = "bla"),
    paste0(
      "Session column name (`bla`) is not valid: ", 
      "it must be one of the deployments column names."
    ),
    fixed = TRUE
  )
  # use_prefix must be TRUE or FALSE
  expect_error(camtrapR_cameraOperation(x, use_prefix = "bla"))
  expect_error(camtrapR_cameraOperation(x, use_prefix = NA))
})

test_that("output is a matrix", {
  skip_if_offline()
  x <- example_dataset()
  cam_op_matrix <- camtrapR_cameraOperation(x)
  expect_true(is.matrix(cam_op_matrix))
})

test_that("output matrix has locations as rownames", {
  skip_if_offline()
  x <- example_dataset()
  cam_op_matrix <- camtrapR_cameraOperation(x)
  locations <- purrr::pluck(deployments(x), "locationName")
  n_locations <- length(locations)
  expect_identical(nrow(cam_op_matrix), n_locations)
  expect_identical(row.names(cam_op_matrix), locations)
})

test_that("output matrix has sessions addded to locations as rownames", {
  skip_if_offline()
  x <- example_dataset()
  x_sessions <- x
  x_sessions$data$deployments <- deployments(x_sessions) %>%
    dplyr::mutate(session = ifelse(
      stringr::str_starts(.data$locationName, "B_DL_"),
      "after2020",
      "before2020"
   )
  )
  cam_op_matrix <- camtrapR_cameraOperation(x_sessions, session_col = "session")
  locations_sessions <- paste(deployments(x_sessions)$locationName,
                              deployments(x_sessions)$session,
                              sep = "__SESS_"
  )
  n_locations <- length(deployments(x_sessions)$locationName)
  expect_identical(nrow(cam_op_matrix), n_locations)
  expect_identical(row.names(cam_op_matrix), locations_sessions)
})

test_that("output matrix has camera IDs addded to locations as rownames", {
  skip_if_offline()
  x <- example_dataset()
  x_cameras <- x
  x_cameras$data$deployments$cameraID <- c(1, 2, 3, 4)
  cam_op_matrix <- camtrapR_cameraOperation(x_cameras, camera_col = "cameraID")
  locations_cameras <- paste(deployments(x_sessions)$locationName,
                             deployments(x_sessions)$cameraID,
                             sep = "__CAM_"
  )
  n_locations <- length(deployments(x_cameras)$locationName)
  expect_identical(nrow(cam_op_matrix), n_locations)
  expect_identical(row.names(cam_op_matrix), locations_cameras)
})

test_that(
  "output matrix has sessions and cameras addded to locations as rownames", {
    skip_if_offline()
    x <- example_dataset()
    x_sess_cam <- x
    x_sess_cam$data$deployments$cameraID <- c(1, 2, 3, 4)
    x_sess_cam$data$deployments$session <- c(1, 2, 3, 4)
    cam_op_matrix <- camtrapR_cameraOperation(x_sess_cam, 
                                camera_col = "cameraID", 
                                session_col = "session"
    )
    locations_sess_cam <- paste(deployments(x_sess_cam)$locationName,
                                deployments(x_sess_cam)$session,
                               sep = "__SESS_"
    )
    locations_sess_cam <- paste(locations_sess_cam,
                                deployments(x_sess_cam)$cameraID,
                                sep = "__CAM_"
    )
    n_locations <- length(deployments(x_sess_cam)$locationName)
    expect_identical(nrow(cam_op_matrix), n_locations)
    expect_identical(row.names(cam_op_matrix), locations_sess_cam)
})

test_that(
  "__SESS_ is a reserved word not used in station, session and camera columns",
  {
    skip_if_offline()
    x <- example_dataset()
    x_sess <- x
    x_sess$data$deployments$session <- c("1__SESS_1")
    expect_error(camtrapR_cameraOperation(x_sess, session_col = "session"),
                 paste0("Session column name (`session`) must not contain any ",
                        "of the reserved words: \"__SESS_\", \"__CAM_\"."),
                 fixed = TRUE
    )
    x_sess <- x
    x_sess$data$deployments$cameraID <- paste0(c(1,2,3,4), "__SESS_")
    expect_error(camtrapR_cameraOperation(x_sess, camera_col = "cameraID"),
                 paste0("Camera column name (`cameraID`) must not contain any ",
                        "of the reserved words: \"__SESS_\", \"__CAM_\"."),
                 fixed = TRUE
    )
    x_sess <- x
    x_sess$data$deployments$locationName[1] <- paste0(
      "__SESS_",
      deployments(x_sess)$locationName[1]
    )
    expect_error(
      camtrapR_cameraOperation(x_sess),
      paste0("Station column name (`locationName`) must not contain any ",
             "of the reserved words: \"__SESS_\", \"__CAM_\"."),
      fixed = TRUE
    )
  }
)

test_that(
  "__CAM_ is a reserved word not used in station, session and camera columns",
  {
    skip_if_offline()
    x <- example_dataset()
    x_cam <- x
    x_cam$data$deployments$session[1] <- c("1__CAM_1")
    expect_error(camtrapR_cameraOperation(x_cam, session_col = "session"),
                 paste0("Session column name (`session`) must not contain any ",
                        "of the reserved words: \"__SESS_\", \"__CAM_\"."),
                 fixed = TRUE
    )
    x_cam <- x
    x_cam$data$deployments$cameraID <- paste0(c(1,2,3,4), "__CAM_")
    expect_error(camtrapR_cameraOperation(x_cam, camera_col = "cameraID"),
                 paste0("Camera column name (`cameraID`) must not contain any ",
                        "of the reserved words: \"__SESS_\", \"__CAM_\"."),
                 fixed = TRUE
    )
    x_cam <- x
    x_cam$data$deployments$locationName[1] <- paste0(
      "__CAM_",
      deployments(x_cam)$locationName[1]
    )
    expect_error(
      camtrapR_cameraOperation(x_cam),
      paste0("Station column name (`locationName`) must not contain any ",
             "of the reserved words: \"__SESS_\", \"__CAM_\"."),
      fixed = TRUE
    )
  }
)

test_that("output matrix has Station prefix in rownames", {
  skip_if_offline()
  x <- example_dataset()
  cam_op_matrix <- camtrapR_cameraOperation(x, use_prefix = TRUE)
  locations <- paste0("Station", purrr::pluck(deployments(x), "locationName"))
  n_locations <- length(locations)
  expect_identical(nrow(cam_op_matrix), n_locations)
  expect_identical(row.names(cam_op_matrix), locations)
})

test_that("output matrix has specified location column as rownames", {
  skip_if_offline()
  x <- example_dataset()
  cam_op_matrix <- camtrapR_cameraOperation(x, station_col = "locationID")
  locations <- purrr::pluck(deployments(x), "locationID")
  n_locations <- length(locations)
  expect_identical(nrow(cam_op_matrix), n_locations)
  expect_identical(row.names(cam_op_matrix), locations)
})


test_that("output matrix has all deployment days as colnames", {
  skip_if_offline()
  x <- example_dataset()
  cam_op_matrix <- camtrapR_cameraOperation(x)
  days_activity <- seq(as.Date(min(purrr::pluck(deployments(x), "deploymentStart"))),
    as.Date(max(purrr::pluck(deployments(x), "deploymentEnd"))),
    by = "days"
  )
  days_activity <- as.character(days_activity)
  n_days <- length(days_activity)
  expect_identical(ncol(cam_op_matrix), n_days)
  expect_identical(colnames(cam_op_matrix), days_activity)
})

test_that("daily effort is > 0 for fully active days, NA for inactive days", {
  skip_if_offline()
  x <- example_dataset()
  cam_op_matrix <- camtrapR_cameraOperation(x)
  location <- purrr::pluck(deployments(x), "locationName", 4)
  deployment_start <- deployments(x) %>%
    dplyr::filter(locationName == location) %>%
    dplyr::pull(deploymentStart)
  deployment_end <- deployments(x) %>%
    dplyr::filter(locationName == location) %>%
    dplyr::pull(deploymentEnd)
  cols_activity <- seq(as.Date(deployment_start) + lubridate::ddays(1),
    as.Date(deployment_end) - lubridate::ddays(1),
    by = "days"
  )
  cols_activity <- as.character(cols_activity)

  cols_inactivity <- seq(as.Date(deployment_end + lubridate::ddays(1)),
    as.Date(max(purrr::pluck(deployments(x), "deploymentEnd"))),
    by = "days"
  )
  cols_inactivity <- as.character(cols_inactivity)
  expect_true(all(cam_op_matrix[4, cols_activity] > 0))
  expect_true(all(is.na(cam_op_matrix[4, cols_inactivity])))
})

test_that("daily effort is > 0 and < 1 for partial active days (start/end)", {
  skip_if_offline()
  x <- example_dataset()
  cam_op_matrix <- camtrapR_cameraOperation(x)
  location <- purrr::pluck(deployments(x), "locationName", 4)
  start <- as.character(
    as.Date(purrr::pluck(deployments(x), "deploymentStart", 4))
  )
  end <- as.character(
    as.Date(purrr::pluck(deployments(x), "deploymentEnd", 4))
  )
  expect_gt(cam_op_matrix[4, deploymentStart], 0)
  expect_lt(cam_op_matrix[4, deploymentStart],1)
  expect_gt(cam_op_matrix[4, deploymentEnd], 0)
  expect_lt(cam_op_matrix[4, deploymentEnd], 1)
})

test_that(
  "effort is > 1 for locations with multiple deployments active at same time",
  {
    skip_if_offline()
    x <- example_dataset()
    x1 <- x
    x1$data$deployments$deploymentStart[2] <- lubridate::as_datetime("2020-07-30 21:00:00")
    x1$data$deployments$deploymentEnd[2] <- lubridate::as_datetime("2020-08-07 21:00:00")
    x1$data$deployments$locationName[2] <- deployments(x1)$locationName[1]
    cam_op_matrix <- camtrapR_cameraOperation(x1)

    first_full_day_two_deps <- as.character(
      as.Date(deployments(x1)$deploymentStart[2]) + lubridate::ddays(1)
    )
    last_full_day_two_deps <- as.character(
      as.Date(deployments(x1)$deployments$deploymentEnd[2]) - lubridate::ddays(1)
    )
    # as many rows as locations
    expect_true(
      nrow(cam_op_matrix) == length(unique(deployments(x1)$locationName))
    )
    expect_gt(cam_op_matrix[1, first_full_day_two_deps], 1)
    expect_gt(cam_op_matrix[1, last_full_day_two_deps], 1)
  }
)

test_that(
  "0<effort<=1 for locations with multiple deployments not simultaneously active",
  {
    skip_if_offline()
    x <- example_dataset()
    x1 <- x
    x1$data$deployments$locationName[2] <- purrr::pluck(
      deployments(x1),
      "locationName",
      1
    )
    cam_op_matrix1 <- camtrapR_cameraOperation(x1)
    cam_op_matrix <- camtrapR_cameraOperation(x)
    start_date1 <- as.character(
      as.Date(purrr::pluck(deployments(x), "deploymentStart", 1))
    )
    start_date2 <- as.character(
      as.Date(purrr::pluck(deployments(x), "deploymentStart", 2))
    )
    end_date1 <- as.character(
      as.Date(purrr::pluck(deployments(x), "deploymentEnd", 1))
    )
    end_date2 <- as.character(
      as.Date(purrr::pluck(deployments(x), "deploymentEnd", 2))
    )
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
