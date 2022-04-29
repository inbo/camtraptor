test_that("input camtrap dp is checked properly", {
  expect_error(get_cam_op("aaa"))
  expect_error(get_cam_op(1))
})

test_that("output is a matrix", {
  cam_op_matrix <- get_cam_op(mica)
  expect_true("matrix" %in% class(cam_op_matrix))
})

test_that("output matrix has locations as rownames", {
  cam_op_matrix <- get_cam_op(mica)
  locations <- mica$deployments$locationName
  n_locations <- length(mica$deployments$locationName)
  expect_equal(nrow(cam_op_matrix), n_locations)
  expect_equal(row.names(cam_op_matrix), locations)
})

test_that("output matrix has Station prefix in rownames", {
  cam_op_matrix <- get_cam_op(mica, use_prefix = TRUE)
  locations <- paste0("Station", mica$deployments$locationName)
  n_locations <- length(mica$deployments$locationName)
  expect_equal(nrow(cam_op_matrix), n_locations)
  expect_equal(row.names(cam_op_matrix), locations)
})

test_that("output matrix has specified location column as rownames", {
  cam_op_matrix <- get_cam_op(mica, station_col = "locationID")
  locations <- mica$deployments$locationID
  n_locations <- length(mica$deployments$locationID)
  expect_equal(nrow(cam_op_matrix), n_locations)
  expect_equal(row.names(cam_op_matrix), locations)
})

test_that("output matrix has all deployment days as colnames", {
  cam_op_matrix <- get_cam_op(mica)
  days_activity <- seq(as.Date(min(mica$deployments$start)),
                        as.Date(max(mica$deployments$end)),
                       by = "days")
  days_activity <- as.character(days_activity)
  n_days <- length(days_activity)
  expect_equal(ncol(cam_op_matrix), n_days)
  expect_equal(colnames(cam_op_matrix), days_activity)
})

test_that("daily effort is > 0 for fully active days, NA for inactive days", {
  cam_op_matrix <- get_cam_op(mica)
  location <- mica$deployments$locationName[4]
  deployment_start <- mica$deployments %>%
    filter(locationName == location) %>%
    pull(start)
  deployment_end <- mica$deployments %>%
    filter(locationName == location) %>%
    pull(end)
  cols_activity <- seq(as.Date(deployment_start) + lubridate::ddays(1),
                       as.Date(deployment_end) - lubridate::ddays(1),
                       by = "days")
  cols_activity <- as.character(cols_activity)

  cols_inactivity <- seq(as.Date(deployment_end+ddays(1)),
                           as.Date(max(mica$deployments$end)),
                           by = "days")
  cols_inactivity <- as.character(cols_inactivity)
  expect_true(all(cam_op_matrix[4, cols_activity] > 0))
  expect_true(all(is.na(cam_op_matrix[4, cols_inactivity])))
})

test_that("daily effort is > 0 and < 1 for partial active days (start/end)", {
  cam_op_matrix <- get_cam_op(mica)
  location <- mica$deployments$locationName[4]
  start <- as.character(as.Date(mica$deployments$start[4]))
  end <- as.character(as.Date(mica$deployments$end[4]))
  expect_true(cam_op_matrix[4, start] > 0)
  expect_true(cam_op_matrix[4, start] <  1)
  expect_true(cam_op_matrix[4, end] > 0)
  expect_true(cam_op_matrix[4, end] < 1)
})

test_that("filtering predicates are allowed and work well", {
  filtered_cam_op_matrix <- get_cam_op(mica, pred_lt("longitude", 4.0))
  expect_equal(rownames(filtered_cam_op_matrix), "Mica Viane")
})
