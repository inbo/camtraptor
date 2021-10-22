test_that("input of get_cam_op, camtrap dp, is checked properly", {
  expect_error(get_cam_op("aaa"))
  expect_error(get_cam_op(1))
})

test_that("output of get_cam_op is a matrix", {
  cam_op_matrix <- get_cam_op(mica)
  expect_true("matrix" %in% class(cam_op_matrix))
})

test_that("output matrix has locations as rownames", {
  cam_op_matrix <- get_cam_op(mica)
  locations <- paste0("Station", mica$deployments$location_name)
  n_locations <- length(mica$deployments$location_name)
  expect_equal(nrow(cam_op_matrix), n_locations)
  expect_equal(row.names(cam_op_matrix), locations)
})

test_that("output matrix has all days of deployment activity as colnames", {
  cam_op_matrix <- get_cam_op(mica)
  days_activity <- seq(as.Date(min(mica$deployments$start)),
                        as.Date(max(mica$deployments$end)),
                       by = "days")
  days_activity <- as.character(days_activity)
  n_days <- length(days_activity)
  expect_equal(ncol(cam_op_matrix), n_days)
  expect_equal(colnames(cam_op_matrix), days_activity)
})

test_that("daily effort is > 0 for active days, 0 for inactive days", {
  cam_op_matrix <- get_cam_op(mica)
  location <- mica$deployments$location_name[4]
  deployment_start <- min(mica$deployments %>%
                             filter(location_name == location) %>%
                             pull(start))
  deployment_end <- max(mica$deployments %>%
                             filter(location_name == location) %>%
                             pull(end))
  cols_activity <- seq(as.Date(deployment_start),
                       as.Date(deployment_end),
                       by = "days")
  cols_activity <- as.character(cols_activity)

  cols_inactivity <- seq(as.Date(deployment_end+ddays(1)),
                           as.Date(max(mica$deployments$end)),
                           by = "days")
  cols_inactivity <- as.character(cols_inactivity)
  expect_true(all(cam_op_matrix[4, cols_activity] > 0))
  expect_true(all(cam_op_matrix[4, cols_inactivity] == 0))
})

test_that("daily effort is > 0 and < 1 for partial active days (start/end)", {
  cam_op_matrix <- get_cam_op(mica)
  location <- mica$deployments$location_name[4]
  start <- as.character(as.Date(mica$deployments$start[4]))
  end <- as.character(as.Date(mica$deployments$end[4]))
  expect_true(cam_op_matrix[4, start] > 0)
  expect_true(cam_op_matrix[4, start] <  1)
  expect_true(cam_op_matrix[4, end] > 0)
  expect_true(cam_op_matrix[4, end] < 1)
})

test_that("filtering predicates are allowed and work well", {
  filtered_cam_op_matrix <- get_cam_op(mica, pred_gt("longitude", 3.6))
  expect_equal(rownames(filtered_cam_op_matrix), "StationB_ML_val 07_Sint-Anna")
})
