test_that("input of get_cam_op, camtrap dp, is checked properly", {
  expect_error(get_cam_op("aaa"))
  expect_error(get_cam_op(1))
})

test_that("output of get_cam_op is a matrix with right structure", {
  cam_op_matrix <- get_cam_op(camtrapdp)
  expect_equal(class(cam_op_matrix), c("matrix", "array"))
  expect_equal(nrow(cam_op_matrix)= unique(camtrapdp$deployments$location_name))
})


