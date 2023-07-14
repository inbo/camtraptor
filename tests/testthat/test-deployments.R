test_that("deployments() can extract deployments from a Camtrap DP", {
  expect_type(deployments(mica), "list")
  expect_s3_class(deployments(mica), "data.frame")
})
