test_that("observations() can extract observations from a Camtrap DP", {
  expect_type(observations(mica), "list")
  expect_s3_class(observations(mica), "data.frame")
  expect_s3_class(observations(mica), "tbl")
  expect_s3_class(observations(mica), "tbl_df")
  expect_identical(observations(mica),
                   mica$data$observations)
})
