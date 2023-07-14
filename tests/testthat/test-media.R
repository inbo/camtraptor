test_that("media() can extract media from a Camtrap DP", {
  expect_type(media(mica), "list")
  expect_s3_class(media(mica), "data.frame")
  expect_identical(media(mica),
                   mica$data$media)
})
