test_that("output of filter_out_timelapse is identical to input if `captureMethod` field is not present", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(filter_out_timelapse(x), x)
})

test_that("output of filter_out_timelapse is identical to input if `captureMethod` field is present but does not contain `timelapse`", {
  skip_if_offline()
  x <- example_dataset()
  observations(x) <- observations(x) %>% 
    dplyr::mutate(captureMethod = "activityDetection")
  expect_identical(filter_out_timelapse(x), x)
})

test_that("output of filter_out_timelapse is different from input if `captureMethod` field is present and contains `timelapse`", {
  skip_if_offline()
  x <- example_dataset()
  observations(x) <- observations(x) %>% 
    dplyr::mutate(captureMethod = "timelapse")
  expect_false(identical(filter_out_timelapse(x), x))
  expect_equal(nrow(observations(filter_out_timelapse(x))), 0)
})
