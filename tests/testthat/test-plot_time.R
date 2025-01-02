test_that("plot_time()", {
  expect_no_error(plot_time(mica))
  
  expect_no_error(plot_time(mica, yaxis= "deploymentID"))
})