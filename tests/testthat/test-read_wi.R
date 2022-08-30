# directory <- "tests/testthat/wi_export_sample/"

directory <- "wi_export_sample/"
test_that("file is checked properly", {
  # Work as expected in the default case
  expect_error(read_wi(directory = directory), NA)
  
  # Return error with incorrect arguments
  expect_error(read_wi(directory = "not_a_directory"))
  expect_error(read_wi(directory = ".")) # empty directory
  expect_error(read_wi(directory = directory, rightsHolder = 1)) # wrong rightsHolder
  expect_error(read_wi(directory = directory, coordinateUncertaintyInMeters = "no_a_number"))
  expect_error(read_wi(directory = directory, captureMethod = "no_a_captureMethod"))
})

test_that("Check returned list", {
  dp <- read_wi(directory = directory)
  expect_true(is.list(dp))
  expect_equal(length(dp), 18)
  expect_true("data" %in% names(dp))
  expect_equal(length(dp$data), 3)
})