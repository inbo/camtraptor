test_that("read_wi() returns error when files are not found", {
  expect_error(
    read_wi("."),
    "Path './projects.csv' does not exist"
  )
})

test_that("read_wi() returns error on invalid capture_method", {
  expect_error(
    read_wi("data", capture_method = "not_a_capture_method"),
    "`capture_method` must be `motion detection` and/or `time lapse`"
  )
  expect_error(
    read_wi("data", capture_method = c("time lapse", "not_a_capture_method")),
    "`capture_method` must be `motion detection` and/or `time lapse`"
  )
})

test_that("read_wi() returns a valid package", {
  expect_type(camtraptor:::check_package(read_wi("data")), "list")
})
