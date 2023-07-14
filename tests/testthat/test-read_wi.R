test_that("read_wi() returns error when files are not found", {
  expect_error(
    read_wi("."),
    "Path './projects.csv' does not exist"
  )
})

test_that("read_wi() returns a valid package", {
  expect_true(camtraptor:::check_package(read_wi("data")))
})
