# test_that("write_eml() can write csv files to a path", {
#   out_dir <- file.path(tempdir(), "eml")
#   unlink(out_dir, recursive = TRUE)
#   dir.create(out_dir)
#   suppressMessages(write_eml(mica, directory = out_dir, title = "MICA"))
# })
# 

test_that("write_eml() can write an eml", {
  # check for any errors
  expect_no_error(suppressMessages(write_eml(
    mica,
    title = "The Mica EML", directory = NULL
  )))
  # compare against known good result
  eml <-
    suppressMessages(write_eml(mica, title = "a valid title", directory = NULL))
  ## don't compare the packageId because it's a random guid
  purrr::pluck(eml, "packageId") <- NULL
  expect_snapshot(eml)
  
  # write to temp dir
  expect_no_error(write_eml(mica, title = "mica title", directory = tempdir()))
  ## read from file, and remove packageID because it's a random guid
  eml_from_file <- EML::read_eml(file.path(tempdir(),"eml.xml"))
  purrr::pluck(eml_from_file,"packageId") <- NULL
  ## compare to known output
  expect_snapshot(eml_from_file)
  ## remove temp file
  unlink(file.path(tempdir(),"eml.xml"))
})

test_that("write_eml() checks for title", {
  expect_error(write_eml(mica),
               regexp = "The dataset must have a `title`.",
               fixed = TRUE)
})

test_that("write_eml() checks for keywords", {
  expect_error(write_eml(mica, title = "mica title", keywords = NULL),
               regexp = "`keywords` should be a character (vector).",
               fixed = TRUE)
})

test_that("write_eml() notifies to check metadata", {
  expect_message(
    write_eml(mica, title = "mica title", directory = NULL),
    regexp = "Please review generated metadata carefully before publishing.",
    fixed = TRUE)
})
