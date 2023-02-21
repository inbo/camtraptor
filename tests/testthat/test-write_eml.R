# test_that("write_eml() can write csv files to a path", {
#   out_dir <- file.path(tempdir(), "eml")
#   unlink(out_dir, recursive = TRUE)
#   dir.create(out_dir)
#   suppressMessages(write_eml(mica, directory = out_dir, title = "MICA"))
# })
# 

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