import_directory <- "wildlife-insights_export_sample/"

test_that("file is checked properly", {
  write_dwc_wi(import_directory = import_directory, export_directory = tempdir())
})
