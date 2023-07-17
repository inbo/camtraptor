test_that("write_dwc() write csv files to a path", {
  out_dir <- file.path(tempdir(), "dwc")
  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir)
  suppressMessages(write_dwc(mica, directory = out_dir))

  expect_identical(
    list.files(out_dir, pattern = "*.csv"),
    c("dwc_audubon.csv", "dwc_occurrence.csv")
  )
  unlink(out_dir, recursive = TRUE)
})

test_that("write_dwc() can return data as list of tibbles rather than files", {
  result <- suppressMessages(write_dwc(mica, directory = NULL))

  expect_identical(names(result), c("dwc_occurrence", "dwc_audubon"))
  expect_s3_class(result$dwc_occurrence, "tbl")
  expect_s3_class(result$dwc_audubon, "tbl")
  # meta.xml is not included
})

test_that("write_dwc() writes the expected meta.xml", {
  out_dir <- file.path(tempdir(), "dwc")
  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir)
  suppressMessages(write_dwc(mica, directory = out_dir))
  
  expect_true("meta.xml" %in% list.files(out_dir))
  expect_snapshot_file(file.path(out_dir, "meta.xml"))
  unlink(out_dir, recursive = TRUE)
})

test_that("write_dwc() returns the expected Darwin Core terms as columns", {
  result <- suppressMessages(write_dwc(mica, directory = NULL))

  expect_identical(
    colnames(result$dwc_occurrence),
    c(
      "type",
      "license",
      "rightsHolder",
      "datasetID",
      "collectionCode",
      "datasetName",
      "basisOfRecord",
      "dataGeneralizations",
      "occurrenceID",
      "individualCount",
      "sex",
      "lifeStage",
      "behavior",
      "occurrenceStatus",
      "occurrenceRemarks",
      "organismID",
      "eventID",
      "parentEventID",
      "eventDate",
      "habitat",
      "samplingProtocol",
      "samplingEffort",
      "eventRemarks",
      "locationID",
      "locality",
      "decimalLatitude",
      "decimalLongitude",
      "geodeticDatum",
      "coordinateUncertaintyInMeters",
      "coordinatePrecision",
      "identifiedBy",
      "dateIdentified",
      "identificationRemarks",
      "taxonID",
      "scientificName",
      "kingdom"
    )
  )
  expect_identical(
    colnames(result$dwc_audubon),
    c(
      "occurrenceID",
      "dcterm:rights",
      "identifier",
      "dc:type",
      "comments",
      "captureDevice",
      "resourceCreationTechnique",
      "accessURI",
      "format",
      "CreateDate"
    )
  )
})

test_that("write_dwc() returns the expected Darwin Core mapping for a known dataset", {
  out_dir <- file.path(tempdir(), "dwc")
  unlink(out_dir, recursive = TRUE)
  
  # Use helper function that outputs path write_dwc() wrote to.  
  expect_snapshot_file(write_dwc_snapshot(mica, out_dir, "occurrence"))
  expect_snapshot_file(write_dwc_snapshot(mica, out_dir, "audubon"))
  unlink(out_dir, recursive = TRUE)
})

test_that("write_dwc() returns files that comply with the info in meta.xml", {
  out_dir <- file.path(tempdir(), "dwc")
  unlink(out_dir, recursive = TRUE)
  suppressMessages(write_dwc(mica, out_dir))
  
  expect_fields(file.path(out_dir,"dwc_occurrence.csv"))
  expect_fields(file.path(out_dir,"dwc_audubon.csv"))
  unlink(out_dir, recursive = TRUE)
})
