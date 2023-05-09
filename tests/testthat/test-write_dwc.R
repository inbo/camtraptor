test_that("write_dwc() can write csv files to a path", {
  out_dir <- file.path(tempdir(), "dwc")
  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir)
  suppressMessages(write_dwc(mica, directory = out_dir))

  expect_identical(
    list.files(out_dir, pattern = "*.csv"),
    c("dwc_audubon.csv", "dwc_occurrence.csv")
  )
})

test_that("write_dwc() can return data as list of tibbles rather than files", {
  result <- suppressMessages(write_dwc(mica, directory = NULL))

  expect_identical(names(result), c("dwc_occurrence", "dwc_audubon"))
  expect_s3_class(result$dwc_occurrence, "tbl")
  expect_s3_class(result$dwc_audubon, "tbl")
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
      "providerManagedID",
      "comments",
      "captureDevice",
      "resourceCreationTechnique",
      "accessURI",
      "format",
      "CreateDate"
    )
  )
})

# Use snapshots to compare output files
## helpers to output file paths

write_dwc_occ <- function(package, directory = ".") {
  suppressMessages(write_dwc(package, directory))
  return(file.path(directory, "dwc_occurrence.csv"))
}

write_dwc_media <- function(package, directory = ".") {
  suppressMessages(write_dwc(package, directory))
  return(file.path(directory, "dwc_audubon.csv"))
}

test_that("write_dwc() generates the right files from a known package", {
  out_dir <- file.path(tempdir(), "dwc")
  expect_snapshot_file(write_dwc_occ(mica,out_dir))
  expect_snapshot_file(write_dwc_media(mica,out_dir))
  unlink(out_dir, recursive = TRUE)
})
