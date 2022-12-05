
test_that("can write csv files to a path", {
  out_dir <- file.path(tempdir(), "dwc")
  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir)

  write_dwc(mica, directory = out_dir)

  expect_equal(
    list.files(out_dir, pattern = "*.csv"),
    c("dwc_audubon.csv", "dwc_occurrence.csv")
  )
})

test_that("if directory is NULL, a named list should be returned", {
  expect_length(
    write_dwc(mica, directory = NULL),
    2
  )

  expect_equal(
    names(write_dwc(mica, directory = NULL)),
    c("dwc_occurrence", "dwc_audubon")
  )
})

test_that("function returns the expected columns in the outputs", {
  named_list <- write_dwc(mica, directory = NULL)

  expected_occ_columns <-
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

  expected_audubon_columns <-
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

  expect_equal(
    colnames(named_list$dwc_occurrence),
    expected_occ_columns
  )

  expect_equal(
    colnames(named_list$dwc_audubon),
    expected_audubon_columns
  )
})
