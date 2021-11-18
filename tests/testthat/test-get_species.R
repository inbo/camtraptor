test_that("right (number of) species", {
  expect_identical(
    get_species(mica),
    tibble(
      taxonID = map_chr(mica$datapackage$taxonomic, ~.[["taxonID"]]),
      taxonIDReference = map_chr(
        mica$datapackage$taxonomic, ~.[["taxonIDReference"]]
      ),
      scientificName = map_chr(
        mica$datapackage$taxonomic, ~.[["scientificName"]]
      ),
      vernacularNames.en = map_chr(
        mica$datapackage$taxonomic, ~.[["vernacularNames"]][["en"]]
      ),
      vernacularNames.nl = map_chr(
        mica$datapackage$taxonomic, ~.[["vernacularNames"]][["nl"]]
      )
    )
  )
})
