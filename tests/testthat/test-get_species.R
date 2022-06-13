test_that("right (number of) species", {
  expect_identical(
    get_species(mica),
    dplyr::tibble(
      taxonID = map_chr(mica$taxonomic, ~.[["taxonID"]]),
      taxonIDReference = map_chr(
        mica$taxonomic, ~.[["taxonIDReference"]]
      ),
      scientificName = map_chr(
        mica$taxonomic, ~.[["scientificName"]]
      ),
      vernacularNames.en = map_chr(
        mica$taxonomic, ~.[["vernacularNames"]][["en"]]
      ),
      vernacularNames.nl = map_chr(
        mica$taxonomic, ~.[["vernacularNames"]][["nl"]]
      )
    )
  )
})

test_that("function works fine with missing vernacular name slots", {
  taxonomy <- list(
    list(scientificName = "Martes foina",
         vernacularNames = list(en = "aa", nl = "bb")),
    # missing Dutch vernacular name
    list(scientificName = "Ardea cinerea",
         vernacularNames = list(en = "cc")),
    # missing vernacular names
    list(scientificName = "Anas strepera", vernacularNames = list())
  )
  mica_modified <- mica
  mica_modified$taxonomic <- taxonomy
  species_df <- get_species(mica_modified)
  # number of rows = number of species
  expect_equal(nrow(species_df), length(mica_modified$taxonomic))
  # number of columns = number of slots of species list + vernacular names - 1
  expect_equal(
    ncol(species_df),
    length(taxonomy[[1]]) + length(taxonomy[[1]]$vernacularNames) - 1
  )
  # column names
  expect_equal(
    names(species_df),
    c("scientificName",
      paste("vernacularNames", names(taxonomy[[1]]$vernacularNames),sep = ".")
    )
  )
  # empty slots are converted to NA
  # Dutch vernacular name of Ardea cinerea not present
  expect_true(
    is.na(species_df %>%
            dplyr::filter(scientificName == "Ardea cinerea") %>%
            dplyr::pull(vernacularNames.nl))
  )
  # English vernacular name of Anas strepera not present
  expect_true(
    is.na(species_df %>%
            dplyr::filter(scientificName == "Anas strepera") %>%
            dplyr::pull(vernacularNames.en))
  )
  # Dutch vernacular name of Anas strepera not present
  expect_true(
    is.na(species_df %>%
            dplyr::filter(scientificName == "Anas strepera") %>%
            dplyr::pull(vernacularNames.nl))
  )
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_species(datapkg = mica)
    )
  )
})
