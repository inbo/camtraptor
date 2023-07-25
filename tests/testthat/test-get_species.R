test_that("right (number of) species", {
  expect_identical(
    get_species(mica),
    dplyr::tibble(
      taxonID = purrr::map_chr(mica$taxonomic, ~ .[["taxonID"]]),
      taxonIDReference = purrr::map_chr(
        mica$taxonomic, ~ .[["taxonIDReference"]]
      ),
      scientificName = purrr::map_chr(
        mica$taxonomic, ~ .[["scientificName"]]
      ),
      vernacularNames.en = purrr::map_chr(
        mica$taxonomic, ~ .[["vernacularNames"]][["en"]]
      ),
      vernacularNames.nl = purrr::map_chr(
        mica$taxonomic, ~ .[["vernacularNames"]][["nl"]]
      )
    )
  )
})

test_that("function works fine with missing vernacular name slots", {
  taxonomy <- list(
    list(
      scientificName = "Martes foina",
      vernacularNames = list(en = "aa", nl = "bb")
    ),
    # missing Dutch vernacular name
    list(
      scientificName = "Ardea cinerea",
      vernacularNames = list(en = "cc")
    ),
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
    c(
      "scientificName",
      paste("vernacularNames", names(taxonomy[[1]]$vernacularNames), sep = ".")
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
    ),
    paste0("\033[38;5;232mThe `datapkg` argument of `get_species()` is ",
           "deprecated as of camtraptor 0.16.0.\n\033[36mℹ\033[38;5;232m Please",
           " use the `package` argument instead.\n\033[36mℹ\033[38;5;232m The",
           " deprecated feature was likely used in the ",
           "\033[34mcamtraptor\033[38;5;232m package.\n  ",
           "Please report the issue at \033[3m\033[34m<\033]8;;",
           "https://github.com/inbo/camtraptor/issues\a",
           "https://github.com/inbo/camtraptor/issues\033]8;;",
           "\a>\033[38;5;232m\033[23m.\n\033[90mThis warning is displayed ",
           "once every 8 hours.\033[38;5;232m\n\033[90mCall ",
           "`lifecycle::last_lifecycle_warnings()` to see where this warning ",
           "was generated.\033[38;5;232m\033[39m"
    ),
    fixed = TRUE
  )
})
