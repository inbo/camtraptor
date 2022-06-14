test_that("Error is returned if species is NULL or of length 0", {
  expect_error(check_species(mica, NULL))
  expect_error(check_species(mica, character(0)))
})

test_that("If input is a scientific name, the result is equal to input", {
  sc_name <- "Anas strepera"
  species <- check_species(mica, sc_name)
  expect_equal(species, sc_name)
})

test_that("Multiput scientific names are allowed", {
  sc_names <- c("Anas strepera", "Ardea cinerea")
  species <- check_species(mica, sc_names)
  expect_equal(species, sc_names)
})

test_that("Function works with vernacular names", {
  vn_names <- c("beech marten", "mallard")
  species <- check_species(mica, vn_names)
  expect_equal(species, c("Martes foina", "Anas platyrhynchos"))
})

test_that("Functions works well with vernacular names of different languages", {
  vn_names <- c("beech marten", "wilde eend")
  species <- check_species(mica, vn_names)
  expect_equal(species, c("Martes foina", "Anas platyrhynchos"))
})

test_that("Functions works with a mix of scientific and vernacular names", {
  mixed_names <- c("mallard", "steenmarter", "Castor fiber")
  species <- check_species(mica, mixed_names)
  expect_equal(species,
               c("Anas platyrhynchos",
                 "Martes foina",
                 "Castor fiber"))
})

test_that("Taxon IDs are not allowed", {
  taxon_id <- mica$taxonomic[[1]]$taxonID
  expect_error(check_species(mica, taxon_id))
})

test_that("Functions works case insensitively", {
  vn_name <- check_species(mica, c("MallARD"))
  species <-  check_species(mica, vn_name)
  expect_equal(species, "Anas platyrhynchos")
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      check_species(datapkg = mica, species = "Anas strepera")
    )
  )
})
