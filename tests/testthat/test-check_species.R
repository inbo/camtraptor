test_that("Error is returned if species is NULL or of length 0", {
  expect_error(check_species(camtrapdp, NULL))
  expect_error(check_species(camtrapdp, character(0)))
})

test_that("If input is a scientific name, the result is equal to input", {
  sc_name <- "Gallinula chloropus"
  species <- check_species(camtrapdp, sc_name)
  expect_equal(species, sc_name)
})

test_that("Multiput scientific names are allowed", {
  sc_names <- c("Rattus norvegicus", "Anas platyrhynchos")
  species <- check_species(camtrapdp, sc_names)
  expect_equal(species, sc_names)
})

test_that("Function works with vernacular names", {
  vn_names <- c("brown rat", "mallard")
  species <- check_species(camtrapdp, vn_names)
  expect_equal(species, c("Rattus norvegicus", "Anas platyrhynchos"))
})

test_that("Functions works well with vernacular names of different languages", {
  vn_names <- c("brown rat", "wilde eend")
  species <- check_species(camtrapdp, vn_names)
  expect_equal(species, c("Rattus norvegicus", "Anas platyrhynchos"))
})

test_that("Functions works with a mix of scientific and vernacular names", {
  mixed_names <- c("mallard", "bruine rat", "Ondatra zibethicus")
  species <- check_species(camtrapdp, mixed_names)
  expect_equal(species,
               c("Anas platyrhynchos",
                 "Rattus norvegicus",
                 "Ondatra zibethicus"))
})

test_that("Taxon IDs are not allowed", {
  taxon_id <- camtrapdp$datapackage$taxonomic[[1]]$taxon_id
  expect_error(check_species(camtrapdp, taxon_id))
})

test_that("Functions works case insensitively", {
  vn_name <- check_species(camtrapdp, c("BrOwN RaT"))
  species <-  check_species(camtrapdp, vn_name)
  expect_equal(species, "Rattus norvegicus")
})
