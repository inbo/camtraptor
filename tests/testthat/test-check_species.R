test_that("Error is returned if species is NULL or of length 0", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(check_species(x, NULL))
  expect_error(check_species(x, character(0)))
})

test_that("Error is returned if one or more species are invalid", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    check_species(
      x,
      c(
        "beech marten",
        "Ans streperi", # wrong
        "blauwe reiger",
        "Anas strepera",
        "bad name" # wrong
      )
    ),
    paste0(
      "Invalid value for species parameter: ans streperi and bad name.\n",
      "Valid inputs are: anas platyrhynchos, anas strepera, ardea, ardea ",
      "cinerea, castor fiber, homo sapiens, martes foina, mustela putorius, ",
      "vulpes vulpes, mallard, gadwall, great herons, grey heron, eurasian ",
      "beaver, human, beech marten, european polecat, red fox, wilde eend, ",
      "krakeend and others..."
    ),
    fixed = TRUE
  )
})

test_that("If input is a scientific name, the result is equal to input", {
  skip_if_offline()
  x <- example_dataset()
  sc_name <- "Anas strepera"
  species <- check_species(x, sc_name)
  expect_equal(species, sc_name)
})

test_that("Multiput scientific names are allowed", {
  skip_if_offline()
  x <- example_dataset()
  sc_names <- c("Anas strepera", "Ardea cinerea")
  species <- check_species(x, sc_names)
  expect_equal(species, sc_names)
})

test_that("Function works with vernacular names", {
  skip_if_offline()
  x <- example_dataset()
  vn_names <- c("beech marten", "mallard")
  species <- suppressMessages(check_species(x, vn_names))
  expect_equal(species, c("Martes foina", "Anas platyrhynchos"))
})

test_that("Functions works well with vernacular names of different languages", {
  skip_if_offline()
  x <- example_dataset()
  vn_names <- c("beech marten", "wilde eend")
  species <- suppressMessages(check_species(x, vn_names))
  expect_equal(species, c("Martes foina", "Anas platyrhynchos"))
})

test_that("Functions works with a mix of scientific and vernacular names", {
  skip_if_offline()
  x <- example_dataset()
  mixed_names <- c("mallard", "steenmarter", "Castor fiber")
  species <- suppressMessages(check_species(x, mixed_names))
  expect_equal(
    species,
    c(
      "Anas platyrhynchos",
      "Martes foina",
      "Castor fiber"
    )
  )
})

test_that("Taxon IDs are not allowed", {
  skip_if_offline()
  x <- example_dataset()
  taxon_id <- x$taxonomic[[1]]$taxonID
  expect_error(check_species(x, taxon_id))
})

test_that("Functions works case insensitively", {
  skip_if_offline()
  x <- example_dataset()
  vn_name <- suppressMessages(check_species(x, c("MallARD")))
  species <- check_species(x, vn_name)
  expect_equal(species, "Anas platyrhynchos")
})
