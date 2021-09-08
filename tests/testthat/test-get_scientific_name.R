test_that("If a vernacular names is not found, NA is returned", {
  sc_names <- get_scientific_name(camtrapdp, c("brown rat", "quaqua"))
  expect_equal(sc_names, c("Rattus norvegicus", NA_character_))
})

test_that("Functions works well with multiple vernacular names", {
  sc_names <- get_scientific_name(camtrapdp, c("brown rat", "mallard"))
  expect_equal(sc_names, c("Rattus norvegicus", "Anas platyrhynchos"))
})

test_that("Functions works well with vernacular names of different languages", {
  sc_names <- get_scientific_name(camtrapdp, c("brown rat", "wilde eend"))
  expect_equal(sc_names, c("Rattus norvegicus", "Anas platyrhynchos"))
})

test_that("Functions works case insensitively", {
  sc_names <- get_scientific_name(camtrapdp, c("BrOwN RaT"))
  expect_equal(sc_names, c("Rattus norvegicus"))
})

test_that("scientific names are returned in the case correct form if passed", {
  sc_names <- get_scientific_name(camtrapdp, c("Rattus NORVEGICUS"))
  expect_equal(sc_names, c("Rattus norvegicus"))
})
