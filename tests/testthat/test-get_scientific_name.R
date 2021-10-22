test_that("If a vernacular names is not found, error arises", {
  expect_error(get_scientific_name(mica, c("brown rat", "quaqua")))
  # a scientific name is also an invalid input for vernacular_name
  expect_error(get_scientific_name(mica, "Rattus norvegicus"))
})

test_that("Functions works well with multiple vernacular names", {
  sc_names <- get_scientific_name(mica, c("brown rat", "mallard"))
  expect_equal(sc_names, c("Rattus norvegicus", "Anas platyrhynchos"))
})

test_that("Functions works well with vernacular names of different languages", {
  sc_names <- get_scientific_name(mica, c("brown rat", "wilde eend"))
  expect_equal(sc_names, c("Rattus norvegicus", "Anas platyrhynchos"))
})

test_that("Functions works case insensitively", {
  sc_names <- get_scientific_name(mica, c("BrOwN RaT"))
  expect_equal(sc_names, c("Rattus norvegicus"))
})
