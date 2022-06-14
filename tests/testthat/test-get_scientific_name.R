test_that("If a vernacular names is not found, error arises", {
  expect_error(get_scientific_name(mica, c("mallard", "quaqua")))
  # a scientific name is also an invalid input for vernacular_name
  expect_error(get_scientific_name(mica, "Anas platyrhynchos"))
})

test_that("Functions works well with multiple vernacular names", {
  sc_names <- get_scientific_name(mica, c("beech marten", "mallard"))
  expect_equal(sc_names, c("Martes foina", "Anas platyrhynchos"))
})

test_that("Functions works well with vernacular names of different languages", {
  sc_names <- get_scientific_name(mica, c("beech marten", "wilde eend"))
  expect_equal(sc_names, c("Martes foina", "Anas platyrhynchos"))
})

test_that("Functions works case insensitively", {
  sc_names <- get_scientific_name(mica, c("beeCH MArten"))
  expect_equal(sc_names, c("Martes foina"))
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_scientific_name(datapkg = mica, vernacular_name = "beech marten")
    )
  )
})
