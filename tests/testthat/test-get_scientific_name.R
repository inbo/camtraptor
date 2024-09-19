test_that("If a vernacular names is not found, error arises", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(get_scientific_name(x, c("mallard", "quaqua")))
  # a scientific name is also an invalid input for vernacular_name
  expect_error(get_scientific_name(x, "Anas platyrhynchos"))
})

test_that("Functions works well with multiple vernacular names", {
  skip_if_offline()
  x <- example_dataset()
  sc_names <- get_scientific_name(x, c("beech marten", "mallard"))
  expect_equal(sc_names, c("Martes foina", "Anas platyrhynchos"))
})

test_that("Functions works well with vernacular names of different languages", {
  skip_if_offline()
  x <- example_dataset()
  sc_names <- get_scientific_name(x, c("beech marten", "wilde eend"))
  expect_equal(sc_names, c("Martes foina", "Anas platyrhynchos"))
})

test_that("Functions works case insensitively", {
  skip_if_offline()
  x <- example_dataset()
  sc_names <- get_scientific_name(x, c("beeCH MArten"))
  expect_equal(sc_names, c("Martes foina"))
})

