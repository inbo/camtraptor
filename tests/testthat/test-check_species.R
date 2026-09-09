test_that("check_species is defunct", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_defunct(check_species(x))
})
