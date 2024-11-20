test_that("get_scientific_name() is defunct", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_defunct(get_scientific_name(x))
})