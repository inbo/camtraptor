test_that("get_species() is deprecated", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_deprecated(get_species(x),
                               regex = "was deprecated in camtraptor 1.0.0.")
})
