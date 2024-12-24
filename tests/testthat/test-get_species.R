test_that("get_species() is deprecated", {
  x <- example_dataset()
  lifecycle::expect_deprecated(get_species(x),
                               regex = "was deprecated in camtraptor 1.0.0.")
})
