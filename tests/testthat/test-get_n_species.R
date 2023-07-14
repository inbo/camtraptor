test_that("get_n_species returns the right dataframe", {
  output_get_n_species <- get_n_species(mica)

  # type list
  expect_type(output_get_n_species, "list")

  # class tibble data.frame
  expect_equal(
    class(output_get_n_species),
    c("tbl_df", "tbl", "data.frame")
  )

  # columns deploymentID and rai only
  expect_equal(
    names(output_get_n_species),
    c(
      "deploymentID",
      "n"
    )
  )
})
