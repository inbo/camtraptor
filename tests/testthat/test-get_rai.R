test_that("get_rai returns the right dataframe", {

  output_anas_platyrhyncos <- get_rai(camtrapdp, "Anas platyrhynchos")

  # type list
  expect_type(output_anas_platyrhyncos, "list")

  # class tibble data.frame
  expect_equal(class(output_anas_platyrhyncos),
               c("tbl_df", "tbl", "data.frame"))

  # columns deployment_id and rai only
  expect_equal(names(output_anas_platyrhyncos),
               c("deployment_id",
                 "rai"))
})

test_that("species is case insensitive", {

  expect_equal(get_rai(camtrapdp, "Anas platyrhynchos"),
               get_rai(camtrapdp, toupper("Anas platyrhynchos")))
})
