test_that("get_n_obs returns the right structure of dataframe", {

  output_anas_platyrhyncos <- get_n_obs(camtrapdp, "Anas platyrhynchos")

  # type list
  expect_type(get_n_obs(camtrapdp, "Anas platyrhynchos"), "list")

  # class tibble data.frame
  expect_equal(class(get_n_obs(camtrapdp, "Anas platyrhynchos")),
               c("tbl_df", "tbl", "data.frame"))

  # columns deployment_id and rai only
  expect_equal(names(output_anas_platyrhyncos),
               c("deployment_id",
                 "n"))
})

test_that("species is case insensitive", {
  expect_equal(get_n_obs(camtrapdp, "Anas platyrhynchos"),
               get_n_obs(camtrapdp, toupper("Anas platyrhynchos")))
})

test_that("if a species is selected, less observations are returned", {

  output_all_species <- get_n_obs(camtrapdp)
  output_anas_platyrhyncos <- get_n_obs(camtrapdp, "Anas platyrhynchos")
  expect_true(all(output_all_species$n >=  output_anas_platyrhyncos$n))
})
