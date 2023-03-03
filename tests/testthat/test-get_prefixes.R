test_that("get_prefixes() returns a tibble", {
  expect_s3_class(get_prefixes('n_species', c("n")),
                  c("tbl_df", "tbl", "data.frame"))
})

test_that("get_prefixes() returns prefix for every ", {
  hover_columns <- c(
    "n",
    "species",
    "deploymentID",
    "locationID",
    "locationName",
    "latitude",
    "longitude",
    "start",
    "end"
  )
  
  expect_identical(
    nrow(get_prefixes("rai",hover_columns)),
    as.integer(length(hover_columns) - 1))
  
})
