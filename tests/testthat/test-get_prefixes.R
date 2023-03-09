test_that("get_prefixes() returns a tibble", {
  expect_s3_class(get_prefixes('n_species', c("n")),
                  c("tbl_df", "tbl", "data.frame"))
})

test_that("get_prefixes() returns an error if the info we want is not valid", {
  # species is a prefix, not an info. It should be scientificName
  hover_columns <- c("species", "deploymentID")
  expect_error(get_prefixes("rai", hover_columns))
  })
  
test_that("get_prefixes() returns prefix for every info we want to show", {
  hover_columns <- c(
    "n",
    "scientificName",
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
    as.integer(length(hover_columns)))
  
})
