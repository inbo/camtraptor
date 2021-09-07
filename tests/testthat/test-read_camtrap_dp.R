library(here) # to write paths easily

test_that("path is checked properly", {
  expect_error(read_camtrap_dp("aaa"))
  expect_error(read_camtrap_dp(1))
})

test_that("multimedia is checked properly", {
  dp_path <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
  expect_error(read_camtrap_dp(
    path = dp_path,
    multimedia = "must_Be_a_logical!")
  )
})

test_that("output is a list", {
  dp_path <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
  dp_without_multimedia <- read_camtrap_dp(
    path = dp_path,
    multimedia = FALSE)
  assert_that(class(dp_without_multimedia) == "list")
})

test_that("output is a list of length 4", {
  dp_path <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
  dp_without_multimedia <- read_camtrap_dp(
    path = dp_path,
    multimedia = FALSE)
  expect_equal(class(dp_without_multimedia), "list")
  expect_equal(length(dp_without_multimedia), 4)
})

test_that("multimedia arg influences only slot multimedia", {
  dp_path <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
  dp_with_multimedia <- read_camtrap_dp(
    path = dp_path,
    multimedia = TRUE)
  dp_without_multimedia <- read_camtrap_dp(
    path = dp_path,
    multimedia = FALSE)
  # multimedia is NULL only for dp_without_multimedia
  expect_null(dp_without_multimedia$multimedia)
  expect_false(is.null(dp_with_multimedia$multimedia))
  # metadata are the same
  expect_identical(dp_with_multimedia$metadata,
                        dp_without_multimedia$metadata)
  # observations are the same
  expect_identical(
    # remove columns with NA only 
    dp_with_multimedia$observations[colSums(!is.na(dp_with_multimedia$observations)) > 0],
    # remove columns with NA only 
    dp_without_multimedia$observations[colSums(!is.na(dp_without_multimedia$observations)) > 0]
  )
  # deployments are the same
  expect_identical(
    # remove columns with NA only 
    dp_with_multimedia$deployments[colSums(!is.na(dp_with_multimedia$deployments)) > 0],
    # remove columns with NA only 
    dp_with_multimedia$deployments[colSums(!is.na(dp_with_multimedia$deployments)) > 0]
  )
})

test_that("Datapackage metadata is a list", {
  dp_path <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
  dp_without_multimedia <- read_camtrap_dp(
    path = dp_path,
    multimedia = FALSE)
 expect_equal(class(dp_without_multimedia$metadata), "list")
})

test_that("Datapackage resources are named as in metadata$resource_names", {
  dp_path <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
  dp_without_multimedia <- read_camtrap_dp(
    path = dp_path,
    multimedia = FALSE)
  resource_names <- dp_without_multimedia$metadata$resource_names
  expect_true(all(resource_names %in% names(dp_without_multimedia)))
})

test_that("Datapackage resources are tibble dataframes", {
  dp_path <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
  dp_without_multimedia <- read_camtrap_dp(
    path = dp_path,
    multimedia = FALSE)
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% 
                class(dp_without_multimedia$deployments)))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% 
                class(dp_without_multimedia$deployments)))
})
