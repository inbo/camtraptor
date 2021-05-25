test_that("camtrap dp data are structured correctly", {
  mica <- read_camtrap_dp(
    "./inst/extdata/mica-muskrat-and-coypu-20210302172233"
  )
  # slots are correctly named
  expect_true(all(names(mica) == c("datapackage",
                                   "deployments",
                                   "multimedia",
                                   "observations"))
  )
  # datapackage is a list
  expect_true(is.list(mica$datapackage))
  # deployments is a df
  expect_true(is.data.frame(mica$deployments))
  # multimedia is a df
  expect_true(is.data.frame(mica$multimedia))
  # observations is a df
  expect_true(is.data.frame(mica$observations))
})

test_that("multimedia are not loaded if flag multimedia is set to FALSE", {
  # load data (multimedia is TRUE by default)
  mica <- read_camtrap_dp(
    "./inst/extdata/mica-muskrat-and-coypu-20210302172233",
    multimedia = FALSE
  )
  expect_null(mica$multimedia)
})
