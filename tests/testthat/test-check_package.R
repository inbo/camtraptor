test_that("check_package() returns depreciation warning on datapkg argument", {
  expect_warning(
    check_package(datapkg = mica, function_name = "function_name_here"),
    regexp = "The `datapkg` argument of `function_name_here()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})

test_that("check_package() returns error when package is not a list", {
  expect_error(
    check_package("not a list!"),
    regexp = "package is not a list",
    fixed = TRUE
  )
  expect_error(
    check_package(data.frame(letters = c("a","b","c"), numbers = c(pi,2*pi,3*pi))),
    regexp = "package is not a list",
    fixed = TRUE
  )
})

test_that("check_package() returns error on missing data", {
  expect_error(
    check_package(purrr::discard_at(mica,at = "data")),
    regexp = "data element is missing from package",
    fixed = TRUE
  )
})

test_that("check_package() returns error if not all elements are present", {
  mica_no_media <- mica
  mica_no_media$data$media <- NULL
  expect_error(
    check_package(mica_no_media),
    regexp = "Can't find 1 elements in data package: media",
    fixed = TRUE
  )
  mica_no_media_no_obs <- mica_no_media
  mica_no_media_no_obs$data$observations <- NULL
  expect_error(
    check_package(mica_no_media_no_obs),
    regexp = "Can't find 2 elements in data package: media and observations",
    fixed = TRUE
  )
})

test_that("check_package() returns error if observations is not a data.frame", {
  mica_listed <- mica
  mica_listed$data$observations <- as.list(mica_listed$data$observations)
  
  expect_error(
    check_package(mica_listed),
    regexp = "package$data$observations is not a data frame",
    fixed = TRUE
  )
})

test_that("check_package() returns error if deployments is not a data.frame", {
  mica_listed <- mica
  mica_listed$data$deployments <- as.list(mica_listed$data$deployments)
  expect_error(
    check_package(mica_listed),
    regexp = "package$data$deployments is not a data frame",
    fixed = TRUE
  )
})

test_that("check_package() doesn't return an error on a NULL media object", {
  # the case when media is not imported
  mica_null_media <- mica
  mica_null_media$data["media"] <- list(NULL)
  expect_true(check_package(mica_null_media))
})

test_that("check_package() returns TRUE on valid package", {
  expect_true(check_package(mica))
})
