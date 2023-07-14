test_that("check_package() returns depreciation warning on datapkg argument", {
  expect_warning(
    check_package(datapkg = mica, function_name = "function_name_here"),
    regexp = "The `datapkg` argument of `function_name_here()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})


test_that("check_package() returns TRUE on valid package", {
  expect_true(check_package(mica))
})
