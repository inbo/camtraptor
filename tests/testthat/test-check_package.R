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


test_that("check_package() returns TRUE on valid package", {
  expect_true(check_package(mica))
})
