test_that("read_camtrap_dp is deprecated, but same result is returned", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  lifecycle::expect_deprecated(
    read_camtrap_dp(file),
    regex = "was deprecated in camtraptor 1.0.0."
  )
  expect_identical(
    suppressWarnings(read_camtrap_dp(file)),
    read_camtrapdp(file)
  )
})
