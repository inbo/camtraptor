test_that("read_camtrapdp() reads a Camtrap DP", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  expect_identical(read_camtrapdp(file), camtrapdp::read_camtrapdp(file))
})


test_that("read_camtrap_dp is deprecated", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  expect_warning(read_camtrap_dp(file))
})
