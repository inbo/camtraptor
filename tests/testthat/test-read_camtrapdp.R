test_that("read_camtrap_dp is deprecated, but same result is returned", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  expect_warning(
    read_camtrap_dp(file),
    glue::glue("'read_camtrap_dp' is deprecated.",
               "Use 'read_camtrapdp' instead.",
               "See help(\"Deprecated\") and help(\"camtraptor-deprecated\").",
               .sep = "\n"),
    fixed = TRUE
  )
  expect_identical(
    suppressWarnings(read_camtrap_dp(file)),
    read_camtrapdp(file)
  )
})
