test_that("path is checked properly", {
  expect_error(read_camtrap_dp("aaa"))
  expect_error(read_camtrap_dp(1))
})

test_that("media is checked properly", {
  dp_path <- system.file("extdata", "mica", package = "camtrapdp")
  expect_error(read_camtrap_dp(
    path = dp_path,
    media = "must_be_a_logical!")
  )
})

test_that("output is a list of length 4", {
  dp_path <- system.file("extdata", "mica", package = "camtrapdp")
  dp_without_media <- read_camtrap_dp(
    path = dp_path,
    media = FALSE)
  expect_equal(class(dp_without_media), "list")
  expect_equal(length(dp_without_media), 4)
})

test_that("media arg influences only slot media", {
  dp_path <- system.file("extdata", "mica", package = "camtrapdp")
  dp_with_media <- read_camtrap_dp(
    path = dp_path,
    media = TRUE)
  dp_without_media <- read_camtrap_dp(
    path = dp_path,
    media = FALSE)
  # media is NULL only for dp_without_media
  expect_null(dp_without_media$media)
  expect_false(is.null(dp_with_media$media))
  # metadata are the same
  expect_identical(dp_with_media$metadata,
                        dp_without_media$metadata)
  # observations are the same
  expect_identical(
    # remove columns with NA only
    dp_with_media$observations[colSums(!is.na(dp_with_media$observations)) > 0],
    # remove columns with NA only
    dp_without_media$observations[colSums(!is.na(dp_without_media$observations)) > 0]
  )
  # deployments are the same
  expect_identical(
    # remove columns with NA only
    dp_with_media$deployments[colSums(!is.na(dp_with_media$deployments)) > 0],
    # remove columns with NA only
    dp_with_media$deployments[colSums(!is.na(dp_with_media$deployments)) > 0]
  )
})

test_that("Datapackage metadata is a list", {
  dp_path <- system.file("extdata", "mica", package = "camtrapdp")
  dp_without_media <- read_camtrap_dp(
    path = dp_path,
    media = FALSE)
 expect_equal(class(dp_without_media$datapackage), "list")
})

test_that("Datapackage resources are named as in metadata$resource_names", {
  dp_path <- system.file("extdata", "mica", package = "camtrapdp")
  dp_without_media <- read_camtrap_dp(
    path = dp_path,
    media = FALSE)
  resource_names <- dp_without_media$metadata$resource_names
  expect_true(all(resource_names %in% names(dp_without_media)))
})

test_that("Datapackage resources are tibble dataframes", {
  dp_path <- system.file("extdata", "mica", package = "camtrapdp")
  dp_without_media <- read_camtrap_dp(
    path = dp_path,
    media = FALSE)
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
                class(dp_without_media$deployments)))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
                class(dp_without_media$deployments)))
})

test_that("sc. names and vernacular names in obs match the info in taxonomic slot", {
  dp_path <- system.file("extdata", "mica", package = "camtrapdp")
  dp <- read_camtrap_dp(
    path = dp_path,
    media = FALSE)
  taxon_infos <- map_dfr(
    dp$datapackage$taxonomic,
    function(x) x %>% as.data.frame()) %>%
    tibble()
  expect_true(all(names(taxon_infos) %in% names(dp$observations)))
  # get scientific names from observations and check that they match with
  # taxonomic info
  sc_names <- dp$observations$scientificName[!is.na(dp$observations$scientificName)]
  expect_true(all(sc_names %in% taxon_infos$scientificName))

  # get vernacular names in English from observations and check that they match
  # with taxonomic info
  en_names <- dp$observations$vernacularNames.en[!is.na(dp$observations$vernacularNames.en)]
  expect_true(all(en_names %in% taxon_infos$vernacularNames.en))

  # get vernacular names in Dutch from observations and check that they match
  # with taxonomic info
  nl_names <- dp$observations$vernacularNames.nl[!is.na(dp$observations$vernacularNames.nl)]
  expect_true(all(nl_names %in% taxon_infos$vernacularNames.nl))
})
