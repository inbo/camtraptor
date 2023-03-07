test_that("file is checked properly", {
  expect_error(read_camtrap_dp("aaa"))
  expect_error(read_camtrap_dp(1))
})

test_that("test warnings", {
  local_edition(2)
  camtrap_dp_file_with_issues <- system.file("extdata", "mica_parsing_issues", "datapackage_for_parsing_issues.json", package = "camtraptor")
  # deployments
  expect_warning(
    camtraptor::read_camtrap_dp(
      file = camtrap_dp_file_with_issues
    ),
    "One or more parsing issues occurred while reading deployments."
  )
  # observations
  expect_warning(
    camtraptor::read_camtrap_dp(
      file = camtrap_dp_file_with_issues
    ),
    "One or more parsing issues occurred while reading observations."
  )
  # media
  expect_warning(
    camtraptor::read_camtrap_dp(
      file = camtrap_dp_file_with_issues
    ),
    "One or more parsing issues occurred while reading media."
  )
})


test_that("media is checked properly", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  expect_error(read_camtrap_dp(
    file = dp_path,
    media = "must_be_a_logical!"
  ))
})

test_that("output is a list", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_without_media <- read_camtrap_dp(
    file = dp_path,
    media = FALSE
  )
  expect_true(is.list(dp_without_media))
  expect_equal(class(dp_without_media), "list")
})

test_that("output data slot is a list of length 3", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_without_media <- read_camtrap_dp(
    file = dp_path,
    media = FALSE
  )
  expect_true("data" %in% names(dp_without_media))
  expect_equal(length(dp_without_media$data), 3)
})

test_that("media arg influences only slot media", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_with_media <- read_camtrap_dp(
    file = dp_path,
    media = TRUE
  )
  dp_without_media <- read_camtrap_dp(
    file = dp_path,
    media = FALSE
  )
  # media is NULL only for dp_without_media
  expect_null(dp_without_media$data$media)
  expect_false(is.null(dp_with_media$data$media))
  # metadata are the same
  metadata_with_media <- dp_with_media
  metadata_with_media$data <- NULL
  metadata_without_media <- dp_without_media
  metadata_without_media$data <- NULL
  expect_identical(metadata_with_media, metadata_without_media)
  # observations are the same
  expect_identical(
    # remove columns with NA only
    dp_with_media$data$observations[colSums(
      !is.na(dp_with_media$data$observations)
    ) > 0],
    # remove columns with NA only
    dp_without_media$data$observations[colSums(
      !is.na(dp_without_media$data$observations)
    ) > 0]
  )
  # deployments are the same
  expect_identical(
    # remove columns with NA only
    dp_with_media$data$deployments[colSums(
      !is.na(dp_with_media$data$deployments)
    ) > 0],
    # remove columns with NA only
    dp_with_media$data$deployments[colSums(
      !is.na(dp_with_media$data$deployments)
    ) > 0]
  )
})

test_that("Datapackage resources are named as in resource_names", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_without_media <- read_camtrap_dp(
    file = dp_path,
    media = FALSE
  )
  resource_names <- dp_without_media$resource_names
  expect_true(all(resource_names %in% names(dp_without_media$data)))
})

test_that("Datapackage resources are tibble dataframes", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_without_media <- read_camtrap_dp(
    file = dp_path,
    media = FALSE
  )
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
    class(dp_without_media$data$deployments)))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
    class(dp_without_media$data$observations)))
})

test_that("sc. names and vernacular names in obs match the info in taxonomic slot", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp <- read_camtrap_dp(
    file = dp_path,
    media = FALSE
  )
  taxon_infos <- purrr::map_dfr(
    dp$taxonomic,
    function(x) x %>% as.data.frame()
  ) %>%
    dplyr::tibble()
  expect_true(all(names(taxon_infos) %in% names(dp$data$observations)))
  # get scientific names from observations and check that they match with
  # taxonomic info
  sc_names <- dp$data$observations$scientificName[!is.na(
    dp$data$observations$scientificName
  )]
  expect_true(all(sc_names %in% taxon_infos$scientificName))

  # get vernacular names in English from observations and check that they match
  # with taxonomic info
  en_names <- dp$data$observations$vernacularNames.en[!is.na(
    dp$data$observations$vernacularNames.en
  )]
  expect_true(all(en_names %in% taxon_infos$vernacularNames.en))

  # get vernacular names in Dutch from observations and check that they match
  # with taxonomic info
  nl_names <- dp$data$observations$vernacularNames.nl[!is.na(
    dp$data$observations$vernacularNames.nl
  )]
  expect_true(all(nl_names %in% taxon_infos$vernacularNames.nl))
})

test_that("file can be an URL", {
  # camtraptor is trailing camtrap-dp, refer to specific commit to keep using old version
  # dp_path <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/main/example/datapackage.json"
  dp_path <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/bb046c85a55bef2ced709357c0047f0136df8326/example/datapackage.json"
  dp <- read_camtrap_dp(
    file = dp_path,
    media = FALSE
  )
  expect_true(is.list(dp))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
    class(dp$data$deployments)))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
    class(dp$data$observations)))
})

test_that("path is deprecated", {
  dp_path_warning <- system.file("extdata", "mica", package = "camtraptor")
  rlang::with_options(
    lifecycle_verbosity = "warning",
    expect_warning(read_camtrap_dp(
      file = dp_path_warning,
      media = FALSE
    ))
  )
  rlang::with_options(
    lifecycle_verbosity = "warning",
    expect_warning(read_camtrap_dp(
      path = dp_path_warning,
      media = FALSE
    ))
  )
})
