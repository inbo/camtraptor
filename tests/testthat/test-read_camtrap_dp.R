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
  dp_without_media <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
  expect_true(is.list(dp_without_media))
  expect_equal(class(dp_without_media), "list")
})

test_that("output data slot is a list of length 3", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_without_media <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
  expect_true("data" %in% names(dp_without_media))
  expect_equal(length(dp_without_media$data), 3)
})

test_that("media arg influences only slot media", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_with_media <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = TRUE
  ))
  dp_without_media <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
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
  dp_without_media <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
  resource_names <- dp_without_media$resource_names
  expect_true(all(resource_names %in% names(dp_without_media$data)))
})

test_that("Datapackage resources are tibble dataframes", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_without_media <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
    class(dp_without_media$data$deployments)))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
    class(dp_without_media$data$observations)))
})

test_that("sc. names and vernacular names in obs match the info in taxonomic slot", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
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
  dp <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
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
    suppressMessages(expect_warning(read_camtrap_dp(
      file = dp_path_warning,
      media = FALSE
    )))
  )
  rlang::with_options(
    lifecycle_verbosity = "warning",
    suppressMessages(expect_warning(read_camtrap_dp(
      path = dp_path_warning,
      media = FALSE
    )))
  )
})

test_that("only DP versions 1.0-rc.1 and dp 0.1.6 are supported", {
  expect_error(
    read_camtrap_dp("https://raw.githubusercontent.com/tdwg/camtrap-dp/bb046c85a55bef2ced709357c0047f0136df8326/example/datapackage.json"),
    "Version https://raw.githubusercontent.com/tdwg/camtrap-dp/0.5/camtrap-dp-profile.json not supported. camtraptor supports camtrap-dp versions: 1.0-rc.1 and 0.1.6."
  )
  
  expect_error(
    read_camtrap_dp("https://raw.githubusercontent.com/tdwg/dwc-for-biologging/403f57db105982dc05b70f3cf66fd2b5591798db/derived/camtrap-dp/data/raw/datapackage.json"),
    "Version tabular-data-package not supported. camtraptor supports camtrap-dp versions: 1.0-rc.1 and 0.1.6."
  )
})

## read camera trap data package from v1.0-rc1
v1_rc1 <- read_camtrap_dp("https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0-rc.1/example/datapackage.json")

test_that(
  "read deployments v1.0-rc1: latitude follows longitude and both present", {
  expect_true("latitude" %in% names(v1_rc1$data$deployments))
  expect_true("longitude" %in% names(v1_rc1$data$deployments))
  which(names(v1_rc1$data$deployments) == "latitude") ==
    which(names(v1_rc1$data$deployments) == "longitude") + 1
})

test_that("read deployments v1.0-rc1: eventStart is renamed as start", {
  expect_false("eventStart" %in% names(v1_rc1$data$deployments))
  expect_true("start" %in% names(v1_rc1$data$deployments))
})

test_that("read deployments v1.0-rc1: eventEnd is renamed as end", {
  expect_false("eventEnd" %in% names(v1_rc1$data$deployments))
  expect_true("end" %in% names(v1_rc1$data$deployments))
})

test_that(
  "read deployments v1.0-rc1: cameraDelay is renamed as cameraInterval", {
    expect_false("cameraDelay" %in% names(v1_rc1$data$deployments))
    expect_true("cameraInterval" %in% names(v1_rc1$data$deployments))
})

test_that(
  "read deployments v1.0-rc1: detectionDistance is a new term and is ignored", {
    expect_false("detectionDistance" %in% names(v1_rc1$data$deployments))
})

test_that(
  "read deployments v1.0-rc1: baitUse is a factor, not a boolean", {
    expect_s3_class(v1_rc1$data$deployments$baitUse, "factor")
    baitUse_levels <- c("none", "scent", "food", "visual", "acoustic", "other")
    expect_equal(levels(v1_rc1$data$deployments$baitUse), baitUse_levels)
    # boolean NA becomes a factor NA
    expect_true(all(is.na(v1_rc1$data$deployments$baitUse)))
  }
)

test_that("read deployemnts v1.0-rc1: session is left empty", {
  expect_true(all(is.na(v1_rc1$data$deployments$session)))
})

test_that("read deployemnts v1.0-rc1: array is left empty", {
  expect_true(all(is.na(v1_rc1$data$deployments$array)))
})

test_that("read deployemnts v1.0-rc1: deploymentTags is renamed as tags", {
  expect_false("deploymentTags" %in% names(v1_rc1$data$deployments))
  expect_true("tags" %in% names(v1_rc1$data$deployments))
})

test_that("read deployemnts v1.0-rc1: deploymentComments is renamed as comments", {
  expect_false("deploymentComments" %in% names(v1_rc1$data$deployments))
  expect_true("comments" %in% names(v1_rc1$data$deployments))
})

test_that("read deployments v1.0-rc1: _id is left empty", {
  expect_true(all(is.na(v1_rc1$data$deployments$`_id`)))
})

test_that("read observations v1.0-rc1: media-based observations are removed", {
  expect_true(all(is.na(v1_rc1$data$observations$mediaID)))
})

test_that("read observations v1.0-rc1: eventID is renamed as sequenceID", {
  expect_false("eventID" %in% names(v1_rc1$data$observations))
  expect_true("sequenceID" %in% names(v1_rc1$data$observations))
})

test_that("read observations v1.0-rc1: eventStart is renamed as timestamp", {
  expect_false("eventStart" %in% names(v1_rc1$data$observations))
  expect_true("timestamp" %in% names(v1_rc1$data$observations))
})

test_that(
  "read observations v1.0-rc1: eventEnd is a new term and is ignored", {
  expect_false("eventEnd" %in% names(v1_rc1$data$observations))
})

test_that(
  "read observations v1.0-rc1: observationLevel is a new term and is ignored", {
    expect_false("observationLevel" %in% names(v1_rc1$data$observations))
})

test_that(
  "read observations v1.0-rc1: cameraSetupType is renamed as cameraSetup", {
  expect_false("cameraSetupType" %in% names(v1_rc1$data$observations))
  expect_true("cameraSetup" %in% names(v1_rc1$data$observations))
})

test_that("read observations v1.0-rc1: countNew is left empty", {
  expect_true(all(is.na(v1_rc1$data$observations$countNew)))
})

test_that(
  "read observations v1.0-rc1: behavior is renamed as behavior", {
  expect_false("behavior" %in% names(v1_rc1$data$observations))
  expect_true("behaviour" %in% names(v1_rc1$data$observations))
})

test_that(
  "read observations v1.0-rc1: classificationProbability renamed as classificationConfidence", 
  {
    expect_false("classificationProbability" %in% names(v1_rc1$data$observations))
    expect_true("classificationConfidence" %in% names(v1_rc1$data$observations))
  }
)

test_that(
  "read observations v1.0-rc1: observationComments is renamed as comments", {
  expect_false("observationComments" %in% names(v1_rc1$data$observations))
  expect_true("comments" %in% names(v1_rc1$data$observations))
})

test_that("read media v1.0-rc1: sequenceID is added", {
  expect_true("sequenceID" %in% names(v1_rc1$data$media))
})

test_that(
  "read media v1.0-rc1: filePublic is a new term in v1.0-rc1 and is ignored", {
  expect_false("filePublic" %in% names(v1_rc1$data$media))
})

test_that("read media v1.0-rc1: favorite is renamed as favourite", {
  expect_false("favorite" %in% names(v1_rc1$data$media))
  expect_true("favourite" %in% names(v1_rc1$data$media))
})

test_that("read media v1.0-rc1: mediaComments is renamed as comments", {
  expect_false("mediaComments" %in% names(v1_rc1$data$media))
  expect_true("comments" %in% names(v1_rc1$data$media))
})

test_that("read media v1.0-rc1: _id is left empty", {
  expect_true(all(is.na(v1_rc1$data$media$`_id`)))
})
