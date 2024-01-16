## read camera trap data package from v1.0
path_to_json_v1 <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
dp_v1_with_media <- suppressMessages(
  read_camtrap_dp(path_to_json_v1)
)
dp_v1_without_media <- suppressMessages(
  read_camtrap_dp(path_to_json_v1, media = FALSE)
)

test_that("file argument is checked properly", {
  expect_error(read_camtrap_dp("aaa"))
  expect_error(read_camtrap_dp(1))
})

test_that("file can be an URL", {
  # camtraptor is trailing camtrap-dp, refer to specific commit to keep using old version
  # dp_path <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/main/example/datapackage.json"
  dp_path <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
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

test_that("only DP versions 1.0 and dp 0.1.6 are supported", {
  expect_error(
    suppressMessages(read_camtrap_dp("https://raw.githubusercontent.com/tdwg/camtrap-dp/bb046c85a55bef2ced709357c0047f0136df8326/example/datapackage.json")),
    "Version `0.5` is not supported. Supported versions: 0.1.6 and 1.0",
    fixed = TRUE
  )
  
  expect_error(
    suppressMessages(read_camtrap_dp("https://raw.githubusercontent.com/tdwg/dwc-for-biologging/403f57db105982dc05b70f3cf66fd2b5591798db/derived/camtrap-dp/data/raw/datapackage.json")),
    "Version `tabular-data-package` is not supported. Supported versions: 0.1.6 and 1.0",
    fixed = TRUE
  )
})

test_that("test warnings while reading files with parsing issues", {
  local_edition(2)
  camtrap_dp_file_with_issues <- system.file(
    "extdata", "mica_parsing_issues", 
    "datapackage_for_parsing_issues.json", 
    package = "camtraptor"
  )
  captured_warnings <- capture_warnings(
    dp_issues <- camtraptor::read_camtrap_dp(file = camtrap_dp_file_with_issues)
  )
  
  # check number of warnings
  expect_length(captured_warnings, 6)
  
  # warning on deployments
  expect_identical(
    captured_warnings[2], # captured_warnings[1] is returned by readr via frictionless
    paste0(
      "One or more parsing issues occurred while reading `deployments`. ",
      "Check `?read_camtrap_dp()` for examples on how to use ",
      "`readr::problems()`."
    )
  )

  problems_deploys <- readr::problems(dp_issues$data$deployments)
  expect_identical(nrow(problems_deploys), 2L)
  expect_identical(problems_deploys$row, c(1L,2L))
  expect_identical(problems_deploys$col, c(7L,7L))
  expect_identical(problems_deploys$expected, rep("date like %Y-%m-%dT%H:%M:%S%z", 2))
  
  # warning on observations
  expect_identical(
    captured_warnings[4], # captured_warnings[3] is returned by readr via frictionless
    paste0(
      "One or more parsing issues occurred while reading `observations`. ",
      "Check `?read_camtrap_dp()` for examples on how to use ",
      "`readr::problems()`."
    )
  )
  problems_obs <- readr::problems(dp_issues$data$observations)
  expect_identical(nrow(problems_obs), 2L)
  expect_identical(problems_obs$row, c(1L,2L))
  expect_identical(problems_obs$col, c(5L,5L))
  expect_identical(problems_obs$expected, rep("date like %Y-%m-%dT%H:%M:%S%z", 2))
  
  # warning on media
  expect_identical(
    captured_warnings[6], # captured_warnings[5] is returned by readr via frictionless
    paste0(
      "One or more parsing issues occurred while reading `media`. ",
      "Check `?read_camtrap_dp()` for examples on how to use ",
      "`readr::problems()`."
    )
  )
  problems_media <- readr::problems(dp_issues$data$media)
  expect_identical(nrow(problems_media), 1L)
  expect_identical(problems_media$row, 2L)
  expect_identical(problems_media$col, 5L)
  expect_identical(problems_media$expected, "date like %Y-%m-%dT%H:%M:%S%z")
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
  expect_type(dp_without_media, "list")
  expect_true(is.list(dp_v1_with_media))
  expect_type(dp_v1_with_media, "list")
  expect_true(is.list(dp_v1_without_media))
  expect_type(dp_v1_without_media, "list")
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
  expect_length(dp_without_media$data, 3)
  expect_true("data" %in% names(dp_v1_with_media))
  expect_length(dp_v1_with_media$data, 3)
  expect_true("data" %in% names(dp_v1_without_media))
  expect_length(dp_v1_without_media$data, 3)
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
  # media is NULL only for data packages imported using `media` = `FALSE`
  expect_null(dp_without_media$data$media)
  expect_false(is.null(dp_with_media$data$media))
  expect_null(dp_v1_without_media$data$media)
  expect_false(is.null(dp_v1_with_media$data$media))
  # metadata are the same
  metadata_with_media <- dp_with_media
  metadata_with_media$data <- NULL
  metadata_without_media <- dp_without_media
  metadata_without_media$data <- NULL
  expect_identical(metadata_with_media, metadata_without_media)
  metadata_with_media_dp_v1 <- dp_v1_with_media
  metadata_with_media_dp_v1$data <- NULL
  metadata_without_media_dp_v1 <- dp_v1_without_media
  metadata_without_media_dp_v1$data <- NULL
  expect_identical(metadata_with_media_dp_v1, 
                   metadata_without_media_dp_v1)
  # observations are the same
  expect_identical(
    dp_with_media$data$observations, 
    dp_without_media$data$observations
  )
  expect_identical(
    dp_v1_with_media$data$observations, 
    dp_v1_without_media$data$observations
  )
  # deployments are the same
  expect_identical(
    dp_with_media$data$deployments, 
    dp_without_media$data$deployments
  )
  expect_identical(
    dp_v1_with_media$data$deployments, 
    dp_v1_without_media$data$deployments
  )
})

test_that("datapackage data elements are named as in resource names", {
  # check for v0.1.6
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_without_media <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
  resource_names <- frictionless::resources(dp_without_media)
  expect_true(all(names(dp_without_media$data) %in% resource_names))
  # check for v1.0
  resource_names <- frictionless::resources(dp_v1_with_media)
  expect_true(all(names(dp_v1_with_media$data) %in% resource_names))
  resource_names <- frictionless::resources(dp_v1_without_media)
  expect_true(all(names(dp_v1_without_media$data %in% resource_names)))
})

test_that("datapackage resources are tibble dataframes", {
  dp_path <- system.file("extdata", "mica", "datapackage.json",
    package = "camtraptor"
  )
  dp_without_media <- suppressMessages(read_camtrap_dp(
    file = dp_path,
    media = FALSE
  ))
  # check for v0.1.6 (only one of the two: chosen for the one without media)
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
    class(dp_without_media$data$deployments)))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
    class(dp_without_media$data$observations)))
  # check for v1.0 (only one of the two: chosen for the one with media)
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
                    class(dp_v1_with_media$data$deployments)))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
                    class(dp_v1_with_media$data$observations)))
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in%
                    class(dp_v1_with_media$data$media)))
})

test_that(
  "v0.1.6: sc. names and vernacular names in obs match info in metadata", {
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

test_that(
  "v1.0: sc. names and vernacular names in obs match info in metadata", {
    taxon_infos <- purrr::map_dfr(
      dp_v1_with_media$taxonomic,
      function(x) x %>% as.data.frame()
    ) %>%
      dplyr::tibble()
    expect_true(
      all(names(taxon_infos) %in% names(dp_v1_with_media$data$observations))
    )
    # get scientific names from observations and check that they match with
    # taxonomic info
    sc_names <- dp_v1_with_media$data$observations$scientificName[!is.na(
      dp_v1_with_media$data$observations$scientificName
    )]
    expect_true(all(sc_names %in% taxon_infos$scientificName))
    
    # get vernacular names in English from observations and check that they match
    # with taxonomic info
    en_names <- dp_v1_with_media$data$observations$vernacularNames.eng[
      !is.na(dp_v1_with_media$data$observations$vernacularNames.eng)
    ]
    expect_true(all(en_names %in% taxon_infos$vernacularNames.eng))
    
    # get vernacular names in Dutch from observations and check that they match
    # with taxonomic info
    nl_names <- dp_v1_with_media$data$observations$vernacularNames.nld[
      !is.na(
        dp_v1_with_media$data$observations$vernacularNames.nld
      )
    ]
    expect_true(all(nl_names %in% taxon_infos$vernacularNames.nld))
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

test_that(
  "read deployments v1.0: latitude follows longitude and both present", {
  expect_true("latitude" %in% names(dp_v1_with_media$data$deployments))
  expect_true("longitude" %in% names(dp_v1_with_media$data$deployments))
  which(names(dp_v1_with_media$data$deployments) == "latitude") ==
    which(names(dp_v1_with_media$data$deployments) == "longitude") + 1
})

test_that("read deployments v1.0: eventStart is renamed as start", {
  expect_false("eventStart" %in% names(dp_v1_with_media$data$deployments))
  expect_true("start" %in% names(dp_v1_with_media$data$deployments))
})

test_that("read deployments v1.0: eventEnd is renamed as end", {
  expect_false("eventEnd" %in% names(dp_v1_with_media$data$deployments))
  expect_true("end" %in% names(dp_v1_with_media$data$deployments))
})

test_that(
  "read deployments v1.0: cameraDelay is renamed as cameraInterval", {
    expect_false("cameraDelay" %in% names(dp_v1_with_media$data$deployments))
    expect_true("cameraInterval" %in% names(dp_v1_with_media$data$deployments))
})

test_that(
  "read deployments v1.0: detectionDistance is a new term and is ignored", {
    expect_false(
      "detectionDistance" %in% names(dp_v1_with_media$data$deployments)
    )
})

test_that(
  "read deployments v1.0: baitUse is a factor, not a boolean", {
    expect_s3_class(dp_v1_with_media$data$deployments$baitUse, "factor")
    baitUse_levels <- c("none", "scent", "food", "visual", "acoustic", "other")
    expect_identical(
      levels(dp_v1_with_media$data$deployments$baitUse), baitUse_levels
    )
  }
)

test_that("read deployments v1.0: boolean NA becomes a factor NA", {
  ## no longer present in the example package!
  skip_if_not(any(is.na(
    readr::read_csv(
      file.path(dirname(path_to_json_v1), "deployments.csv"),
      show_col_types = FALSE,
      col_select = "baitUse"
    )
  )),
  message = "There are no NA values present in deployments.baitUse")
  
  expect_true(all(is.na(dp_v1_with_media$data$deployments$baitUse)))
})

test_that(
  "read deployments v1.0: session is left empty when deploymentGroups is NA",{
    skip_if(all(!is.na(
      readr::read_csv(
        file.path(dirname(path_to_json_v1), "deployments.csv"),
        show_col_types = FALSE,
        col_select = "deploymentGroups"
      )
    )), message = "All rows have value for deploymentGroups in deployments.csv")
    expect_true(any(is.na(
      dp_v1_with_media$data$deployments$session
    )))
  })

test_that("read deployments v1.0: array is left empty", {
  expect_true(all(is.na(dp_v1_with_media$data$deployments$array)))
})

test_that("read deployments v1.0: deploymentTags is renamed as tags", {
  expect_false(
    "deploymentTags" %in% names(dp_v1_with_media$data$deployments)
  )
  expect_true("tags" %in% names(dp_v1_with_media$data$deployments))
})

test_that(
  "read deployments v1.0: deploymentComments is renamed as comments", {
    expect_false(
      "deploymentComments" %in% names(dp_v1_with_media$data$deployments)
    )
    expect_true("comments" %in% names(dp_v1_with_media$data$deployments))
  }
)

test_that("read deployments v1.0: _id is left empty", {
  expect_true(all(is.na(dp_v1_with_media$data$deployments$`_id`)))
})

test_that(
  "all cols `v0.1.6:deployments` are present in `v1.0`", {
    dp_path <- system.file("extdata", "mica", "datapackage.json",
                           package = "camtraptor"
    )
    dp_without_media <- suppressMessages(read_camtrap_dp(
      file = dp_path,
      media = FALSE
    ))

    cols_deployments_dp_v1 <- dp_v1_without_media$data$deployments %>%
      names()
    cols_deployments_dp_v0_1_6 <- dp_without_media$data$deployments %>%
      names()
    expect_identical(cols_deployments_dp_v0_1_6, cols_deployments_dp_v1)
  }
)

test_that("read observations v1.0: media-based observations are removed", {
  expect_true(all(is.na(dp_v1_with_media$data$observations$mediaID)))
})

test_that("read observations v1.0: eventID is renamed as sequenceID", {
  expect_false("eventID" %in% names(dp_v1_with_media$data$observations))
  expect_true("sequenceID" %in% names(dp_v1_with_media$data$observations))
})

test_that("read observations v1.0: eventStart is renamed as timestamp", {
  expect_false("eventStart" %in% names(dp_v1_with_media$data$observations))
  expect_true("timestamp" %in% names(dp_v1_with_media$data$observations))
})

test_that(
  "read observations v1.0: eventEnd is a new term and is ignored", {
  expect_false("eventEnd" %in% names(dp_v1_with_media$data$observations))
})

test_that(
  "read observations v1.0: observationLevel is a new term and is ignored", {
    expect_false(
      "observationLevel" %in% names(dp_v1_with_media$data$observations)
    )
})

test_that(
  "read observations v1.0: cameraSetupType is renamed as cameraSetup", {
  expect_false(
    "cameraSetupType" %in% names(dp_v1_with_media$data$observations)
  )
  expect_true("cameraSetup" %in% names(dp_v1_with_media$data$observations))
})

test_that("read observations v1.0: countNew is left empty", {
  expect_true(all(is.na(dp_v1_with_media$data$observations$countNew)))
})

test_that("read observations v1.0: higher taxonomic ranks ignored", {
  expect_false(
    any(c("kingdom", "phylum", "class", "order", "family", "genus") %in% 
          names(dp_v1_with_media$data$observations)
      )
  )
})

test_that(
  "read observations v1.0: behavior is renamed as behavior", {
  expect_false("behavior" %in% names(dp_v1_with_media$data$observations))
  expect_true("behaviour" %in% names(dp_v1_with_media$data$observations))
})

test_that(
  "read observations v1.0: classificationProbability renamed as classificationConfidence", 
  {
    expect_false(
      "classificationProbability" %in% 
        names(dp_v1_with_media$data$observations)
    )
    expect_true(
      "classificationConfidence" %in% 
        names(dp_v1_with_media$data$observations)
    )
  }
)

test_that(
  "read observations v1.0: observationComments is renamed as comments", {
  expect_false(
    "observationComments" %in% names(dp_v1_with_media$data$observations)
  )
  expect_true("comments" %in% names(dp_v1_with_media$data$observations))
})

test_that("read observations v1.0: _id is left empty", {
  expect_true(all(is.na(dp_v1_with_media$data$observations$`_id`)))
})

test_that(
  "read observations v1.0: individualSpeed is renamed as speed", {
    expect_false(
      "individualSpeed" %in% names(dp_v1_with_media$data$observations)
    )
    expect_true("speed" %in% names(dp_v1_with_media$data$observations))
})

test_that(
  "read observations v1.0: individualPositionRadius is renamed as radius", {
    expect_false(
      "individualPositionRadius" %in% 
        names(dp_v1_with_media$data$observations)
    )
    expect_true("radius" %in% names(dp_v1_with_media$data$observations))
  }
)

test_that(
  "read observations v1.0: radius is NA as NA in media based obs too", {
    expect_true(all(is.na(dp_v1_with_media$data$observations$radius)))
  }
)

test_that(
  "read observations v1.0: individualPositionAngle is renamed as angle", {
    expect_false(
      "individualPositionAngle" %in% 
        names(dp_v1_with_media$data$observations)
    )
    expect_true("angle" %in% names(dp_v1_with_media$data$observations))
  }
)

test_that(
  "read observations v1.0: angle is NA as NA in media based obs too", {
    expect_true(all(is.na(dp_v1_with_media$data$observations$angle)))
  }
)

test_that(
  "read observations v1.0: bounding box related columns are not present", {
    expect_false(
      "bboxX" %in% 
        names(dp_v1_with_media$data$observations)
    )
    expect_false(
      "bboxY" %in% 
        names(dp_v1_with_media$data$observations)
    )
    expect_false(
      "bboxWidth" %in% 
        names(dp_v1_with_media$data$observations)
    )
    expect_false(
      " bboxHeight" %in% 
        names(dp_v1_with_media$data$observations)
    )
  }
)

test_that(
  "all cols `v0.1.6:observations` are present in `v1.0`", {
    # notice that cols with vernacular names are different due to use of ISO
    # 693-3 in v1.0 vs ISO 693-2 in v0.1.6.
    dp_path <- system.file("extdata", "mica", "datapackage.json",
                           package = "camtraptor"
    )
    dp_without_media <- suppressMessages(read_camtrap_dp(
      file = dp_path,
      media = FALSE
    ))
    cols_obs_dp_v1 <- dp_v1_with_media$data$observations %>%
      dplyr::select(-dplyr::starts_with("vernacularNames")) %>%
      names()
    cols_obs_dp_v0_1_6 <- dp_without_media$data$observations %>%
      dplyr::select(-dplyr::starts_with("vernacularNames")) %>%
      names()
    expect_true(
      all(cols_obs_dp_v0_1_6 %in% cols_obs_dp_v1)
    )
  }
)

test_that("read media v1.0: sequenceID is added", {
  expect_true("sequenceID" %in% names(dp_v1_with_media$data$media))
})

test_that(
  "read media v1.0: filePublic is a new term in v1.0 and is ignored", {
  expect_false("filePublic" %in% names(dp_v1_with_media$data$media))
})

test_that("read media v1.0: favorite is renamed as favourite", {
  expect_false("favorite" %in% names(dp_v1_with_media$data$media))
  expect_true("favourite" %in% names(dp_v1_with_media$data$media))
})

test_that("read media v1.0: mediaComments is renamed as comments", {
  expect_false("mediaComments" %in% names(dp_v1_with_media$data$media))
  expect_true("comments" %in% names(dp_v1_with_media$data$media))
})

test_that("read media v1.0: _id is left empty", {
  expect_true(all(is.na(dp_v1_with_media$data$media$`_id`)))
})

test_that(
  "all cols `v0.1.6:media` are present in `v1.0:media`", {
    dp_path <- system.file("extdata", "mica", "datapackage.json",
                           package = "camtraptor"
    )
    dp_with_media <- suppressMessages(read_camtrap_dp(
      file = dp_path,
      media = TRUE
    ))
    cols_media_dp_v1 <- dp_v1_with_media$data$media %>%
      names()
    cols_media_dp_v0_1_6 <- dp_with_media$data$media %>%
      names()
    expect_identical(cols_media_dp_v1, cols_media_dp_v0_1_6)
  }
)

test_that("v1.0:media$captureMethod values are replaced with v0.1.6 values",{
  expect_false(
    "activityDetection" %in% dp_v1_with_media$data$media$captureMethod
  )
  expect_identical(
    levels(dp_v1_with_media$data$media$captureMethod),
    c("motionDetection", "timeLapse")
  )
})

test_that("v1.0:project$captureMethod values are replaced with v0.1.6 values", {
  expect_false(
    "activityDetection" %in% dp_v1_with_media$project$captureMethod
  )
  expect_identical(
    dp_v1_with_media$project$captureMethod,
    c("motionDetection", "timeLapse")
  )
})
