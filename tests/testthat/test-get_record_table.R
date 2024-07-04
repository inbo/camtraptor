test_that("input of get_record_table, camtrap dp, is checked properly", {
  expect_error(get_record_table("aaa"))
  expect_error(get_record_table(1))
})

test_that("input of get_record_table, stationCol, is checked properly", {
  expect_error(get_record_table(mica, stationCol = "aaa"))
})

test_that("input of get_record_table, exclude, is checked properly", {
  expect_error(get_record_table(mica, exclude = "rattus not existing"))
})

test_that("input of get_record_table, minDeltaTime, is checked properly", {
  expect_error(
    get_record_table(mica, minDeltaTime = "1"),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
  expect_error(
    get_record_table(mica, minDeltaTime = -10),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
})

test_that("input of get_record_table, deltaTimeComparedTo, is checked properly", {
  expect_error(get_record_table(mica,
    minDeltaTime = 100,
    deltaTimeComparedTo = NULL
  ))
  expect_error(get_record_table(mica,
    minDeltaTime = 100,
    deltaTimeComparedTo = "not valid"
  ))
})

test_that("if not integer, minDeltaTime is set to integer (floor)", {
  record_table_int <- get_record_table(mica,
    minDeltaTime = 1000,
    deltaTimeComparedTo = "lastRecord"
  )
  record_table_dec <- suppressMessages(
    get_record_table(mica,
      minDeltaTime = 1000.7,
      deltaTimeComparedTo = "lastRecord"
    )
  )
  expect_identical(record_table_int, record_table_dec)
})

test_that("input of get_record_table, removeDuplicateRecords, is checked properly", {
  # only TRUE or FALSE are allowed
  expect_error(get_record_table(mica,
    removeDuplicateRecords = 5
  ))
  expect_error(get_record_table(mica,
    removeDuplicateRecords = NA
  ))
})

test_that("right columns are returned", {
  expect_named(
    get_record_table(mica),
    c(
      "Station",
      "Species",
      "n",
      "DateTimeOriginal",
      "Date",
      "Time",
      "delta.time.secs",
      "delta.time.mins",
      "delta.time.hours",
      "delta.time.days",
      "Directory",
      "FileName"
    )
  )
})

test_that("nrows = n obs of identified individuals if minDeltaTime is 0", {
  nrow_output <- get_record_table(mica, minDeltaTime = 0) %>% nrow()
  expect_identical(
    nrow_output,
    observations(mica) %>%
      dplyr::filter(!is.na(scientificName)) %>% nrow()
  )
})

test_that("nrows = n obs of red foxes if all other species are excluded", {
  species_to_exclude <- c(
    "Anas platyrhynchos",
    "Anas strepera",
    "Ardea",
    "Ardea cinerea",
    "Castor fiber",
    "Homo sapiens",
    "Martes foina",
    "Mustela putorius"
  )
  nrow_foxes <- get_record_table(mica, exclude = species_to_exclude) %>%
    nrow()
  expect_identical(
    nrow_foxes,
    observations(mica) %>%
      dplyr::filter(scientificName == "Vulpes vulpes") %>% nrow()
  )
})

test_that("Higher minDeltaTime means less rows returned", {
  nrow_delta_0 <- get_record_table(mica) %>% nrow()
  nrow_delta_10000 <- suppressMessages(get_record_table(mica,
    minDeltaTime = 10000,
    deltaTimeComparedTo = "lastRecord"
  )) %>%
    nrow()
  nrow_delta_100000 <- suppressMessages(get_record_table(mica,
    minDeltaTime = 100000,
    deltaTimeComparedTo = "lastRecord"
  )) %>%
    nrow()
  expect_lt(nrow_delta_10000, nrow_delta_0)
  expect_lt(nrow_delta_100000, nrow_delta_10000)
})

test_that("stations names are equal to values in column passed to StationCOl", {
  # use locationName as Station
  stations <- get_record_table(mica) %>%
    dplyr::distinct(Station) %>%
    dplyr::pull()
  location_names <- unique(deployments(mica)$locationName)
  expect_true(all(stations %in% location_names))

  # use locationID as Station
  stations <- get_record_table(mica, stationCol = "locationID") %>%
    dplyr::distinct(Station) %>%
    dplyr::pull()
  location_ids <- unique(deployments(mica)$locationID)
  expect_true(all(stations %in% location_ids))
})

test_that("Directory and Filename columns are lists", {
  file_values <- get_record_table(mica) %>%
    dplyr::select(Directory, FileName)
  expect_true(class(file_values$Directory) == "list")
  expect_true(class(file_values$FileName) == "list")
})

test_that(
  paste(
    "Directory and Filename element contain as many values as",
    "media of independent obs"
  ),
  {
    output <- get_record_table(mica)
    # add n media, observationID and sequenceID to record table
    output <- output %>%
      dplyr::mutate(len = purrr::map_dbl(Directory, function(x) length(x))) %>%
      dplyr::left_join(
        observations(mica) %>%
          dplyr::select(
            observationID,
            timestamp,
            scientificName,
            sequenceID
          ),
        by = c(
          "DateTimeOriginal" = "timestamp",
          "Species" = "scientificName"
        )
      )
    n_media <-
      media(mica) %>%
      dplyr::group_by(.data$sequenceID) %>%
      dplyr::count() %>%
      dplyr::rename(n_media = n)
    output <- output %>%
      dplyr::left_join(n_media,
        by = "sequenceID"
      )
    expect_equal(output$len, output$n_media)
  }
)

test_that(paste(
  "removeDuplicateRecords allows removing duplicates,",
  "but structure output remains the same"
), {
  mica_dup <- mica
  # create duplicates at 2020-07-29 05:46:48, location: B_DL_val 5_beek kleine vijver
  # use 3rd observation as the first two are unknown or blank (= no animal)
  mica_dup$data$observations[,"sequenceID"] <- observations(mica_dup)$sequenceID[3]
  mica_dup$data$observations[, "deploymentID"] <- observations(mica_dup)$deploymentID[3]
  mica_dup$data$observations[, "timestamp"] <- observations(mica_dup)$timestamp[3]
  mica_dup$data$observations[, "scientificName"] <- observations(mica_dup)$scientificName[3]
  
  rec_table <- get_record_table(mica_dup)
  rec_table_dup <- get_record_table(mica_dup,
    removeDuplicateRecords = FALSE
  )
  expect_identical(nrow(rec_table), 1L)
  expect_identical(
    rec_table$DateTimeOriginal, observations(mica)$timestamp[3]
  )
  expect_identical(rec_table$delta.time.secs, 0)
  expect_identical(names(rec_table_dup), names(rec_table))
  expect_identical(
    nrow(rec_table_dup),
    nrow(observations(mica_dup))
  )
})

test_that("Argument datapkg is deprecated: warning returned", {
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_record_table(datapkg = mica)
    ),
    "The `datapkg` argument of `get_record_table()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})
