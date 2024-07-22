test_that("input of get_record_table, camtrap dp, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(get_record_table("aaa"))
  expect_error(get_record_table(1))
})

test_that("input of get_record_table, stationCol, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(get_record_table(x, stationCol = "aaa"))
})

test_that("input of get_record_table, exclude, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(get_record_table(x, exclude = "rattus not existing"))
})

test_that("input of get_record_table, minDeltaTime, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    get_record_table(x, minDeltaTime = "1"),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
  expect_error(
    get_record_table(x, minDeltaTime = -10),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
})

test_that("input of get_record_table, deltaTimeComparedTo, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(get_record_table(x,
    minDeltaTime = 100,
    deltaTimeComparedTo = NULL
  ))
  expect_error(get_record_table(x,
    minDeltaTime = 100,
    deltaTimeComparedTo = "not valid"
  ))
})

test_that("if not integer, minDeltaTime is set to integer (floor)", {
  skip_if_offline()
  x <- example_dataset()
  record_table_int <- get_record_table(x,
    minDeltaTime = 1000,
    deltaTimeComparedTo = "lastRecord"
  )
  record_table_dec <- suppressMessages(
    get_record_table(x,
      minDeltaTime = 1000.7,
      deltaTimeComparedTo = "lastRecord"
    )
  )
  expect_identical(record_table_int, record_table_dec)
})

test_that("input of get_record_table, removeDuplicateRecords, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  # only TRUE or FALSE are allowed
  expect_error(get_record_table(x,
    removeDuplicateRecords = 5
  ))
  expect_error(get_record_table(x,
    removeDuplicateRecords = NA
  ))
})

test_that("right columns are returned", {
  skip_if_offline()
  x <- example_dataset()
  expect_named(
    get_record_table(x),
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
  skip_if_offline()
  x <- example_dataset()
  nrow_output <- get_record_table(x, minDeltaTime = 0) %>% nrow()
  expect_identical(
    nrow_output,
    observations(x) %>%
      dplyr::filter(!is.na(scientificName)) %>% nrow()
  )
})

test_that("nrows = n obs of red foxes if all other species are excluded", {
  skip_if_offline()
  x <- example_dataset()
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
  nrow_foxes <- get_record_table(x, exclude = species_to_exclude) %>%
    nrow()
  expect_identical(
    nrow_foxes,
    observations(x) %>%
      dplyr::filter(scientificName == "Vulpes vulpes") %>% nrow()
  )
})

test_that("Higher minDeltaTime means less rows returned", {
  skip_if_offline()
  x <- example_dataset()
  nrow_delta_0 <- get_record_table(x) %>% nrow()
  nrow_delta_10000 <- suppressMessages(get_record_table(x,
    minDeltaTime = 10000,
    deltaTimeComparedTo = "lastRecord"
  )) %>%
    nrow()
  nrow_delta_100000 <- suppressMessages(get_record_table(x,
    minDeltaTime = 100000,
    deltaTimeComparedTo = "lastRecord"
  )) %>%
    nrow()
  expect_lt(nrow_delta_10000, nrow_delta_0)
  expect_lt(nrow_delta_100000, nrow_delta_10000)
})

test_that("stations names are equal to values in column passed to StationCOl", {
  # use locationName as Station
  stations <- get_record_table(x) %>%
    dplyr::distinct(Station) %>%
    dplyr::pull()
  location_names <- unique(deployments(x)$locationName)
  expect_true(all(stations %in% location_names))

  # use locationID as Station
  stations <- get_record_table(x, stationCol = "locationID") %>%
    dplyr::distinct(Station) %>%
    dplyr::pull()
  location_ids <- unique(deployments(x)$locationID)
  expect_true(all(stations %in% location_ids))
})

test_that("Directory and Filename columns are lists", {
  file_values <- get_record_table(x) %>%
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
    output <- get_record_table(x)
    # add n media, observationID and sequenceID to record table
    output <- output %>%
      dplyr::mutate(len = purrr::map_dbl(Directory, function(x) length(x))) %>%
      dplyr::left_join(
        observations(x) %>%
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
      media(x) %>%
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
  skip_if_offline()
  x <- example_dataset()
  x_dup <- x
  # create duplicates at 2020-07-29 05:46:48, location: B_DL_val 5_beek kleine vijver
  # use 3rd observation as the first two are unknown or blank (= no animal)
  x_dup$data$observations[,"sequenceID"] <- observations(x_dup)$sequenceID[3]
  x_dup$data$observations[, "deploymentID"] <- observations(x_dup)$deploymentID[3]
  x_dup$data$observations[, "timestamp"] <- observations(x_dup)$timestamp[3]
  x_dup$data$observations[, "scientificName"] <- observations(x_dup)$scientificName[3]
  
  rec_table <- get_record_table(x_dup)
  rec_table_dup <- get_record_table(x_dup,
    removeDuplicateRecords = FALSE
  )
  expect_identical(nrow(rec_table), 1L)
  expect_identical(
    rec_table$DateTimeOriginal, observations(x)$timestamp[3]
  )
  expect_identical(rec_table$delta.time.secs, 0)
  expect_identical(names(rec_table_dup), names(rec_table))
  expect_identical(
    nrow(rec_table_dup),
    nrow(observations(x_dup))
  )
})

test_that("Argument datapkg is deprecated: warning returned", {
  skip_if_offline()
  x <- example_dataset()
  expect_warning(
    rlang::with_options(
      lifecycle_verbosity = "warning",
      get_record_table(datapkg = x)
    ),
    "The `datapkg` argument of `get_record_table()` is deprecated as of camtraptor 0.16.0.",
    fixed = TRUE
  )
})
