test_that("input of camtrapR_recordTable, camtrap dp, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(camtrapR_recordTable("aaa"))
  expect_error(camtrapR_recordTable(1))
})

test_that("input of camtrapR_recordTable, stationCol, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(camtrapR_recordTable(x, stationCol = "aaa"))
})

test_that("input of camtrapR_recordTable, exclude, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(camtrapR_recordTable(x, exclude = "rattus not existing"))
})

test_that("input of camtrapR_recordTable, minDeltaTime, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    camtrapR_recordTable(x, minDeltaTime = "1"),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
  expect_error(
    camtrapR_recordTable(x, minDeltaTime = -10),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
})

test_that("input of camtrapR_recordTable, deltaTimeComparedTo, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(camtrapR_recordTable(x,
    minDeltaTime = 100,
    deltaTimeComparedTo = NULL
  ))
  expect_error(camtrapR_recordTable(x,
    minDeltaTime = 100,
    deltaTimeComparedTo = "not valid"
  ))
})

test_that("if not integer, minDeltaTime is set to integer (floor)", {
  skip_if_offline()
  x <- example_dataset()
  record_table_int <- camtrapR_recordTable(x,
    minDeltaTime = 1000,
    deltaTimeComparedTo = "lastRecord"
  )
  record_table_dec <- suppressMessages(
    camtrapR_recordTable(x,
      minDeltaTime = 1000.7,
      deltaTimeComparedTo = "lastRecord"
    )
  )
  expect_identical(record_table_int, record_table_dec)
})

test_that("input of camtrapR_recordTable, removeDuplicateRecords, is checked properly", {
  skip_if_offline()
  x <- example_dataset()
  # only TRUE or FALSE are allowed
  expect_error(camtrapR_recordTable(x,
    removeDuplicateRecords = 5
  ))
  expect_error(camtrapR_recordTable(x,
    removeDuplicateRecords = NA
  ))
})

test_that("right columns are returned", {
  skip_if_offline()
  x <- example_dataset()
  expect_named(
    camtrapR_recordTable(x),
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
  nrow_output <- camtrapR_recordTable(x, minDeltaTime = 0) %>% nrow()
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
    "Aves",
    "Homo sapiens",
    "Martes foina",
    "Mustela putorius",
    "rattus norvegicus"
  )
  nrow_foxes <- camtrapR_recordTable(x, exclude = species_to_exclude) %>%
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
  nrow_delta_0 <- camtrapR_recordTable(x) %>% nrow()
  nrow_delta_10000 <- suppressMessages(camtrapR_recordTable(x,
    minDeltaTime = 10000,
    deltaTimeComparedTo = "lastRecord"
  )) %>%
    nrow()
  nrow_delta_100000 <- suppressMessages(camtrapR_recordTable(x,
    minDeltaTime = 100000,
    deltaTimeComparedTo = "lastRecord"
  )) %>%
    nrow()
  expect_lt(nrow_delta_10000, nrow_delta_0)
  expect_lt(nrow_delta_100000, nrow_delta_10000)
})

test_that("stations names are equal to values in column passed to StationCOl", {
  # use locationName as Station
  stations <- camtrapR_recordTable(x) %>%
    dplyr::distinct(Station) %>%
    dplyr::pull()
  location_names <- unique(purrr::pluck(deployments(x), "locationName"))
  expect_true(all(stations %in% location_names))

  # use locationID as Station
  stations <- camtrapR_recordTable(x, stationCol = "locationID") %>%
    dplyr::distinct(Station) %>%
    dplyr::pull()
  location_ids <- unique(purrr::pluck(deployments(x), "locationID"))
  expect_true(all(stations %in% location_ids))
})

test_that("Directory and Filename columns are lists", {
  file_values <- camtrapR_recordTable(x) %>%
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
    output <- camtrapR_recordTable(x)
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
  x_dup$data$observations[,"sequenceID"] <- purrr::pluck(
    observations(x_dup),
    "sequenceID",
    3
  )
  x_dup$data$observations[, "deploymentID"] <- purrr::pluck(
    observations(x_dup),
    "deploymentID",
    3
  )
  x_dup$data$observations[, "timestamp"] <- purrr::pluck(
    observations(x_dup),
    "timestamp",
    3
  )
  x_dup$data$observations[, "scientificName"] <- purrr::pluck(
    observations(x_dup),
    "scientificName",
    3
  )
  
  rec_table <- camtrapR_recordTable(x_dup)
  rec_table_dup <- camtrapR_recordTable(x_dup,
    removeDuplicateRecords = FALSE
  )
  expect_identical(nrow(rec_table), 1L)
  expect_identical(
    rec_table$DateTimeOriginal, purrr::pluck(observations(x), "timestamp", 3)
  )
  expect_identical(rec_table$delta.time.secs, 0)
  expect_identical(names(rec_table_dup), names(rec_table))
  expect_identical(
    nrow(rec_table_dup),
    nrow(observations(x_dup))
  )
})

test_that("get_record_table() is deprecated and calls camtrapR_recordTable()", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_deprecated(get_record_table(x))
})

test_that(
  "output of get_record_table() is the same as camtrapR_recordTable()",
  {
    skip_if_offline()
    x <- example_dataset()
    expect_identical(
      suppressWarnings(get_record_table(x)),
      camtrapR_recordTable(x))
  }
)
