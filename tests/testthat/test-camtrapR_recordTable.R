test_that("inputs of camtrapR_recordTable are correct", {
  # Check `x`
  expect_error(camtrapR_recordTable("aaa"))
  expect_error(camtrapR_recordTable(1))
  skip_if_offline()
  
  # Check `stationCol`
  x <- example_dataset()
  expect_error(
    camtrapR_recordTable(x, stationCol = "aaa"),
    paste0("Station column name `aaa` not valid: ",
           "It must be one of the deployments column names."
    ),
    fixed = TRUE
  )
  
  # Check `exclude`
  expect_error(
    camtrapR_recordTable(x, exclude = c("rattussss", "Rattus norvegicus")),
    paste0("The following species in `exclude` argument ",
           "are not present in the camera trap data package: `rattussss`."
    ),
    fixed = TRUE
  )
  
  # Check `minDeltaTime`
  expect_error(
    camtrapR_recordTable(x, minDeltaTime = "1"),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
  expect_error(
    camtrapR_recordTable(x, minDeltaTime = -10),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
  
  # Check `deltaTimeComparedTo`
  expect_error(camtrapR_recordTable(x,
    minDeltaTime = 100,
    deltaTimeComparedTo = NULL
  ))
  expect_error(camtrapR_recordTable(x,
    minDeltaTime = 100,
    deltaTimeComparedTo = "not valid"
  ))
  
  # Check `removeDuplicateRecords`
  expect_error(camtrapR_recordTable(x, removeDuplicateRecords = 5))
  expect_error(camtrapR_recordTable(x, removeDuplicateRecords = NA))
})

test_that("if not integer, `minDeltaTime` is set to integer (floor)", {
  skip_if_offline()
  x <- example_dataset()
  expect_message(
    camtrapR_recordTable(
      x,
      minDeltaTime = 1.2,
      deltaTimeComparedTo = "lastRecord"
    ),
    "`minDeltaTime` has to be an integer. Set to `1`."
  )
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

test_that("warning is returned if some observations have no `eventStart`", {
  skip_if_offline()
  x <- example_dataset()
  x_no_timestamp <- x
  o <- observations(x_no_timestamp)
  o$eventStart[1] <- NA
  observations(x_no_timestamp) <- o
  expect_warning(
    camtrapR_recordTable(x_no_timestamp),
    "Some observations have no `eventStart` and will be removed."
  )
  expect_identical(
    nrow(
      suppressWarnings(
        camtrapR_recordTable(x_no_timestamp, removeDuplicateRecords = FALSE))
      ),
    nrow(camtrapR_recordTable(x, removeDuplicateRecords = FALSE)) - 1L
  )
})

test_that("Right columns are returned", {
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
      "FileName",
      "latitude",
      "longitude",
      "clock",
      "solar"
    )
  )
})

test_that(paste(
  "nrows = n event-bsed obs of identified individuals if minDeltaTime is 0 and",
  "duplicates are allowed"
  ), {
  skip_if_offline()
  x <- example_dataset()
  nrow_output <- camtrapR_recordTable(
    x,
    minDeltaTime = 0,
    removeDuplicateRecords = FALSE
  ) %>%
    nrow()
  expect_identical(
    nrow_output,
    x %>%
      filter_observations(
        !is.na(scientificName) & observationLevel == "event"
      ) %>%
      observations() %>%
      nrow()
  )
})

test_that("Species in `exclude` are not present in output", {
  skip_if_offline()
  x <- example_dataset()
  species_to_exclude <- c("Anas platyrhynchos", "Anas strepera", "Ardea")
  species_in_output <- x %>%
    filter_observations(
      observationLevel == "event",
      !is.na(.data$scientificName),
      !scientificName %in% species_to_exclude
    ) %>%
    observations() %>%
    dplyr::distinct(.data$scientificName) %>%
    dplyr::arrange(.data$scientificName) %>%
    dplyr::pull(.data$scientificName)
  expect_equal(
    camtrapR_recordTable(x, exclude = species_to_exclude) %>%
      dplyr::distinct(Species) %>%
      dplyr::arrange(Species) %>%
      dplyr::pull(Species),
    species_in_output
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
  # Use `locationName` as Station
  stations <- camtrapR_recordTable(x) %>%
    dplyr::distinct(Station) %>%
    dplyr::arrange(Station) %>%
    dplyr::pull()
  location_names <- deployments(x) %>%
    dplyr::distinct(locationName) %>%
    dplyr::arrange(locationName) %>%
    dplyr::pull()
  expect_equal(stations, location_names)

  # Use `locationID` as Station
  stations <- camtrapR_recordTable(x, stationCol = "locationID") %>%
    dplyr::distinct(Station) %>%
    dplyr::arrange(Station) %>%
    dplyr::pull()
  location_ids <- deployments(x) %>%
    dplyr::distinct(locationID) %>%
    dplyr::arrange(locationID) %>%
    dplyr::pull()
  expect_equal(stations, location_ids)
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
    output <- camtrapR_recordTable(x, removeDuplicateRecords = FALSE)
    # add n media, observationID and eventID to record table
    output <- output %>%
      dplyr::mutate(len = purrr::map_dbl(Directory, function(x) length(x))) %>%
      dplyr::left_join(
        x %>%
          filter_observations(
            !is.na(scientificName),
            observationLevel == "event"
          ) %>%
        observations() %>%
          dplyr::select(
            observationID,
            eventStart,
            scientificName,
            eventID
          ),
        by = c(
          "DateTimeOriginal" = "eventStart",
          "Species" = "scientificName"
        ),
        relationship = "many-to-many"
      )
    n_media <-
      media(x) %>%
      dplyr::group_by(.data$eventID) %>%
      dplyr::count() %>%
      dplyr::rename(n_media = n)
    output <- output %>%
      dplyr::left_join(n_media,
        by = "eventID"
      )
    expect_equal(output$len, output$n_media)
})

test_that(paste(
  "`removeDuplicateRecords` allows removing observations of same species at",
  "same time, but structure output remains the same"
), {
  skip_if_offline()
  x <- example_dataset()
  rec_table <- camtrapR_recordTable(x)
  rec_table_dup <- camtrapR_recordTable(x, removeDuplicateRecords = FALSE)
  n_obs_no_dup <- x %>%
    filter_observations(
      !is.na(scientificName),
      observationLevel == "event"
    ) %>%
    observations() %>%
    dplyr::distinct(scientificName, deploymentID, eventStart) %>%
    nrow()
  expect_identical(nrow(rec_table), n_obs_no_dup)
  expect_gt(nrow(rec_table_dup), nrow(rec_table))
  expect_identical(names(rec_table_dup), names(rec_table))
  expect_identical(
    nrow(rec_table_dup),
    nrow(observations(x) %>%
      dplyr::filter(
        !is.na(.data$scientificName),
        .data$observationLevel == "event"
      )
    )
  )
})

test_that("clock is always in the range [0, 2*pi]", {
  skip_if_offline()
  x <- example_dataset()
  clock_values <- camtrapR_recordTable(x) %>%
    dplyr::pull(clock)
  expect_true(all(clock_values >= 0))
  expect_true(all(clock_values <= 2 * pi))
})

test_that("solar is always in the range [0, 2*pi]", {
  skip_if_offline()
  x <- example_dataset()
  solar_values <- camtrapR_recordTable(x) %>%
    dplyr::pull(solar)
  expect_true(all(solar_values >= 0))
  expect_true(all(solar_values <= 2 * pi))
})

test_that("get_record_table() is deprecated and calls camtrapR_recordTable()", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_deprecated(
    get_record_table(x),
    "was deprecated in camtraptor 1.0.0.",
    fixed = TRUE
  )
  expect_identical(
    suppressWarnings(get_record_table(x)),
    camtrapR_recordTable(x)
  )
})
