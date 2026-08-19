test_that("get_record_table() is deprecated", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_deprecated(
    get_record_table(x),
    "was deprecated in camtraptor 1.0.0.",
    fixed = TRUE
  )
})

test_that("inputs of get_record_table are correct", {
  # Check `x`
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_error(get_record_table("aaa"))
  expect_error(get_record_table(1))
  skip_if_offline()
  
  # Check `stationCol`
  x <- example_dataset()
  expect_error(
    get_record_table(x, stationCol = "aaa"),
    paste0("Station column name `aaa` not valid: ",
           "It must be one of the deployments column names."
    ),
    fixed = TRUE
  )
  
  # Check `exclude`
  expect_error(
    get_record_table(x, exclude = c("rattussss", "Rattus norvegicus")),
    paste0("The following species in `exclude` argument ",
           "are not present in the Camera Trap Data Package: `rattussss`."
    ),
    fixed = TRUE
  )
  
  # Check `minDeltaTime`
  expect_error(
    get_record_table(x, minDeltaTime = "1"),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
  expect_error(
    get_record_table(x, minDeltaTime = -10),
    "`minDeltaTime` must be a number greater or equal to 0."
  )
  
  # Check `deltaTimeComparedTo`
  expect_error(
    get_record_table(x, minDeltaTime = 100, deltaTimeComparedTo = NULL)
  )
  expect_error(
    get_record_table(x, minDeltaTime = 100, deltaTimeComparedTo = "not valid")
  )
  
  # Check `removeDuplicateRecords`
  expect_error(get_record_table(x, removeDuplicateRecords = 5))
  expect_error(get_record_table(x, removeDuplicateRecords = NA))
})

test_that("if not integer, `minDeltaTime` is set to integer (floor)", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  expect_message(
    get_record_table(
      x,
      minDeltaTime = 1.2,
      deltaTimeComparedTo = "lastRecord"
    ),
    "`minDeltaTime` has to be an integer. Set to `1`."
  )
  record_table_int <- get_record_table(
    x,
    minDeltaTime = 1000,
    deltaTimeComparedTo = "lastRecord"
  )
  record_table_dec <- suppressMessages(
    get_record_table(
      x,
      minDeltaTime = 1000.7,
      deltaTimeComparedTo = "lastRecord"
    )
  )
  expect_identical(record_table_int, record_table_dec)
})

test_that(paste0(
  "warning is returned if some observations have no `eventStart` ",
  "or media have no timestamp"
), {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  x_no_eventStart <- x
  o <- observations(x_no_eventStart)
  o$eventStart[1] <- NA
  observations(x_no_eventStart) <- o
  expect_warning(
    get_record_table(x_no_eventStart),
    "Some observations have no `eventStart` and will be removed."
  )
  expect_identical(
    nrow(
      suppressWarnings(
        get_record_table(x_no_eventStart, removeDuplicateRecords = FALSE)
      )
    ),
    nrow(get_record_table(x, removeDuplicateRecords = FALSE)) - 1L
  )
  
  x_no_timestamp <- x
  m <- media(x_no_timestamp)
  # Set timestamp of media with "eventID == "4bb69c45" to NA
  m$timestamp[m$eventID == "4bb69c45"] <- NA
  media(x_no_timestamp) <- m
  expect_warning(
    get_record_table(x_no_timestamp),
    "Some media have no `timestamp` and will be removed."
  )
  expect_identical(
    nrow(
      suppressWarnings(
        get_record_table(x_no_timestamp, removeDuplicateRecords = FALSE)
      )
    ),
    nrow(get_record_table(x, removeDuplicateRecords = FALSE)) - 1L
  )
})

test_that("Right columns are returned", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
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
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  nrow_output <- get_record_table(
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
  rlang::local_options(lifecycle_verbosity = "quiet")
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
  expect_equal(get_record_table(x, exclude = species_to_exclude) %>%
      dplyr::distinct(Species) %>%
      dplyr::arrange(Species) %>%
      dplyr::pull(Species),
    species_in_output
  )
})

test_that("Higher minDeltaTime means less rows returned", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  nrow_delta_0 <- get_record_table(x) %>% nrow()
  nrow_delta_10000 <- suppressMessages(
    get_record_table(
      x,
      minDeltaTime = 10000,
      deltaTimeComparedTo = "lastRecord"
    )
  ) %>%
    nrow()
  nrow_delta_100000 <- suppressMessages(
    get_record_table(
      x,
      minDeltaTime = 100000,
      deltaTimeComparedTo = "lastRecord"
    )
  ) %>%
    nrow()
  expect_lt(nrow_delta_10000, nrow_delta_0)
  expect_lt(nrow_delta_100000, nrow_delta_10000)
})

test_that(paste0(
  "Values lastIndependentRecord and lastRecord can ",
  "return different number of rows"
), {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  obs <- observations(x)
  obs[obs$eventID == "02ae9f43", "eventStart"] <- lubridate::as_datetime(
    "2020-08-02 05:10:20"
  )
  
  med <- media(x) 
  rows_to_update <- which(med$eventID == "02ae9f43") 
  med[rows_to_update, "timestamp"] <- lubridate::as_datetime(
    "2020-08-02 05:10:20"
  ) 
  x_modified <- x
  observations(x_modified) <- obs
  media(x_modified) <- med
  
  rec_last_indep <- get_record_table(
    x_modified,
    minDeltaTime = 10,
    deltaTimeComparedTo = "lastIndependentRecord"
  )
  
  rec_last <- suppressMessages(
    get_record_table(
      x_modified,
      minDeltaTime = 10,
      deltaTimeComparedTo = "lastRecord"
    )
  )
  # Same columns
  expect_identical(names(rec_last_indep), names(rec_last))
  # One row less
  expect_identical(nrow(rec_last), nrow(rec_last_indep) - 1L)
})

test_that("stations names are equal to values in column passed to StationCOl", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  # Use `locationName` as Station
  stations <- get_record_table(x) %>%
    dplyr::distinct(Station) %>%
    dplyr::arrange(Station) %>%
    dplyr::pull()
  location_names <- deployments(x) %>%
    dplyr::distinct(locationName) %>%
    dplyr::arrange(locationName) %>%
    dplyr::pull()
  expect_equal(stations, location_names)

  # Use `locationID` as Station
  stations <- get_record_table(x, stationCol = "locationID") %>%
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
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  file_values <- get_record_table(x) %>%
    dplyr::select(Directory, FileName)
  expect_true(inherits(file_values$Directory, "list"))
  expect_true(inherits(file_values$FileName, "list"))
})

test_that(
  paste(
    "Directory and Filename element contain as many values as",
    "media of independent obs"
  ),
  {
    skip_if_offline()
    rlang::local_options(lifecycle_verbosity = "quiet")
    x <- example_dataset()
    output <- get_record_table(x, removeDuplicateRecords = FALSE)
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
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  rec_table <- get_record_table(x)
  rec_table_dup <- get_record_table(x, removeDuplicateRecords = FALSE)
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
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  clock_values <- get_record_table(x) %>%
    dplyr::pull(clock)
  expect_true(all(clock_values >= 0))
  expect_true(all(clock_values <= 2 * pi))
})

test_that("solar is always in the range [0, 2*pi]", {
  skip_if_offline()
  rlang::local_options(lifecycle_verbosity = "quiet")
  x <- example_dataset()
  solar_values <- get_record_table(x) %>%
    dplyr::pull(solar)
  expect_true(all(solar_values >= 0))
  expect_true(all(solar_values <= 2 * pi))
})
