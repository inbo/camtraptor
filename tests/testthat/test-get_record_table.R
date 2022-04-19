test_that("input of get_record_table, camtrap dp, is checked properly", {
  testthat::expect_error(get_record_table("aaa"))
  testthat::expect_error(get_record_table(1))
})

test_that("input of get_record_table, stationCol, is checked properly", {
  testthat::expect_error(get_record_table(mica, stationCol = "aaa"))
})

test_that("input of get_record_table, exclude, is checked properly", {
  testthat::expect_error(get_record_table(mica, exclude = "rattus not existing"))
})

test_that("input of get_record_table, minDeltaTime, is checked properly", {
  testthat::expect_error(get_record_table(mica, minDeltaTime = "1"),
               "minDeltaTime must be a number greater or equal to 0")
  testthat::expect_error(get_record_table(mica, minDeltaTime = -10),
               "minDeltaTime must be a number greater or equal to 0")
})

test_that("input of get_record_table, deltaTimeComparedTo, is checked properly", {
  testthat::expect_error(get_record_table(mica,
                                minDeltaTime = 100,
                                deltaTimeComparedTo = NULL))
  testthat::expect_error(get_record_table(mica,
                                minDeltaTime = 100,
                                deltaTimeComparedTo = "not valid"))
})

test_that("if not integer, minDeltaTime is set to integer (floor)", {
  record_table_int <- get_record_table(mica,
                                     minDeltaTime = 1000,
                                     deltaTimeComparedTo = "lastRecord")
  record_table_dec <- get_record_table(mica,
                                      minDeltaTime = 1000.7,
                                      deltaTimeComparedTo = "lastRecord")
  testthat::expect_equal(record_table_int, record_table_dec)
})

test_that("input of get_record_table, removeDuplicateRecords, is checked properly", {
  # only TRUE or FALSE are allowed
  testthat::expect_error(get_record_table(mica,
                                removeDuplicateRecords = 5))
  testthat::expect_error(get_record_table(mica,
                                removeDuplicateRecords = NA))
})

test_that("nrows = n obs of identified individuals if minDeltaTime is 0", {
  nrow_output <- get_record_table(mica, minDeltaTime = 0) %>% nrow
  testthat::expect_equal(nrow_output,
               mica$observations %>%
                 filter(!is.na(scientificName)) %>% nrow)
})

test_that("nrows = n obs of red foxes if all other species are excluded", {
  species_to_exclude <- c("Anas platyrhynchos",
                          "Anas strepera",
                          "Ardea",
                          "Ardea cinerea",
                          "Castor fiber",
                          "Homo sapiens",
                          "Martes foina",
                          "Mustela putorius"
                          )
  nrow_foxes <- get_record_table(mica, exclude = species_to_exclude) %>%
    nrow
  testthat::expect_equal(nrow_foxes,
               mica$observations %>%
                 filter(scientificName == "Vulpes vulpes") %>% nrow)
})

test_that("Higher minDeltaTime means less rows returned", {
  nrow_delta_0 <- get_record_table(mica) %>% nrow
  nrow_delta_1000 <- get_record_table(mica,
                                      minDeltaTime = 1000,
                                      deltaTimeComparedTo = "lastRecord") %>%
    nrow
  nrow_delta_10000 <- get_record_table(mica,
                                       minDeltaTime = 10000,
                                       deltaTimeComparedTo = "lastRecord") %>%
    nrow
  testthat::expect_true(nrow_delta_1000 <= nrow_delta_0)
  testthat::expect_true(nrow_delta_10000 <= nrow_delta_1000)
})

test_that("stations names are equal to values in column passed to StationCOl", {
  # use locationName as Station
  stations <- get_record_table(mica) %>% distinct(Station) %>% pull()
  location_names <- unique(mica$deployments$locationName)
  testthat::expect_true(all(stations %in% location_names))

  # use locationID as Station
  stations <- get_record_table(mica, stationCol = "locationID") %>%
    dplyr::distinct(Station) %>%
    dplyr::pull()
  location_ids <- unique(mica$deployments$locationID)
  testthat::expect_true(all(stations %in% location_ids))
})

test_that("Directory and Filename columns are lists", {
  file_values <- get_record_table(mica) %>%
    dplyr::select(Directory, FileName)
  testthat::expect_true(class(file_values$Directory) == "list")
  testthat::expect_true(class(file_values$FileName) == "list")
})

test_that(
  paste("Directory and Filename slot contain as many values as",
        "media of independent obs"), {
  output <- get_record_table(mica)
  # add n media, observationID and sequenceID to record table
  output <- output %>%
    mutate(len = purrr::map_dbl(Directory, function(x) length(x))) %>%
    dplyr::left_join(mica$observations %>%
                dplyr::select(.data$observationID,
                              .data$timestamp,
                              .data$scientificName,
                              .data$sequenceID),
              by = c("DateTimeOriginal" = "timestamp",
                     "Species" = "scientificName"))
  n_media <-
    mica$media %>%
    group_by(sequenceID) %>%
    count()
  output <- output %>%
    dplyr::left_join(n_media,
              by = "sequenceID")
  testthat::expect_equal(output$len, output$n)
})

test_that(paste("removeDuplicateRecords allows removing duplicates,",
                "but structure output remains the same"), {
  mica_duplicates <- mica
  mica_duplicates$observations$sequenceID <- b$observations$sequenceID[1]
  mica_duplicates$observations$deploymentID <- b$observations$deploymentID[1]
  mica_duplicates$observations$timestamp <- b$observations$timestamp[1]
  mica_duplicates$observations$scientificName <- "Anas strepera"
  rec_table <- get_record_table(mica_duplicates)
  rec_table_dup <- get_record_table(mica_duplicates,
                                    removeDuplicateRecords = FALSE)
  testthat::expect_equal(nrow(rec_table), 1)
  testthat::expect_equal(
    rec_table$DateTimeOriginal, mica$observations$timestamp[1]
  )
  testthat::expect_equal(rec_table$delta.time.secs, 0)
  testthat::expect_equal(names(rec_table_dup), names(rec_table))
  testthat::expect_equal(nrow(rec_table_dup),
                         nrow(mica_duplicates$observations)
  )
})

test_that("filtering predicates are allowed and work well", {
  stations <- unique(
    get_record_table(mica, pred_lt("longitude", 4.0))$Station
  )
  stations_calculate <- mica$deployments %>%
    filter(longitude < 4.0) %>%
    pull(locationName)
  testthat::expect_identical(stations, stations_calculate)
})
