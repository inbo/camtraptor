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
  expect_error(get_record_table(mica, minDeltaTime = "1"),
               "minDeltaTime must be a number greater or equal to 0")
  expect_error(get_record_table(mica, minDeltaTime = -10),
               "minDeltaTime must be a number greater or equal to 0")
})

test_that("input of get_record_table, deltaTimeComparedTo, is checked properly", {
  expect_error(get_record_table(mica,
                                minDeltaTime = 100,
                                deltaTimeComparedTo = NULL))
  expect_error(get_record_table(mica,
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
  expect_equal(record_table_int, record_table_dec)
})

test_that("nrows = n obs of identified individuals if minDeltaTime is 0", {
  nrow_output <- get_record_table(mica, minDeltaTime = 0) %>% nrow
  expect_equal(nrow_output,
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
  expect_equal(nrow_foxes,
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
  expect_true(nrow_delta_1000 <= nrow_delta_0)
  expect_true(nrow_delta_10000 <= nrow_delta_1000)
})

test_that("stations names are equal to values in column passed to StationCOl", {
  # use locationName as Station
  stations <- get_record_table(mica) %>% distinct(Station) %>% pull()
  location_names <- unique(mica$deployments$locationName)
  expect_true(all(stations %in% location_names))

  # use locationID as Station
  stations <- get_record_table(mica, stationCol = "locationID") %>%
    distinct(Station) %>%
    pull()
  location_ids <- unique(mica$deployments$locationID)
  expect_true(all(stations %in% location_ids))
})

test_that("Directory and Filename columns are lists", {
  file_values <- get_record_table(mica) %>%
    select(Directory, FileName)
  expect_true(class(file_values$Directory) == "list")
  expect_true(class(file_values$FileName) == "list")
})

test_that("Each Directory and Filename slot contains as many values as media linked to the independent obs", {
  output <- get_record_table(mica)
  # add n media, observationID and sequenceID to record table
  output <- output %>%
    mutate(len = purrr::map_dbl(Directory, function(x) length(x))) %>%
    left_join(mica$observations %>%
                select(observationID,
                       timestamp,
                       scientificName,
                       sequenceID),
              by = c("DateTimeOriginal" = "timestamp",
                     "Species" = "scientificName"))
  n_media <-
    mica$media %>%
    group_by(sequenceID) %>%
    count()
  output <- output %>%
    left_join(n_media,
              by = "sequenceID")
  expect_equal(output$len, output$n)
})

test_that("filtering predicates are allowed and work well", {
  stations <- unique(
    get_record_table(mica, pred_lt("longitude", 4.0))$Station
  )
  stations_calculate <- mica$deployments %>%
    filter(longitude < 4.0) %>% 
    pull(locationName)
  expect_identical(stations, stations_calculate)
})
