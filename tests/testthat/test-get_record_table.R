test_that("input of get_record_table, camtrap dp, is checked properly", {
  expect_error(get_record_table("aaa"))
  expect_error(get_record_table(1))
})

test_that("input of get_record_table, stationCol, is checked properly", {
  expect_error(get_record_table(camtrapdp, stationCol = "aaa"))
})

test_that("input of get_record_table, exclude, is checked properly", {
  expect_error(get_record_table(camtrapdp, exclude = "rattus not existing"))
})

test_that("input of get_record_table, minDeltaTime, is checked properly", {
  expect_error(get_record_table(camtrapdp, minDeltaTime = "1"),
               "minDeltaTime must be a number greater or equal to 0")
  expect_error(get_record_table(camtrapdp, minDeltaTime = -10),
               "minDeltaTime must be a number greater or equal to 0")
})

test_that("input of get_record_table, deltaTimeComparedTo, is checked properly", {
  expect_error(get_record_table(camtrapdp,
                                minDeltaTime = 100,
                                deltaTimeComparedTo = NULL))
  expect_error(get_record_table(camtrapdp,
                                minDeltaTime = 100,
                                deltaTimeComparedTo = "not valid"))
})

test_that("if not integer, minDeltaTime is set to integer (floor)", {
  record_table_int <- get_record_table(camtrapdp,
                                     minDeltaTime = 1000,
                                     deltaTimeComparedTo = "lastRecord")
  record_table_dec <- get_record_table(camtrapdp,
                                      minDeltaTime = 1000.7,
                                      deltaTimeComparedTo = "lastRecord")
  expect_equal(record_table_int, record_table_dec)
})

test_that("nrows = n obs of identified individuals if minDeltaTime is 0", {
  nrow_output <- get_record_table(camtrapdp, minDeltaTime = 0) %>% nrow
  expect_equal(nrow_output,
               camtrapdp$observations %>% 
                 filter(!is.na(scientific_name)) %>% nrow)
})

test_that("nrows = n obs of mallards if all other species are excluded", {
  species_to_exclude <- c("Norway Rat",
                          "Muskrat",
                          "Coypu",
                          "Common Moorhen")
  nrow_mallards <- get_record_table(camtrapdp, exclude = species_to_exclude) %>% 
    nrow
  expect_equal(nrow_mallards,
               camtrapdp$observations %>% 
                 filter(vernacular_name == "Mallard") %>% nrow)
})

test_that("Higher minDeltaTime means less rows returned", {
  nrow_delta_0 <- get_record_table(camtrapdp) %>% nrow
  nrow_delta_1000 <- get_record_table(camtrapdp,
                                      minDeltaTime = 1000,
                                      deltaTimeComparedTo = "lastRecord") %>% 
    nrow
  nrow_delta_10000 <- get_record_table(camtrapdp,
                                       minDeltaTime = 10000,
                                       deltaTimeComparedTo = "lastRecord") %>% 
    nrow
  expect_true(nrow_delta_1000 <= nrow_delta_0)
  expect_true(nrow_delta_10000 <= nrow_delta_1000)
})

test_that("stations names are equal to values in column passed to StationCOl", {
  # use location_name as Station
  stations <- get_record_table(camtrapdp) %>% distinct(Station) %>% pull()
  location_names <- unique(camtrapdp$deployments$location_name)
  expect_true(all(stations %in% location_names))
  
  # use location_id as Station
  stations <- get_record_table(camtrapdp, stationCol = "location_id") %>% 
    distinct(Station) %>% 
    pull()
  location_ids <- unique(camtrapdp$deployments$location_id)
  expect_true(all(stations %in% location_ids))
})

test_that("Directory and Filename columns are lists", {
  file_values <- get_record_table(camtrapdp) %>% 
    select(Directory, Filename)
  expect_true(class(file_values$Directory) == "list")
  expect_true(class(file_values$Filename) == "list")
})

test_that("Each Directory and Filename slot contains as many values as multimedia linked to the independent obs", {
  output <- get_record_table(camtrapdp)
  # add n multimedia, observation_id and sequence_id to record table
  output <- output %>%
    mutate(len = purrr::map_dbl(Directory, function(x) length(x))) %>%
    left_join(camtrapdp$observations %>%
                select(observation_id,
                       timestamp,
                       scientific_name,
                       sequence_id),
              by = c("DateTimeOriginal" = "timestamp",
                     "Species" = "scientific_name"))
  n_multimedia <- 
    camtrapdp$multimedia %>%
    group_by(sequence_id) %>%
    count()
  output <- output %>%
    left_join(n_multimedia,
              by = "sequence_id")
  expect_equal(output$len, output$n)
})
