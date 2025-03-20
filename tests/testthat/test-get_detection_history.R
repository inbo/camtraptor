test_that("Camera operation matrix input, `camOp`, has right format", {
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- "binary"
  occasionLength <- 1
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = NULL,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength),
               "`camOp` must be a matrix.",
               fixed = TRUE
  )
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = "not a matrix",
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength),
               "`camOp` must be a matrix.",
               fixed = TRUE
  )
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = dplyr::as_tibble(cam_op),
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength),
               "`camOp` must be a matrix.",
               fixed = TRUE
  )
}
)

test_that("Record table input, `recordTable`, has right format", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- "binary"
  occasionLength <- 1
  expect_error(get_detection_history(recordTable = NULL,
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength),
               "`recordTable` must be a tibble data.frame.",,
               fixed = TRUE
  )
  expect_error(get_detection_history(recordTable = "not a tibble",
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength),
               "`recordTable` must be a tibble data.frame.",,
               fixed = TRUE
  )
  expect_error(get_detection_history(recordTable = as.matrix(rec_table),
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength),
               "`recordTable` must be a tibble data.frame.",
               fixed = TRUE
  )
  
  rec_table_no_n <- rec_table
  rec_table_no_n$n <- NULL
  expect_error(
    get_detection_history(recordTable = rec_table_no_n,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength),
    paste0("Invalid record table. Must contain at least the columns: ",
           "`Station`, `Date`, `Species` and `n`."),
    fixed = TRUE
  )
}
)

test_that("Check species", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = NULL,
                                     output = output,
                                     occasionLength = occasionLength),
               "`species` must be a character vector of lenght 1.",
               fixed = TRUE
  )
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = 1,
                                     output = output,
                                     occasionLength = occasionLength),
               "`species` must be a character vector of lenght 1.",
               fixed = TRUE
  )
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = c("Anas platyrhynchos", "Anas strepera"),
                          output = output,
                          occasionLength = occasionLength),
    "`species` must be a character vector of lenght 1.",
    fixed = TRUE
  )
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = "not a species",
                          output = output,
                          occasionLength = occasionLength),
    paste0("Invalid value for species parameter: not a species.\n",
           "Valid inputs are: Anas platyrhynchos, Anas strepera, Ardea, ",
           "Ardea cinerea, Castor fiber, Homo sapiens, Martes foina, ",
           "Mustela putorius and Vulpes vulpes"),
    fixed = TRUE
  )
})

test_that("Check output", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- 5
  occasionLength <- 1
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = "Anas platyrhynchos",
                                     output = output,
                                     occasionLength = occasionLength,
                                     day1 = "station"),
               "`output` must be a character vector of lenght 1.",
               fixed = TRUE
  )
  output <- c(5,2)
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = "Anas platyrhynchos",
                                     output = output,
                                     occasionLength = occasionLength,
                                     day1 = "station"),
               "`output` must be a character vector of lenght 1.",
               fixed = TRUE
  )
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- "wrong"
  occasionLength <- 1
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = "Anas platyrhynchos",
                          output = output,
                          occasionLength = occasionLength,
                          day1 = "station"),
    paste0("Invalid value for output parameter: wrong.\n",
           "Valid inputs are: binary, n_observations and n_individuals"),
    fixed = TRUE
  )
})

test_that("Check occasionLength", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- "binary"
  occasionLength <- "not an integer"
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength,
                                     day1 = "station"),
               "Invalid `occasionLength`. Must be an integer vector of length 1.",
               fixed = TRUE
  )
  occasionLength <- c(1,2)
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          day1 = "station"),
    "Invalid `occasionLength`. Must be an integer vector of length 1.",
    fixed = TRUE
  )
  occasionLength <- -1
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength,
                                     day1 = "station"),
               "Invalid `occasionLength`. Must be greater than 0.",
               fixed = TRUE
  )
})

test_that("Check minActiveDaysPerOccasion", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- "binary"
  occasionLength <- 5
  minActiveDaysPerOccasion <- "not an integer"
  expect_error(
    get_detection_history(
      recordTable = rec_table,
      camOp = cam_op,
      species = species,
      output = output,
      occasionLength = occasionLength,
      minActiveDaysPerOccasion = minActiveDaysPerOccasion,
      day1 = "station"
    ),
    "Invalid `minActiveDaysPerOccasion`. Must be an integer vector of length 1.",
    fixed = TRUE
  )
  minActiveDaysPerOccasion <- c(1,2)
  expect_error(
    get_detection_history(
      recordTable = rec_table,
      camOp = cam_op,
      species = species,
      output = output,
      occasionLength = occasionLength,
      minActiveDaysPerOccasion = minActiveDaysPerOccasion,
      day1 = "station"
    ),
    paste0("Invalid `minActiveDaysPerOccasion`. Must be an integer vector ",
           "of length 1."),
    fixed = TRUE
  )
  minActiveDaysPerOccasion <- -1
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          minActiveDaysPerOccasion = minActiveDaysPerOccasion,
                          day1 = "station"),
    "Invalid `minActiveDaysPerOccasion`. Must be greater than 0.",
    fixed = TRUE
  )
})

# Check `day1`
test_that("day1 is equal station or a valid date", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          day1 = "not a station"),
    paste0("`day1` must be equal to 'station' or a string representing a ",
           "valid date in ISO 8601 format."
           ),
    fixed = TRUE
  )
  # `day1` is too late
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          day1 = "2100-01-01"),
    paste0("`day1` must be a date lower or equal to the last date ",
           "in the camera operation matrix."
    ),
    fixed = TRUE
  )
  # `day1` is too early
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          day1 = "2000-01-01"),
    paste0("`day1` must be a date greater or equal to the first date ",
           "in the camera operation matrix."
    ),
    fixed = TRUE
  )
})


# Check `buffer`
test_that("buffer is NULL or an integer of length 1", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  # `buffer` is a character, not right class
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          buffer = "blablabla"),
  paste0("Invalid `buffer`. If buffer is defined, ",
         "it must be an integer of length 1.")
  )
  # `buffer` is a vector with length > 1 
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          buffer = c(1, 2, 4)),
    paste0("Invalid `buffer`. If buffer is defined, ",
           "it must be an integer of length 1.")
  )
  # `buffer` is negative
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          buffer = -1),
    "Invalid `buffer`. If `buffer` is defined, it must be 1 or higher."
  )
})


# Test output ####

test_that("Output is a list of three matrices", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  res <- get_detection_history(recordTable = rec_table,
                               camOp = cam_op,
                               species = species,
                               output = output,
                               occasionLength = occasionLength,
                               day1 = "station")
  expect_type(res, "list")
  expect_length(res, 3)
  expect_true(is.matrix(res$detection_history))
  expect_true(is.matrix(res$effort))
  expect_true(is.matrix(res$dates))
  expect_type(res$detection_history, "double")
  expect_type(res$effort, "double")
  expect_type(res$dates, "character")
  expect_equal(
    rownames(res$detection_history),
    c("B_DL_val 3_dikke boom",
      "B_DL_val 5_beek kleine vijver",
      "B_DM_val 4_'t WAD",
      "Mica Viane")
  )
  expect_equal(
    rownames(res$effort),
    c("B_DL_val 3_dikke boom",
      "B_DL_val 5_beek kleine vijver",
      "B_DM_val 4_'t WAD",
      "Mica Viane")
  )
  expect_equal(
    rownames(res$dates),
    c("B_DL_val 3_dikke boom",
      "B_DL_val 5_beek kleine vijver",
      "B_DM_val 4_'t WAD",
      "Mica Viane")
  )
})

test_that("detection history dates and effort are output independent", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  res_binary <- get_detection_history(recordTable = rec_table,
                                      camOp = cam_op,
                                      species = species,
                                      output = output,
                                      occasionLength = occasionLength,
                                      day1 = "station")
  output <- "n_observations"
  res_n_observations <- get_detection_history(recordTable = rec_table,
                                              camOp = cam_op,
                                              species = species,
                                              output = output,
                                              occasionLength = occasionLength,
                                              day1 = "station")
  output <- "n_individuals"
  res_n_individuals <- get_detection_history(recordTable = rec_table,
                                             camOp = cam_op,
                                             species = species,
                                             output = output,
                                             occasionLength = occasionLength,
                                             day1 = "station")
  expect_equal(res_binary$dates, res_n_observations$dates)
  expect_equal(res_binary$dates, res_n_individuals$dates)
  expect_equal(res_binary$effort, res_n_observations$effort)
  expect_equal(res_binary$effort, res_n_individuals$effort)
})

test_that("dates are in the right ISO format (YYY-MM-DD)", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  res <- get_detection_history(recordTable = rec_table,
                               camOp = cam_op,
                               species = species,
                               output = output,
                               occasionLength = occasionLength,
                               day1 = "station")
  # Check if all dates are in the right format or NA
  expect_true(all(is.na(res$dates) | grepl("\\d{4}-\\d{2}-\\d{2}", res$dates)))
  
  # It is the same also with occasionLength > 1
  occasionLength <- 2
  res <- get_detection_history(recordTable = rec_table,
                               camOp = cam_op,
                               species = species,
                               output = output,
                               occasionLength = occasionLength,
                               day1 = "station")
  # Check if all dates are in the right format or NA
  expect_true(all(is.na(res$dates) | grepl("\\d{4}-\\d{2}-\\d{2}", res$dates)))
})

test_that(
  paste0("dates are the same as the colnames of camera operation matrix ",
         "with at least one value not NA (occasionLength = 1)"), 
  {
    cam_op <- get_cam_op(mica)
    # Remove columns with NAs only
    cam_op_without_na <- cam_op[, colSums(is.na(cam_op)) != nrow(cam_op)]
    rec_table <- get_record_table(mica)
    output <- "binary"
    occasionLength <- 1
    species <- "Anas platyrhynchos"
    res <- get_detection_history(recordTable = rec_table,
                                 camOp = cam_op,
                                 species = species,
                                 output = output,
                                 occasionLength = occasionLength,
                                 day1 = "station")
    expect_true(
      all(
        sort(unique(res$dates[!is.na(res$dates)])) == 
          sort(colnames(cam_op_without_na))
      )
    )
})

test_that("Test occasionLength > 1", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  res_1 <- get_detection_history(recordTable = rec_table,
                                 camOp = cam_op,
                                 species = species,
                                 output = output,
                                 occasionLength = occasionLength,
                                 day1 = "station")
  n_occasions_1 <- ncol(res_1$detection_history)
  occasionLength <- 7
  res_7 <- get_detection_history(recordTable = rec_table,
                                 camOp = cam_op,
                                 species = species,
                                 output = output,
                                 occasionLength = occasionLength,
                                 day1 = "station")
  n_occasions_7 <- ncol(res_7$detection_history)
  expect_true(n_occasions_1 > n_occasions_7)
  
  # All dates in res_7$dates are present in res_1$dates
  expect_true(all(res_7$dates %in% res_1$dates))
  
  # Number of occasions in res_7 is the ceiling of the number of occasions in
  # res_1 divided by occasionLength
  expect_equal(n_occasions_7, ceiling(n_occasions_1 / occasionLength))
  
  # Effort is always less or equal to the occasion length or NA
  expect_true(all(res_7$effort <= occasionLength | is.na(res_7$effort)))
  
  # The total effort is not dependent on the occasion length
  expect_equal(
    sum(res_1$effort, na.rm = TRUE), sum(res_7$effort, na.rm = TRUE)
  )
  
  # The total effort per station is not dependent on the occasion length
  expect_equal(
    rowSums(res_1$effort, na.rm = TRUE), rowSums(res_7$effort, na.rm = TRUE)
  )
})

test_that("Test day1 = specific date", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  # Right warning returned with number of records removed and example
  expect_warning(
    res_1 <- get_detection_history(recordTable = rec_table,
                                   camOp = cam_op,
                                   species = species,
                                   output = output,
                                   occasionLength = occasionLength,
                                   day1 = "2020-08-03"),
    paste0("2 record(s) (out of 4) are removed because they were taken ",
    "before `day1` (2020-08-03), e.g.:\n",
    "B_DL_val 5_beek kleine vijver: 2020-07-31."
    ),
    fixed = TRUE
  )
  # All dates are more recent than `day1` 
  expect_true(all(res_1$dates >= "2020-08-03" | is.na(res_1$dates)))
})

test_that("Test `buffer`", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  buffer <- 5
  # Error returned if `buffer` is so big that no occasions are found.
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength,
                                     day1 = "station",
                                     buffer = 1000),
    paste0("In all stations, the occasions begin after retrieval. ",
           "Choose a smaller buffer argument.")
  )
  # Right warning returned with number of removed records and an example
  expect_warning(
    res_with_buffer <- get_detection_history(recordTable = rec_table,
                                   camOp = cam_op,
                                   species = species,
                                   output = output,
                                   occasionLength = occasionLength,
                                   day1 = "station",
                                   buffer = buffer),
    paste0("2 record(s) (out of 4) are removed because they were taken ",
           "during the buffer period of 5 day(s), e.g.:\n",
           "B_DL_val 5_beek kleine vijver: 2020-07-31."
    ),
    fixed = TRUE
  )
  # All dates are more recent than `start` of deployments +
  # `buffer`. We check second row only, the one containing records of Anas platyrhynchos.
  expect_true(
    all(res_with_buffer$dates[2,] >= "2020-08-03" | 
          is.na(res_with_buffer$dates[2,])
    )
  )
  
  # Number of columns is reduced by buffer
  res_no_buffer <- get_detection_history(recordTable = rec_table,
                                         camOp = cam_op,
                                         species = species,
                                         output = output,
                                         occasionLength = occasionLength,
                                         day1 = "station",
                                         buffer = NULL)
  n_cols_no_buffer <- ncol(res_no_buffer$detection_history)
  # The detection history is shifted by buffer days. But do not compare the
  # columns names as the ones with no buffer have higher numbers (higher number
  # of occasions)
  expect_identical(
    unname(res_with_buffer$detection_history),
    unname(res_no_buffer$detection_history[, (buffer+1):n_cols_no_buffer])
  )
  expect_identical(
    unname(res_with_buffer$effort),
    unname(res_no_buffer$effort[, (buffer+1):n_cols_no_buffer])
  )
  expect_identical(
    unname(res_with_buffer$dates),
    unname(res_no_buffer$dates[, (buffer+1):n_cols_no_buffer])
  )
  
  # If `occasionLength` is higher than 1, the situation is more complex and not
  # easy to test. However, if `buffer` is equal to `occasionLength`, then we
  # have one occasion less.
  occasionLength <- 7
  expect_warning(
    res_buffer_occasion_length <- get_detection_history(
      recordTable = rec_table,
      camOp = cam_op,
      species = species,
      output = output,
      occasionLength = occasionLength,
      day1 = "station",
      buffer = occasionLength
    )
  )
  res_no_buffer_occasion_length <- get_detection_history(
    recordTable = rec_table,
    camOp = cam_op,
    species = species,
    output = output,
    occasionLength = occasionLength,
    day1 = "station",
    buffer = NULL
  )
  n_cols_no_buffer <- ncol(res_no_buffer_occasion_length$detection_history)
  expect_identical(
    unname(res_buffer_occasion_length$detection_history),
    unname(
      res_no_buffer_occasion_length$detection_history[, 2:n_cols_no_buffer]
    )
  )
  expect_identical(
    unname(res_buffer_occasion_length$effort),
    unname(res_no_buffer_occasion_length$effort[, 2:n_cols_no_buffer])
  )
  expect_identical(
    unname(res_buffer_occasion_length$dates),
    unname(res_no_buffer_occasion_length$dates[, 2:n_cols_no_buffer])
  )
})