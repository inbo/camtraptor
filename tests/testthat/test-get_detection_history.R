test_that("Camera operation matrix input, `camOp`, has right format", {
  rec_table <- get_record_table(mica)
  cam_op <- get_cam_op(mica)
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
  # All rownames of camOp must be with or without `__SESS_`. No mixed names
  rownames(cam_op)[1] <- stringr::str_c(rownames(cam_op)[1], "__SESS_A")
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength),
    paste0(
      "No prefix `__SESS_` found in row names of the camera operation ",
      "matrix. If sessions are used, they must be indicated in all ",
      "rownames of camera operation matrix. Please check the camera ",
      "operation matrix."
    )
  )
  # Sessions are used, but not all rownames have the session after prefix
  # `__SESS_`.
  cam_op <- get_cam_op(mica)
  rownames(cam_op) <- stringr::str_c(rownames(cam_op), "__SESS_")
  rownames(cam_op)[1:2] <- stringr::str_c(rownames(cam_op)[1:2], "A")
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength),
    paste0(
      "No session found in some row names of the camera operation matrix. ",
      "Be sure that all row names contain a valid string after prefix ",
      "`__SESS_`. Please check the camera operation matrix."
    )
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
    paste0("Invalid `minActiveDaysPerOccasion`. If defined, it must be an ",
           "integer vector of length 1."),
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
    paste0("Invalid `minActiveDaysPerOccasion`. If defined, it must be an ",
           "integer vector of length 1."),
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
    paste0("Invalid `minActiveDaysPerOccasion`. If defined, it must be ",
           "greater than 0."),
    fixed = TRUE
  )
})

# Check `maxNumberDays`
test_that("maxNumberDays is NULL or an integer of length 1", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  species <- "Anas platyrhynchos"
  occasionLength <- 1
  # `maxNumberDays` is a character, not right class
  maxNumberDays = "blablabla"
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          maxNumberDays = maxNumberDays),
    paste0("Invalid `maxNumberDays`. If defined, it must be an integer ",
           "vector of length 1."),
    fixed = TRUE
  )
  # `maxNumberDays` is a vector with length > 1
  maxNumberDays <- c(1, 2, 4)
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          maxNumberDays = maxNumberDays),
    paste0("Invalid `maxNumberDays`. If defined, it must be an integer ",
           "vector of length 1."),
    fixed = TRUE
  )
  # `maxNumberDays` is negative
  maxNumberDays <- -1
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          maxNumberDays = maxNumberDays),
    "Invalid `maxNumberDays`. Must be greater than 0.",
    fixed = TRUE
  )
  # `maxNumberDays` is less than `occasionLength`
  maxNumberDays <- 1
  occasionLength <- 7
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          maxNumberDays = maxNumberDays),
    paste0("Invalid `maxNumberDays`. If defined, it must be greater than or ",
           "equal to `occasionLength`."),
    fixed = TRUE
  )
  # `maxNumberDays` is greater than the number of days in the camera operation
  # matrix
  maxNumberDays <- 1000
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          maxNumberDays = maxNumberDays),
    paste0(
      "Invalid `maxNumberDays`. Must be smaller than or equal to the number ",
      "of columns of `camOp`."),
    fixed = TRUE
  )
  # maxNumberDays is very short: all records are removed as they are taken after
  maxNumberDays <- 1
  occasionLength <- 1
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          day1 = "station",
                          maxNumberDays = maxNumberDays),
    paste0("All records removed because they are taken after `maxNumberDays` ",
           "(1 days). The detection history would be empty."),
    fixed = TRUE
  )
})

# Check `day1`
test_that("day1 is equal to `\"station\"` or a valid date", {
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
    paste0("Invalid `day1`. Must be equal to 'station' or a string ",
           "representing a valid date in ISO 8601 format."),
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
    paste0("Invalid `day1`. Must be a date lower or equal to the last date ",
           "in the camera operation matrix."),
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
    paste0("Invalid `day1`. Must be a date greater or equal to the first date ",
           "in the camera operation matrix."),
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
  paste0("Invalid `buffer`. If defined, ",
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
    paste0("Invalid `buffer`. If defined, ",
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
    "Invalid `buffer`. If defined, it must be 1 or higher."
  )
})

test_that("Test combination of `day1`/`buffer` and `maxNumberDays`", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  
  # Combination `day1` as date and `maxNumberDays`
  maxNumberDays <- 3
  day1 <- "2021-04-01"
  # For all stations first day (`day1`: 2021-04-01) > last day (latest date is
  # 2021-03-29 = 2021-03-27  + `maxNumberDays` - 1).
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          day1 = day1,
                          maxNumberDays = maxNumberDays),
    paste0("In all stations, the occasions begin after retrieval. ",
           "Choose an earlier `day1` date or a larger `maxNumberDays` ",
           "argument."),
    fixed = TRUE
  )
  
  # Combination `buffer` (`day1` = "station") and `maxNumberDays`
  maxNumberDays <- 3
  # `buffer` equivalent of `day1`="2020-08-03 (`B_DL_val 5_beek kleine vijver`)
  buffer <- 5
  day1 <- "station"
  # Right error returned: all records removed as first day of the station is
  # 2020-08-03 (2020-07-29 + `buffer`) and the last day is 2020-07-31
  # (2020-07-29 + `maxNumberDays`).
  expect_error(get_detection_history(recordTable = rec_table,
                                           camOp = cam_op,
                                           species = species,
                                           output = output,
                                           occasionLength = occasionLength,
                                           day1 = day1,
                                           maxNumberDays = maxNumberDays,
                                           buffer = buffer),
    paste0("In all stations, the occasions begin after retrieval. ",
           "Choose a smaller `buffer` argument or a larger `maxNumberDays` ",
           "argument."),
    fixed = TRUE
  )
  
  maxNumberDays <- 3
  day1 <- "2020-08-03"
  # Combination `day1` as date and `maxNumberDays`. In this case there is still
  # ONE deployment where last day > `day1` + `maxNumberDays` - 1 as the
  # deployment starts in 2021! So, another error is returned, when filtering
  # record table at later stage. All records of the given species are removed as
  # first day of the station is 2020-08-03 (`day1`) and the last day is
  # 2020-07-31 (2020-07-29 -> `maxNumberDays`).
  expect_error(
    suppressWarnings(get_detection_history(recordTable = rec_table,
                                           camOp = cam_op,
                                           species = species,
                                           output = output,
                                           occasionLength = occasionLength,
                                           day1 = day1,
                                           maxNumberDays = maxNumberDays)),
    paste0("All records removed. The detection history would be empty. ",
           "Check that `recordTable` contains records of the species ",
           "and that the dates are within the range of the camera ",
           "operation matrix. Check also the comibnation of arguments ",
           "`buffer`, `day1` and `maxNumberDays`."),
    fixed = TRUE
  )
})

# Check `unmarkedMultFrameInput`
test_that("unmarkedMultFrameInput is TRUE or FALSE", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  # `unmarkedMultFrameInput` is a character, not right class
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          unmarkedMultFrameInput = "blablabla"),
    "`unmarkedMultFrameInput` must be logical (`TRUE` / `FALSE`).",
    fixed = TRUE
  )
  # `unmarkedMultFrameInput` is a vector with length > 1
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          unmarkedMultFrameInput = c(TRUE, FALSE)),
    "`unmarkedMultFrameInput` must be logical (`TRUE` / `FALSE`).",
    fixed = TRUE
  )
})

# Check `unmarkedMultFrameInput`/`day1`
test_that("day1 must be `\"station\"` if unmarkedMultFrameInput is `TRUE`", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  species <- "Anas platyrhynchos"
  occasionLength <- 1
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          unmarkedMultFrameInput = TRUE,
                          day1 = "2020-08-03"),
    paste0("`day1` must be equal to `\"station\"` for multi-season detection ",
           "history (`unmarkedMultFrameInput` = `TRUE`)."),
    fixed = TRUE
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
  expect_identical(rownames(res$detection_history),rownames(cam_op))
  expect_identical(rownames(res$effort),rownames(cam_op))
  expect_identical(rownames(res$dates), rownames(cam_op))
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
  expect_identical(res_binary$dates, res_n_observations$dates)
  expect_identical(res_binary$dates, res_n_individuals$dates)
  expect_identical(res_binary$effort, res_n_observations$effort)
  expect_identical(res_binary$effort, res_n_individuals$effort)
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

test_that("Test minActiveDaysPerOccasion > 1", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "n_observations"
  occasionLength <- 7
  species <- "Anas platyrhynchos"
  res_1 <- get_detection_history(recordTable = rec_table,
                                 camOp = cam_op,
                                 species = species,
                                 output = output,
                                 occasionLength = occasionLength,
                                 day1 = "station")
  # Get number of NAs per rows in detection history matrix
  n_na_per_row_1_det_hist <- rowSums(is.na(res_1$detection_history))
  # Get number of NAs per rows in effort matrix
  n_na_per_row_1_effort <- rowSums(is.na(res_1$effort))
  # Get number of NAs per rows in dates matrix
  n_na_per_row_1_dates <- rowSums(is.na(res_1$dates))
  
  minActiveDaysPerOccasion <- 5 # less than `occasionLength`
  res_5 <- get_detection_history(
    recordTable = rec_table,
    camOp = cam_op,
    species = species,
    output = output,
    occasionLength = occasionLength,
    minActiveDaysPerOccasion = minActiveDaysPerOccasion,
    day1 = "station")
  # Get number of NAs per rows in detection history matrix
  n_na_per_row_5_det_hist <- rowSums(is.na(res_5$detection_history))
  # Get number of NAs per rows in effort matrix
  n_na_per_row_5_effort <- rowSums(is.na(res_5$effort))
  # Get number of NAs per rows in dates matrix
  n_na_per_row_5_dates <- rowSums(is.na(res_5$dates))
  
  # Number of NAs for each row (station) in res_5 is higher or equal to the
  # number of NAs for the same row in res_1 (as we are looking for more active
  # days)
  expect_true(all(n_na_per_row_5_det_hist >= n_na_per_row_1_det_hist))
  expect_true(all(n_na_per_row_5_effort > n_na_per_row_1_effort))
  expect_true(all(n_na_per_row_5_dates > n_na_per_row_1_dates))
  
  # Same number of NAs for each row (station) in res_5 for detection history,
  # effort and dates, doesn't matter the value of `minActiveDaysPerOccasion`
  expect_identical(n_na_per_row_1_det_hist, n_na_per_row_1_effort)
  expect_identical(n_na_per_row_1_det_hist, n_na_per_row_1_dates)
  expect_identical(n_na_per_row_5_det_hist, n_na_per_row_5_effort)
  expect_identical(n_na_per_row_5_det_hist, n_na_per_row_5_dates)
})

test_that("Test maxNumberDays", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  output <- "binary"
  species <- "Anas platyrhynchos"
  occasionLength <- 1
  maxNumberDays <- 2
  # Right error returned because all records are removed: first record of Anas
  # platyrhynchos occurs on 2020-07-31, the third day of the station.
  expect_error(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          day1 = "station",
                          maxNumberDays = maxNumberDays),
    paste0("All records removed because they are taken after `maxNumberDays` ",
           "(2 days). The detection history would be empty."),
    fixed = TRUE
  )
  maxNumberDays <- 3
  # Right warning returned with number of records removed and example. The first
  # record of Anas platyrhynchos occurs on 2020-07-31, the third day of the
  # station. All the other records are removed.
  expect_warning(
    get_detection_history(recordTable = rec_table,
                          camOp = cam_op,
                          species = species,
                          output = output,
                          occasionLength = occasionLength,
                          day1 = "station",
                          maxNumberDays = maxNumberDays),
    paste0("3 record(s) (out of 4) are removed because they were taken ",
           "after `maxNumberDays` (3 days) the first day of each station, ",
           "e.g.:\nB_DL_val 5_beek kleine vijver: 2020-08-02."
    ),
    fixed = TRUE
  )
  
  # 7 rows returned if `maxNumberDays`: 7 (`occasionLength`  = 1 day)
  occasionLength <- 1
  maxNumberDays <- 7
  res_max_days_7 <- get_detection_history(
    recordTable = rec_table,
    camOp = cam_op,
    species = species,
    output = output,
    occasionLength = occasionLength,
    day1 = "station",
    maxNumberDays = maxNumberDays
  )
  expect_equal(ncol(res_max_days_7$detection_history), maxNumberDays)
  expect_equal(ncol(res_max_days_7$effort),maxNumberDays)
  expect_equal(ncol(res_max_days_7$dates), maxNumberDays)
  
  # ncols = maxNumberDays / occasionLength. Example: 3 columns returned if
  # `maxNumberDays` = 6 and`occasionLength` = 2 days.
  occasionLength <- 2
  maxNumberDays <- 6
  res_max_days_6 <- suppressWarnings(
    get_detection_history(
      recordTable = rec_table,
      camOp = cam_op,
      species = species,
      output = output,
      occasionLength = occasionLength,
      day1 = "station",
      maxNumberDays = maxNumberDays
    )
  )
  expect_equal(ncol(res_max_days_6$detection_history),
               maxNumberDays / occasionLength
  )
  expect_equal(ncol(res_max_days_6$effort), maxNumberDays / occasionLength)
  expect_equal(ncol(res_max_days_6$dates),maxNumberDays / occasionLength)
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
  output <- "n_observations"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  # Error returned if `buffer` is so big that no occasions are found.
  buffer <- 1000
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength,
                                     day1 = "station",
                                     buffer = buffer),
    paste0("In all stations, the occasions begin after retrieval. ",
           "Choose a smaller `buffer` argument.")
  )
  # Error returned if `buffer` is so big that all records for given species are
  # removed.
  buffer <- 10
  expect_error(get_detection_history(recordTable = rec_table,
                                     camOp = cam_op,
                                     species = species,
                                     output = output,
                                     occasionLength = occasionLength,
                                     day1 = "station",
                                     buffer = buffer),
               paste0("No more records after removing records before survey ",
                      "begin. The detection history would be empty.")
  )
  # Right warning returned with number of removed records and an example
  buffer <- 5
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
  # `buffer`. We check first row only, the one containing records of Anas platyrhynchos.
  expect_true(
    all(res_with_buffer$dates[1,] >= "2020-08-03" | 
          is.na(res_with_buffer$dates[1,])
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
  # If `occasionLength` is equal to 1, the detection history is shifted by
  # buffer days. But do not compare the columns names as the ones with no buffer
  # have higher numbers (higher number of occasions)
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
  occasionLength <- 3
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
  
  # If `maxNumberDays` is defined and `occasionLength` = 1:
  # ncols = `maxNumberDays` - `buffer`
  buffer <- 2
  maxNumberDays <- 5
  occasionLength <- 1
  res_max_buffer <- suppressWarnings(
    get_detection_history(
      recordTable = rec_table,
      camOp = cam_op,
      species = species,
      output = output,
      occasionLength = occasionLength,
      day1 = "station",
      buffer = buffer,
      maxNumberDays = maxNumberDays
    )
  )
  expect_equal(ncol(res_max_buffer$detection_history), maxNumberDays - buffer)
  expect_equal(ncol(res_max_buffer$effort), maxNumberDays - buffer)
  expect_equal(ncol(res_max_buffer$dates), maxNumberDays - buffer)
  
  # If `maxNumberDays` is defined and `occasionLength` = 2:
  # ncols = (`maxNumberDays` - `buffer`) / `occasionLength`
  occasionLength <- 2
  maxNumberDays <- 6
  buffer <- 2
  res_max_buffer <- suppressWarnings(
    get_detection_history(
      recordTable = rec_table,
      camOp = cam_op,
      species = species,
      output = output,
      occasionLength = occasionLength,
      day1 = "station",
      buffer = buffer,
      maxNumberDays = maxNumberDays
    )
  )
  expect_equal(ncol(res_max_buffer$detection_history),
               (maxNumberDays - buffer) / occasionLength
  )
  expect_equal(ncol(res_max_buffer$effort),
               (maxNumberDays - buffer) / occasionLength
  )
  expect_equal(ncol(res_max_buffer$dates),
               (maxNumberDays - buffer) / occasionLength
  )
})

test_that("Test unmarkedMultFrameInput", {
  output <- "binary"
  occasionLength <- 1
  species <- "Anas platyrhynchos"
  # Create a multi-season camera operation matrix / record table
  mica_sessions <- mica
  mica_sessions$data$deployments$session <- c("2020", "2020", "2021", "2021")
  mica_sessions$data$deployments$locationID <- c(
    mica_sessions$data$deployments$locationID[1:2],
    mica_sessions$data$deployments$locationID[1:2]
  )
  mica_sessions$data$deployments$locationName <- c(
   mica_sessions$data$deployments$locationName[1:2],
   mica_sessions$data$deployments$locationName[1:2]
  )
  lubridate::year(mica_sessions$data$deployments$start[4]) <- 2021
  lubridate::year(mica_sessions$data$deployments$end[4]) <- 2021
  lubridate::year(mica_sessions$data$observations$timestamp) <- dplyr::if_else(
    lubridate::year(mica_sessions$data$observations$timestamp) == 2019,
    2021,
    lubridate::year(mica_sessions$data$observations$timestamp)
  )
  lubridate::year(mica_sessions$data$media$timestamp) <-  dplyr::if_else(
    lubridate::year(mica_sessions$data$media$timestamp) == 2019,
    2021,
    lubridate::year(mica_sessions$data$media$timestamp)
  )
  camOp_sessions <- get_cam_op(mica_sessions, session_col = "session")
  recordTable_sessions <- get_record_table(mica_sessions)
  # No multi-season detection history (`unmarkedMultFrameInput` = `FALSE`)
  # requested: all stations/seasons returned as they would be different
  # stations.
  no_multi_season <- get_detection_history(
    recordTable_sessions,
    camOp_sessions,
    species = species,
    output = output,
    unmarkedMultFrameInput = FALSE
  )
  # Multi-season detection history (`unmarkedMultFrameInput` = `TRUE`)
  multi_season <- get_detection_history(
    recordTable_sessions,
    camOp_sessions,
    species = species,
    output = output,
    unmarkedMultFrameInput = TRUE
  )
  # Run a standard detection history with basic example dataset
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  standard <- get_detection_history(
    recordTable = rec_table,
    camOp = cam_op,
    species = species,
    output = output
  )
  
  # Check output of detection history without taking into account the seasons.
  # Rownames are the same as the camera operation matrix
  expect_identical(
    rownames(no_multi_season$detection_history),
    rownames(camOp_sessions)
  )
  expect_identical(
    rownames(no_multi_season$effort),
    rownames(camOp_sessions)
  )
  expect_identical(
    rownames(no_multi_season$dates),
    rownames(camOp_sessions)
  )
  # Content of detection history and effort is the same as the standard
  # detection history and correspondent effort. The dates are the same only for
  # the first two rows by construction.
  expect_identical(
    unname(no_multi_season$detection_history),
    unname(standard$detection_history)
  )
  expect_identical(
    unname(no_multi_season$effort),
    unname(standard$effort)
  )
  expect_identical(
    unname(no_multi_season$dates[1:2,]),
    unname(standard$dates[1:2,])
  )
  # Check output of detection history taking into account the seasons.
  # Number of rows is equal to the number of stations, which is 2
  expect_equal(nrow(multi_season$detection_history), 2)
  expect_equal(nrow(multi_season$effort), 2)
  expect_equal(nrow(multi_season$dates), 2)
  # Number of columns is equal to the highest number of days of a
  # station/season multiplied by number of seasons, which is 2.
  n_cols_standard <- ncol(standard$detection_history)
  n_cols <- n_cols_standard * 2
  expect_equal(ncol(multi_season$detection_history), n_cols)
  expect_equal(ncol(multi_season$effort), n_cols)
  expect_equal(ncol(multi_season$dates), n_cols)
  # Output of first half of columns is equal to first two rows of output without
  # season.
  expect_identical(
    unname(
      multi_season$detection_history[, 1:n_cols_standard]
    ),
    unname(standard$detection_history[1:2, ])
  )
  expect_identical(
    unname(multi_season$effort[, 1:n_cols_standard]),
    unname(standard$effort[1:2, ])
  )
  expect_identical(
    unname(multi_season$dates[, 1:n_cols_standard]),
    unname(standard$dates[1:2, ])
  )
  # Output of second half of columns is equal to the second two rows of output
  expect_identical(
    unname(
      multi_season$detection_history[, (n_cols_standard + 1):n_cols]
    ),
    unname(standard$detection_history[3:4, ])
  )
  
  # Multi-season works with `occasionLength` > 1
  occasionLength <- 2
  multi_season <- get_detection_history(
    recordTable_sessions,
    camOp_sessions,
    species = species,
    output = output,
    unmarkedMultFrameInput = TRUE,
    occasionLength = occasionLength
  )
  standard <- get_detection_history(
    recordTable = rec_table,
    camOp = cam_op,
    species = species,
    output = output,
    occasionLength = occasionLength
  )
  # Number of rows is equal to the number of stations, which is 2
  # Number of rows is equal to the number of stations, which is 2
  expect_equal(nrow(multi_season$detection_history), 2)
  expect_equal(nrow(multi_season$effort), 2)
  expect_equal(nrow(multi_season$dates), 2)
  # Number of columns is equal to the highest number of days of a
  # station/season multiplied by number of seasons, which is 2.
  n_cols_standard <- ncol(standard$detection_history)
  n_cols <- n_cols_standard * 2
  expect_equal(ncol(multi_season$detection_history), n_cols)
  expect_equal(ncol(multi_season$effort), n_cols)
  expect_equal(ncol(multi_season$dates), n_cols)
  # Output of first half of columns is equal to first two rows of output without
  # season.
  expect_identical(
    unname(
      multi_season$detection_history[, 1:n_cols_standard]
    ),
    unname(standard$detection_history[1:2, ])
  )
  expect_identical(
    unname(multi_season$effort[, 1:n_cols_standard]),
    unname(standard$effort[1:2, ])
  )
  expect_identical(
    unname(multi_season$dates[, 1:n_cols_standard]),
    unname(standard$dates[1:2, ])
  )
  
  # Multi-season works with `buffer`
  buffer <- 2
  occasionLength <- 1 # Set back to 1 to test `buffer` independently
  multi_season <- get_detection_history(
    recordTable_sessions,
    camOp_sessions,
    species = species,
    output = output,
    unmarkedMultFrameInput = TRUE,
    occasionLength = occasionLength,
    buffer = buffer
  )
  standard <- get_detection_history(
    recordTable = rec_table,
    camOp = cam_op,
    species = species,
    output = output,
    occasionLength = occasionLength,
    buffer = buffer
  )
  # Number of rows is equal to the number of stations, which is 2
  expect_equal(nrow(multi_season$detection_history), 2)
  expect_equal(nrow(multi_season$effort), 2)
  expect_equal(nrow(multi_season$dates), 2)
  # Number of columns is equal to the highest number of days of a
  # station/season multiplied by number of seasons, which is 2.
  n_cols_standard <- ncol(standard$detection_history)
  n_cols <- n_cols_standard * 2
  expect_equal(ncol(multi_season$detection_history), n_cols)
  expect_equal(ncol(multi_season$effort), n_cols)
  expect_equal(ncol(multi_season$dates), n_cols)
  # Output of first half of columns is equal to first two rows of output without
  # season.
  expect_identical(
    unname(
      multi_season$detection_history[, 1:n_cols_standard]
    ),
    unname(standard$detection_history[1:2, ])
  )
  expect_identical(
    unname(multi_season$effort[, 1:n_cols_standard]),
    unname(standard$effort[1:2, ])
  )
  expect_identical(
    unname(multi_season$dates[, 1:n_cols_standard]),
    unname(standard$dates[1:2, ])
  )
  
})