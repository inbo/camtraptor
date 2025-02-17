test_that("Camera operation matrix input, `cam_op`, has right format", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- "binary"
  occasion_length <- 1
  expect_error(get_detection_history(cam_op = NULL,
                                     rec_table = rec_table,
                                     species = species,
                                     output = output,
                                     occasion_length = occasion_length),
               "`cam_op` must be a matrix.",
               fixed = TRUE
  )
  expect_error(get_detection_history(cam_op = "not a matrix",
                                     rec_table = rec_table,
                                     species = species,
                                     output = output,
                                     occasion_length = occasion_length),
               "`cam_op` must be a matrix.",
               fixed = TRUE
  )
  expect_error(get_detection_history(cam_op = dplyr::as_tibble(cam_op),
                                     rec_table = rec_table,
                                     species = species,
                                     output = output,
                                     occasion_length = occasion_length),
               "`cam_op` must be a matrix.",
               fixed = TRUE
  )
}
)

test_that("Record table input, `rec_table`, has right format", {
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- "binary"
  occasion_length <- 1
  expect_error(get_detection_history(cam_op = cam_op,
                                     rec_table = NULL,
                                     species = species,
                                     output = output,
                                     occasion_length = occasion_length),
               "`rec_table` must be a tibble data.frame.",,
               fixed = TRUE
  )
  expect_error(get_detection_history(cam_op = cam_op,
                                     rec_table = "not a tibble",
                                     species = species,
                                     output = output,
                                     occasion_length = occasion_length),
               "`rec_table` must be a tibble data.frame.",,
               fixed = TRUE
  )
  expect_error(get_detection_history(cam_op = cam_op,
                                     rec_table = as.matrix(rec_table),
                                     species = species,
                                     output = output,
                                     occasion_length = occasion_length),
               "`rec_table` must be a tibble data.frame.",
               fixed = TRUE
  )
  
  rec_table_no_n <- rec_table
  rec_table_no_n$n <- NULL
  expect_error(
    get_detection_history(cam_op = cam_op,
                          rec_table = rec_table_no_n,
                          species = species,
                          output = output,
                          occasion_length = occasion_length),
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
  occasion_length <- 1
  expect_error(get_detection_history(cam_op = cam_op,
                                     rec_table = rec_table,
                                     species = NULL,
                                     output = output,
                                     occasion_length = occasion_length),
               "`species` must be a character vector of lenght 1.",
               fixed = TRUE
  )
  expect_error(get_detection_history(cam_op = cam_op,
                                     rec_table = rec_table,
                                     species = 1,
                                     output = output,
                                     occasion_length = occasion_length),
               "`species` must be a character vector of lenght 1.",
               fixed = TRUE
  )
  expect_error(
    get_detection_history(cam_op = cam_op,
                          rec_table = rec_table,
                          species = c("Anas platyrhynchos", "Anas strepera"),
                          output = output,
                          occasion_length = occasion_length),
    "`species` must be a character vector of lenght 1.",
    fixed = TRUE
  )
  expect_error(
    get_detection_history(cam_op = cam_op,
                          rec_table = rec_table,
                          species = "not a species",
                          output = output,
                          occasion_length = occasion_length),
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
  occasion_length <- 1
  expect_error(get_detection_history(cam_op = cam_op,
                                     rec_table = rec_table,
                                     species = "Anas platyrhynchos",
                                     output = output,
                                     occasion_length = occasion_length),
               "`output` must be a character vector of lenght 1.",
               fixed = TRUE
  )
  output <- c(5,2)
  expect_error(get_detection_history(cam_op = cam_op,
                                     rec_table = rec_table,
                                     species = "Anas platyrhynchos",
                                     output = output,
                                     occasion_length = occasion_length),
               "`output` must be a character vector of lenght 1.",
               fixed = TRUE
  )
  cam_op <- get_cam_op(mica)
  rec_table <- get_record_table(mica)
  species <- "Anas platyrhynchos"
  output <- "wrong"
  occasion_length <- 1
  expect_error(
    get_detection_history(cam_op = cam_op,
                          rec_table = rec_table,
                          species = "Anas platyrhynchos",
                          output = output,
                          occasion_length = occasion_length),
    paste0("Invalid value for output parameter: wrong.\n",
           "Valid inputs are: binary, n_observations and n_individuals"),
    fixed = TRUE
  )
})