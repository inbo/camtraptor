test_that("Camera operation matrix input, `cam_op`, is a matrix", {
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

test_that("Record table input, `rec_table`, is a tibble", {
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
}
)
