testthat::test_that(
  "calc_animal_pos returns errors if animal_pos is not valid", {
  # animal_pos is not a dataframe
  testthat::expect_error(calc_animal_pos(1, list(a = "a")))
  # x and y columns missing
  testthat::expect_error(
    calc_animal_pos(dplyr::tibble(deployment = "A",
                              ImageWidth = 5,
                              ImageHeight = 10),
                list(a = "a")),
    "Columns x and y not found in animal_pos."
  )
  # imageWidth, imageHeight and deployment columns missing
  testthat::expect_error(
    calc_animal_pos(dplyr::tibble(width = 5,
                              height = 10,
                              x = 4, y = 2),
                list(a = "a")),
    "Columns ImageWidth, ImageHeight and deployment not found in animal_pos."
  )
})

testthat::test_that(
  "calc_animal_post returns errors if calib_models is not valid", {
  df <- dplyr::tibble(ImageWidth = 5,
                      ImageHeight = 10,
                      x = 4,
                      y = 2,
                      deployment = 2)
  # calib_models is not a (named) list
  testthat::expect_error(calc_animal_pos(df, calib_models = 2))
  testthat::expect_error(calc_animal_pos(df, calib_models = list(2)),
                         "calib_models must be a named list.")
})

testthat::test_that("Deployments with no matching calibration model", {
  missing_calib_model <- dep_calib_models
  missing_calib_model$S01 <- NULL
  testthat::expect_warning(
    calc_animal_pos(animal_positions, missing_calib_model),
    paste("Some deployments have no matching calibration model",
          "and are stripped out: S01"
    )
  )
})

testthat::test_that("Deploys with multiple values for image width/height", {
  multi_pixel_dim <- animal_positions
  multi_pixel_dim$ImageWidth[1] <- 4096
  multi_pixel_dim$ImageHeight[20] <- 3072
  testthat::expect_warning(
    calc_animal_pos(multi_pixel_dim, dep_calib_models),
    paste("There is more than one unique value per deployment for ImageWidth",
          "and/or ImageHeight in deployment(s): S01,S02"),
    fixed = TRUE
  )
})

testthat::test_that("Right output", {
  output <- calc_animal_pos(animal_positions, dep_calib_models)
  # right class
  testthat::expect_true(inherits(output, c("tbl_df", "tbl", "data.frame")))
  # right number of rows
  testthat::expect_true(nrow(output) == nrow(animal_positions))
  # right number of columns
  testthat::expect_true(ncol(output) == ncol(animal_positions) + 3)
  # new columns have right names
  testthat::expect_true(
    all(c("radius", "angle", "frame_count") %in% names(output))
  )
  # output is exactly the same as animal_positions except for the new columns
  testthat::expect_equal(
    output %>%
      dplyr::select(-c(.data$radius, .data$angle, .data$frame_count)),
    animal_positions
  )
})
