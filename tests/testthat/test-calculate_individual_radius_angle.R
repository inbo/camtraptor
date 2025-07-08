test_that(
  paste0("calculate_individual_radius_angle returns errors if ",
         "animal_positions is not valid"
  ),
  {
    # animal_positions is not a dataframe
    expect_error(calculate_individual_radius_angle(1, list(a = "a")))
    # x, y and eventID columns missing
    expect_error(
      calculate_individual_radius_angle(
        dplyr::tibble(
          deploymentID = "A",
          imageWidth = 5,
          imageHeight = 10
        ),
        list(a = "a")
      ),
      "Columns `eventID`, `x` and `y` not found in `animal_positions`."
    )
    # imageWidth, imageHeight and deploymentID columns missing
    expect_error(
      calculate_individual_radius_angle(
        dplyr::tibble(
          width = 5,
          height = 10,
          eventID = 2,
          x = 4, y = 2
        ),
        list(a = "a")
      ),
      paste0("Columns `deploymentID`, `imageWidth` and `imageHeight` ",
             "not found in `animal_positions`."
      )
    )
  }
)

test_that(
  paste0("calculate_individual_radius_angle returns errors if ",
         "calibration_models is not valid"
  ),
  {
    df <- dplyr::tibble(
      imageWidth = 5,
      imageHeight = 10,
      deploymentID = "Z",
      eventID = 3,
      x = 4,
      y = 2,
      deployment = 2
    )
    # calibration_models is not a (named) list
    expect_error(calculate_individual_radius_angle(df, calibration_models = 2))
    expect_error(
      calculate_individual_radius_angle(df, calibration_models = list(2)),
      "`calibration_models` must be a named list."
    )
  }
)

test_that("Deployments with no matching calibration model", {
  missing_calib_model <- calibration_models
  missing_calib_model$S01 <- NULL
  expect_warning(
    calculate_individual_radius_angle(animal_positions, missing_calib_model),
    paste(
      "Some deployments have no matching calibration model",
      "and are stripped out: S01"
    )
  )
})

test_that("Deploys with multiple values for image width/height", {
  multi_pixel_dim <- animal_positions
  multi_pixel_dim$imageWidth[1] <- 4096
  multi_pixel_dim$imageHeight[20] <- 3072
  expect_warning(
    calculate_individual_radius_angle(multi_pixel_dim, calibration_models),
    paste(
      "There is more than one unique value per deployment for `imageWidth`",
      "and/or `imageHeight` in deployment(s): S01, S02"
    ),
    fixed = TRUE
  )
})

test_that("Right output", {
  output <- calculate_individual_radius_angle(animal_positions,
                                              calibration_models
  )
  # right class
  expect_true(inherits(output, c("tbl_df", "tbl", "data.frame")))
  # right number of rows
  expect_true(nrow(output) == nrow(animal_positions))
  # right number of columns
  expect_true(ncol(output) == ncol(animal_positions) + 3)
  # new columns have right names
  expect_true(
    all(c("radius", "angle", "frame_count") %in% names(output))
  )
  # output is exactly the same as animal_positions except for the new columns
  expect_equal(
    output %>%
      dplyr::select(-c(radius, angle, frame_count)),
    animal_positions
  )
})

test_that("Right output with non default column names", {
  output_default <- calculate_individual_radius_angle(animal_positions, calibration_models)
  animal_positions_non_default <- dplyr::rename(
    animal_positions,
    deployment_id = deploymentID,
    event_id = eventID,
    X = x,
    Y = y,
    image_width = imageWidth,
    image_height = imageHeight
  )
  output <- calculate_individual_radius_angle(animal_positions_non_default,
    calibration_models, 
    deployment_id = "deployment_id",
    event_id = "event_id",
    x = "X",
    y = "Y",
    image_width = "image_width",
    image_height = "image_height"
  )
  # content is the same (column names are different)
  names(output) <- as.character(1:length(names(output)))
  names(output_default) <- as.character(1:length(names(output_default)))
  expect_equal(output, output_default)
})
