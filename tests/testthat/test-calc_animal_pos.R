test_that(
  "calc_animal_pos returns errors if animal_pos is not valid",
  {
    # animal_pos is not a dataframe
    expect_error(calc_animal_pos(1, list(a = "a")))
    # x, y and sequenceID columns missing
    expect_error(
      calc_animal_pos(
        dplyr::tibble(
          deploymentID = "A",
          imageWidth = 5,
          imageHeight = 10
        ),
        list(a = "a")
      ),
      "Columns `sequenceID`, `x` and `y` not found in `animal_pos`."
    )
    # imageWidth, imageHeight and deploymentID columns missing
    expect_error(
      calc_animal_pos(
        dplyr::tibble(
          width = 5,
          height = 10,
          sequenceID = 2,
          x = 4, y = 2
        ),
        list(a = "a")
      ),
      "Columns `deploymentID`, `imageWidth` and `imageHeight` not found in `animal_pos`."
    )
  }
)

test_that(
  "calc_animal_post returns errors if calib_models is not valid",
  {
    df <- dplyr::tibble(
      imageWidth = 5,
      imageHeight = 10,
      deploymentID = "Z",
      sequenceID = 3,
      x = 4,
      y = 2,
      deployment = 2
    )
    # calib_models is not a (named) list
    expect_error(calc_animal_pos(df, calib_models = 2))
    expect_error(
      calc_animal_pos(df, calib_models = list(2)),
      "`calib_models` must be a named list."
    )
  }
)

test_that("Deployments with no matching calibration model", {
  missing_calib_model <- dep_calib_models
  missing_calib_model$S01 <- NULL
  expect_warning(
    calc_animal_pos(animal_positions, missing_calib_model),
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
    calc_animal_pos(multi_pixel_dim, dep_calib_models),
    paste(
      "There is more than one unique value per deployment for `imageWidth`",
      "and/or `imageHeight` in deployment(s): S01, S02"
    ),
    fixed = TRUE
  )
})

test_that("Right output", {
  output <- calc_animal_pos(animal_positions, dep_calib_models)
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
  output_default <- calc_animal_pos(animal_positions, dep_calib_models)
  animal_positions_non_default <- dplyr::rename(
    animal_positions,
    deployment_id = deploymentID,
    sequence_id = sequenceID,
    X = x,
    Y = y,
    image_width = imageWidth,
    image_height = imageHeight
  )
  output <- calc_animal_pos(animal_positions_non_default,
    dep_calib_models,
    dep_tag = "deployment_id",
    sequence_id = "sequence_id",
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
