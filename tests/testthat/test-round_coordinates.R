test_that("round_coordinates() returns error on invalid digits", {
  error_msg <- "`digits` must be 1, 2 or 3."
  expect_error(round_coordinates(mica, digits = 0), error_msg)
  expect_error(round_coordinates(mica, digits = 4), error_msg)
  expect_error(round_coordinates(mica, digits = 1.5), error_msg)
})

test_that("round_coordinates() sets lat, long, uncertainty and precision", {
  # Set coordinates and uncertainty of deployments along latitude gradients
  deployments(mica)$longitude[[1]] <- 5.65555
  deployments(mica)$latitude[[1]] <- 15.18155 # 0 latitude
  deployments(mica)$coordinateUncertainty[[1]] <- 10
  deployments(mica)$longitude[[2]] <- 5.65
  deployments(mica)$latitude[[2]] <- 51.18 # 30 latitude
  deployments(mica)$coordinateUncertainty[[2]] <- NA_integer_
  deployments(mica)$longitude[[3]] <- 5.65
  deployments(mica)$latitude[[3]] <- -61.18 # 60 latitude
  deployments(mica)$coordinateUncertainty[[3]] <- NA_integer_
  deployments(mica)$longitude[[4]] <- 5.65
  deployments(mica)$latitude[[4]] <- -85.18 # 85 latitude
  deployments(mica)$coordinateUncertainty[[4]] <- NA_integer_

  mica1 <- round_coordinates(mica, 1)
  deploys1 <- deployments(mica1)
  expect_equal(mica1$coordinatePrecision, 0.1)
  expect_equal(deploys1$longitude[[1]], 5.7)
  expect_equal(deploys1$latitude[[1]], 15.2)
  expect_equal(deploys1$coordinateUncertainty[[1]], 10 + 15691)
  expect_equal(deploys1$longitude[[2]], 5.7)
  expect_equal(deploys1$latitude[[2]], 51.2)
  expect_equal(deploys1$coordinateUncertainty[[2]], 30 + 15691)
  expect_equal(deploys1$longitude[[3]], 5.7)
  expect_equal(deploys1$latitude[[3]], -61.2)
  expect_equal(deploys1$coordinateUncertainty[[3]], 30 + 15691)
  expect_equal(deploys1$longitude[[4]], 5.7)
  expect_equal(deploys1$latitude[[4]], -85.2)
  expect_equal(deploys1$coordinateUncertainty[[4]], 30 + 15691)

  mica2 <- round_coordinates(mica, 2)
  deploys2 <- deployments(mica2)
  expect_equal(mica2$coordinatePrecision, 0.01)
  expect_equal(deploys2$longitude[[1]], 5.66)
  expect_equal(deploys2$latitude[[1]], 15.18)
  expect_equal(deploys2$coordinateUncertainty[[1]], 10 + 1570)
  expect_equal(deploys2$longitude[[2]], 5.65)
  expect_equal(deploys2$latitude[[2]], 51.18)
  expect_equal(deploys2$coordinateUncertainty[[2]], 30 + 1570)
  expect_equal(deploys2$longitude[[3]], 5.65)
  expect_equal(deploys2$latitude[[3]], -61.18)
  expect_equal(deploys2$coordinateUncertainty[[3]], 30 + 1570)
  expect_equal(deploys2$longitude[[4]], 5.65)
  expect_equal(deploys2$latitude[[4]], -85.18)
  expect_equal(deploys2$coordinateUncertainty[[4]], 30 + 1570)

  mica3 <- round_coordinates(mica, 3)
  deploys3 <- deployments(mica3)
  expect_equal(mica3$coordinatePrecision, 0.001)
  expect_equal(deploys3$longitude[[1]], 5.656)
  expect_equal(deploys3$coordinateUncertainty[[1]], 10 + 157)
  expect_equal(deploys3$longitude[[2]], 5.65) # Unchanged
  expect_equal(deploys3$latitude[[2]], 51.18) # Unchanged
  expect_equal(deploys3$coordinateUncertainty[[2]], 30 + 157)
  expect_equal(deploys3$longitude[[3]], 5.65) # Unchanged
  expect_equal(deploys3$latitude[[3]], -61.18) # Unchanged
  expect_equal(deploys3$coordinateUncertainty[[3]], 30 + 157)
  expect_equal(deploys3$longitude[[4]], 5.65) # Unchanged
  expect_equal(deploys3$latitude[[4]], -85.18) # Unchanged
  expect_equal(deploys3$coordinateUncertainty[[4]], 30 + 157)
})

test_that("round_coordinates() does not allow to round to higher precision", {
  mica2 <- round_coordinates(mica, 2)

  # Based on package$coordinatePrecision
  expect_error(
    round_coordinates(mica2, 3),
    paste(
      "Can't round from 2 to 3 digits.",
      "`2` is derived from the `package$coordinatePrecision=0.01`."
    ),
    fixed = TRUE
  )

  # Based on data
  mica2$coordinatePrecision <- NULL
  expect_error(
    round_coordinates(mica2, 3),
    paste(
      "Can't round from 2 to 3 digits.",
      "`2` is the maximum number of decimals for latitude in the data."
    ),
    fixed = TRUE
  )
})

test_that("round_coordinates() doesn't overestimate uncertainty on multiple runs", {
  # Set uncertainty of first 2 deployments
  deployments(mica)$coordinateUncertainty[[1]] <- 10
  deployments(mica)$coordinateUncertainty[[2]] <- NA_integer_

  mica1 <- round_coordinates(mica, 1)
  mica1_via_2 <- round_coordinates(round_coordinates(mica, 2), 1)
  expect_equal(
    mica1_via_2$coordinatePrecision,
    mica1$coordinatePrecision
  )
  expect_equal(
    deployments(mica1_via_2)$coordinateUncertainty[[1]],
    deployments(mica1)$coordinateUncertainty[[1]]
  )
  expect_equal(
    deployments(mica1_via_2)$coordinateUncertainty[[2]],
    deployments(mica1)$coordinateUncertainty[[2]]
  )
})
