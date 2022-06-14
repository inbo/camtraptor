testthat::test_that("predict_pos returns errors if dat is not valid", {
  # dat is not a dataframe
  testthat::expect_error(predict_pos(1, list(a= "a")))
  # x and y columns missing
  testthat::expect_error(predict_pos(dplyr::tibble(ImageWidth = 5,
                                                   ImageHeight = 10),
                                     list(a= "a")))
  # imageWidth, imageHeight and deployment columns missing
  testthat::expect_error(
    predict_pos(dplyr::tibble(width = 5,height = 10, x = 4, y = 2),
                list(a= "a")),
    "Columns ImageWidth, ImageHeight and deployment not found in dat.")
})

testthat::test_that("predict_post returns errors if mods is not valid", {
  df <- dplyr::tibble(ImageWidth = 5,
                      ImageHeight = 10,
                      x = 4,
                      y = 2,
                      deployment = 2)
  # mods is not a (named) list
  testthat::expect_error(predict_pos(df, mods = 2))
  testthat::expect_error(predict_pos(df, mods = list(2)),
                         "mods must be a named list.")
})

testthat::test_that("Deployments with no matching calibration model", {
  # to do
  # testthat::expect_warning()
})

testthat::test_that("Deploys with multiple values for image width/height", {
  #to do
  # testthat::expect_warning()
})
