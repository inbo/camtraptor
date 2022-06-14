testthat::test_that("predict_pos returns errors if dat is not valid", {
  # dat is not a dataframe
  testthat::expect_error(predict_pos(1, list(a= "a")))
  # x and y columns missing
  testthat::expect_error(predict_pos(dplyr::tibble(ImageWidth = 5,
                                                   ImageHeight = 10),
                                     list(a= "a")))
})
