#' Check validity camera trap data package
#'
#' Checks the validity of a camera trap data package.
#' It checks whether the data package is a list containing an element called
#' `data` with the following resources as tibble data frames:
#' - `observations`
#' - `media`
#' - `deployments`
#'
#' @param package Camera trap data package
#' @param media Has the `media` resource been loaded while reading the data
#'   package? Default: `FALSE`.
#' @return `TRUE` or error.
#' @noRd
check_package <- function(package = NULL,
                          media = FALSE) {
  # check media arg
  assertthat::assert_that(
    media %in% c(TRUE, FALSE),
    msg = "`media` must be a logical: TRUE or FALSE"
  )
  # camera trap data package is a list
  assertthat::assert_that(is.list(package),
                          msg = "package is not a list.")
  assertthat::assert_that(!is.data.frame(package),
                          msg = "package is not a list.")
  # check existence of an element called data
  assertthat::assert_that("data" %in% names(package),
                          msg = "data element is missing from package")
  # check validity data element of package: does it contain deployments and
  # observations?
  elements <- c("deployments", "observations")
  if (media) {
    elements <- c(elements, "media")
  }
  tables_absent <- elements[
    !elements %in% names(package$data)
  ]
  assertthat::assert_that(
    length(tables_absent) == 0,
    msg = glue::glue(
      "Can't find {length(tables_absent)} elements in data package: {tables_absent*}",
      .transformer = collapse_transformer(sep = ", ", last = " and ")
    )
  )
  if (media) {
    assertthat::assert_that(
      !is.null(package$data$media),
      msg = glue::glue("Can't find media in .$data.")
    )
  }
  # check observations and deployments are data.frames
  assertthat::assert_that(is.data.frame(package$data$observations))
  assertthat::assert_that(is.data.frame(package$data$deployments))
  # check media is a data.frame (if imported, i.e. if not NULL)
  if (!is.null(package$data$media)) {
    assertthat::assert_that(is.data.frame(package$data$media))
  }
  # If no errors are encountered, TRUE is returned
  return(TRUE)
}
