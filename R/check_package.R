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
#' @param datapkg Deprecated. Use `package` instead.
#' @param media Has the `media` resource been loaded while reading the data
#'   package? Default: `FALSE`.
#' @return A camera trap data package.
#' @noRd
check_package <- function(package = NULL,
                          datapkg = NULL,
                          function_name,
                          media = FALSE) {
  if (lifecycle::is_present(datapkg) & !is.null(datapkg)) {
    lifecycle::deprecate_warn(
      when = "0.16.0",
      what = paste0(function_name, "(datapkg = )"),
      with = paste0(function_name, "(package = )")
    )
    if (is.null(package)) {
      package <- datapkg
    }
  }
  # check media arg
  assertthat::assert_that(
    media %in% c(TRUE, FALSE),
    msg = "`media` must be a logical: TRUE or FALSE"
  )
  # camera trap data package is a list
  assertthat::assert_that(is.list(package))
  assertthat::assert_that(!is.data.frame(package))
  # check existence of an element called data
  assertthat::assert_that("data" %in% names(package))
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
      "Can't find {tables_absent} elements in data package: {tables_absent*}",
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
  # When all is good, nothing
}
