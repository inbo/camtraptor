#' Check validity camera trap data package
#'
#' Checks the validity of a camera trap data package.
#' It checks whether the data package is a list containing an element called
#' `data` with the following resources as tibble data frames:
#' - `observations`
#' - `multimedia`
#' - `deployments`
#'
#' @param package Camera trap data package
#' @param datapkg Deprecated. Use `package` instead.
#' @return A camera trap data package.
#' @noRd
check_package <- function(package = NULL,
                          datapkg = NULL,
                          function_name) {
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
  # camera trap data package is a list
  assertthat::assert_that(is.list(package))
  assertthat::assert_that(!is.data.frame(package))
  # check existence of an element called data
  assertthat::assert_that("data" %in% names(package))
  # check validity data element of package: does it contain all 4 elements?
  elements <- c("deployments", "media", "observations")
  tables_absent <- names(elements)[
    !names(elements) %in% names(package$data)
  ]
  n_tables_absent <- length(tables_absent)
  assertthat::assert_that(n_tables_absent == 0,
              msg = glue::glue(
                "There are {n_tables_absent} elements not found in",
                " data package: {tables_absent*}",
                .transformer = collapse_transformer(
                  sep = ", ",
                  last = " and "
                  )
              )
  )

  # check observations and deployments are data.frames
  assertthat::assert_that(is.data.frame(package$data$observations))
  assertthat::assert_that(is.data.frame(package$data$deployments))
  # check multimedia is a data.frame (if imported, i.e. if not NULL)
  if (!is.null(package$data$multimedia)) {
    assertthat::assert_that(is.data.frame(package$data$multimedia))
  }
  package
}

#' Check input value against list of provided values
#'
#' Will return error message if an input value cannot be found in list of
#' provided values. NULL values can be allowed (default) or not by setting
#' argument `null_allowed` equal to `TRUE` or `FALSE`.
#'
#' @param arg character containing the input argument provided by the user
#' @param options character vector of valid inputs for the argument
#' @param arg_name character with the name of the argument used in the function
#'   to test
#' @param null_allowed logical (`TRUE`, the default, or `FALSE`) Are NULL values
#'   allowed?
#' @return If no error, `TRUE`.
#' @noRd
#' @examples
#' \dontrun{
#' # Valid inputs for species
#' check_value("Canis lupus", c("Canis lupus", "Corvus monedula"), "species")
#'
#' # Invalid inputs for species
#' values <- c("Ans streperi", # wrong
#'   "Anas strepera",
#'   "wld end", # wrong
#'   "wilde eend"
#'  )
#' check_value(values, c("Anas strepera", "wilde eend"), "species")
#' }
check_value <- function(arg, options = NULL, arg_name, null_allowed = TRUE) {
  max_print <- 20

  # Drop NA
  options <- options[!is.na(options)]

  # Wrong values
  wrong_values <- arg[!(arg %in% options)]

  # Suppress long messages with valid options
  if (length(options) > max_print) {
    options_to_print <- c(options[1:max_print], "others..")
  } else {
    options_to_print <- options
  }

  # compose error message
  if (null_allowed == TRUE) {
    string_to_print <- "Invalid value for {arg_name} argument: {wrong_values}.
        Valid inputs are: NULL, {options_to_print*}."
  } else {
    string_to_print <- "Invalid value for {arg_name} argument: {wrong_values}.
        Valid inputs are: {options_to_print*}."
  }

  msg_to_print <- glue::glue(
    string_to_print,
    .transformer = collapse_transformer(
      sep = ", ",
      last = " and "
    )
  )

  # Provide user message
  if (!is.null(arg)) {
    assertthat::assert_that(
      all(arg %in% options),
      msg = msg_to_print
    )
  } else {
      assertthat::assert_that(null_allowed == TRUE,
                  msg = msg_to_print
      )
    }
}

#' Print list of options
#'
#' @param regex Character. A regular expression to parse.
#' @param ... Additional arguments passed to the collapse.
#' @noRd
collapse_transformer <- function(regex = "[*]$", ...) {
  function(code, envir) {
    if (grepl(regex, code)) {
      code <- sub(regex, "", code)
    }
    res <- eval(parse(text = code), envir)
    glue::glue_collapse(res, ...)
  }
}

#' Custom label format function
#'
#' Add "+" to last label of legend while using absolute scale. At the moment
#' only numeric scale is needed and therefore implemented.
#'
#' @source based on leaflet's
#'   [labelFormat()](https://github.com/rstudio/leaflet/commit/bb3ab964486b357ddc160a7032cfdce6cd8fbe35)
#'    function
#'
#' @param max_scale a number indicating the maximum value of the absolute scale
#'   (`NULL` if relative scale is used, default)
#' @param prefix a prefix of legend labels
#' @param suffix a suffix of legend labels
#' @param digits the number of digits of numeric values in labels
#' @param big.mark the thousand separator
#' @param transform a function to transform the label value
#' @noRd
labelFormat_scale <- function(max_scale = NULL,
                              prefix = "",
                              suffix = "",
                              digits = 3,
                              big.mark = ",",
                              transform = identity) {
  formatNum <- function(x, max_scale) {
    cuts_chrs <- format(round(transform(x), digits),
      trim = TRUE,
      scientific = FALSE,
      big.mark = big.mark
    )
    if (!is.null(max_scale)) {
      n <- length(x)
      if (x[n] == max_scale) {
        cuts_chrs[n] <- paste0(cuts_chrs[n], "+")
      }
    }
    return(cuts_chrs)
  }

  function(type, ...) {
    switch(
      type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum(cuts, max_scale), suffix)
      })(...)
    )
  }
}

#' Get deployments without observations
#'
#' Return subset of deployments without observations. A message is also returned
#' to list the ID of such deployments.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param datapkg Deprecated. Use `package` instead.
#' @param ... Filter predicates for filtering on deployments
#' @return A tibble data frame with deployments not linked to any observations.
#' @family exploration functions
#' @importFrom dplyr .data %>%
#' @noRd
#' @examples
#' get_dep_no_obs(mica)
get_dep_no_obs <- function(package = NULL,
                           ...,
                           datapkg = lifecycle::deprecated()) {

  # check input camera trap data package
  package <- check_package(package, datapkg, "get_dep_no_obs")

  # extract observations and deployments
  observations <- package$data$observations
  deployments <- package$data$deployments

  # apply filtering (do not show filtering expression, verbose = FALSE)
  deployments <- apply_filter_predicate(df = deployments, verbose = FALSE, ...)

  # deployment with no observations
  dep_no_obs <-
    deployments %>%
    dplyr::anti_join(observations %>%
                dplyr::distinct(.data$deploymentID),
              by = "deploymentID")

  dep_no_obs_ids <- dep_no_obs$deploymentID
  n_dep_no_obs <- length(dep_no_obs_ids)

  if (n_dep_no_obs > 0) {
    max_print <- 20
    # Suppress long messages
    if (length(dep_no_obs_ids) > max_print) {
      options_to_print <- c(dep_no_obs_ids[1:max_print], "others..")
    } else {
      options_to_print <- dep_no_obs_ids
    }
    message(glue::glue("There are {n_dep_no_obs} deployments",
                 " with no observations: {options_to_print*}",
                 .transformer = collapse_transformer(
                   sep = ", ",
                   last = " and "
                 )
    ))
  }
  return(dep_no_obs)
}

#' Calculate daily effort for start or end day
#'
#' While assessing the camera operation matrix, start and end day are edge
#' case. The daily effort is a real number between 0 and 1 as and is defined as
#' the fraction of the day the camera was on
#'
#' @importFrom dplyr %>%
#' @noRd
calc_daily_effort <- function(deploy_df, calc_start=NULL, calc_end=NULL) {
  # check calc_start or calc_end are passed
  assertthat::assert_that(
    (is.null(calc_start) & !is.null(calc_end)) |
      (!is.null(calc_start) & is.null(calc_end)),
    msg = "Either calc_start or calc_end must be defined.")
  deploy_df <- deploy_df %>%
    dplyr::mutate(edge = dplyr::if_else(!is.null(calc_start), .data$start, .data$end),
           edge_day = dplyr::if_else(!is.null(calc_start), .data$start_day, .data$end_day))
  deploy_df %>%
  # calculate the duration of the start/end day (edge day)
  dplyr::mutate(edge_day_duration =
           lubridate::as.duration(lubridate::as_datetime(.data$edge_day) +
                         lubridate::ddays(1) -
                         lubridate::as_datetime(.data$edge_day))) %>%
    # calculate the duration of the active part of the start/end day
  dplyr::mutate(active_edge_day_duration = dplyr::if_else(
    !is.null(calc_start),
    # start day
    .data$edge_day_duration - lubridate::as.duration(.data$edge - lubridate::as_datetime(.data$edge_day)),
    # end day
    .data$edge_day_duration - lubridate::as.duration(lubridate::as_datetime(.data$edge_day) + lubridate::ddays(1) - .data$edge))) %>%
    # calculate the fraction of the duration of the active part
  dplyr::mutate(daily_effort = .data$active_edge_day_duration / .data$edge_day_duration) %>%
  dplyr::pull(.data$daily_effort)
}

#' Predict radial distance
#'
#' Predict radial distance from camera given pixel positions.
#'
#' @param mod Site calibration model (`depcal` object), produced using
#'   `cal.site()`).
#' @param relx x Pixel position relative to the centre line.
#' @param rely y Pixel position relative to the top edge.
#' @return Vector numeric radii.
#' @noRd
#' @note Units depend on the units of pole height above ground used to calibrate
#'   the site model.
predict_r <- function(mod, rel_x, rel_y) {
  new_data <- data.frame(relx = rel_x, rely = rel_y)
  res <- stats::predict(mod, newdata = new_data)
  res[res<0] <- Inf
  return(res)
}
