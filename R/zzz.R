#' Check validity data package
#'
#' This function checks the validity of a camera trap data package. Up to now it
#' checks whether the data package contains the 4 elements: `datapackage`,
#' `observations`,  `multimedia` and `deployments` and their type (list for the
#' metadata `datapakage`, data.frame for the other three). More checks can be
#' added in the future...
#'
#' @param datapkg a camera trap data package
#'
#' @noRd
#'
#' @importFrom assertthat assert_that
#' 
#' @keywords internal
#'
check_datapkg <- function(datapkg) {
  # check validity data package: does it contain all 4 elements?
  tables_absent <- names(camtrapdp::camtrapdp)[
    !names(camtrapdp::camtrapdp) %in% names(datapkg)
  ]
  n_tables_absent <- length(tables_absent)
  assert_that(n_tables_absent == 0,
              msg = glue("There are {n_tables_absent} elements not found in",
                         " data package: {tables_absent*}",
                         .transformer = collapse_transformer(
                           sep = ", ",
                           last = " and ")
                         )
              )


  # check observations and deployments are data.frames
  assert_that(is.data.frame(datapkg$observations))
  assert_that(is.data.frame(datapkg$deployments))
  # check multimedia is a data.frame (if imported, i.e. if not NULL)
  if (!is.null(datapkg$multimedia)) {
    assert_that(is.data.frame(datapkg$multimedia))
  }

  # check element datapackage (metadata) is a list
  assert_that(is.list(datapkg$datapackage))
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
#'
#' @return If no error, `TRUE`.
#'
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' 
#' @keywords internal
#' 
#' @examples
#' \dontrun{
#' # Valid inputs for species
#' check_value("Canis lupus", c("Canis lupus", "Corvus monedula"), "species")
#'
#' # Invalid inputs for species
#' check_value("ddsf", c("Canis lupus", "Corvus monedula"), "species")
#' }
check_value <- function(arg, options = NULL, arg_name, null_allowed = TRUE) {
  max_print <- 20

  # Drop NA
  options <- options[!is.na(options)]

  # Suppress long messages
  if (length(options) > max_print) {
    options_to_print <- c(options[1:max_print], "others..")
  } else {
    options_to_print <- options
  }

  # compose error message
  if (null_allowed == TRUE) {
    string_to_print <- "Invalid value for {arg_name} argument.
        Valid inputs are: NULL, {options_to_print*}."
  } else {
    string_to_print <- "Invalid value for {arg_name} argument.
        Valid inputs are: {options_to_print*}."
  }

  msg_to_print <- glue(
    string_to_print,
    .transformer = collapse_transformer(
      sep = ", ",
      last = " and "
    )
  )

  # Provide user message
  if (!is.null(arg)) {
    assert_that(
      all(arg %in% options),
      msg = msg_to_print
    )
  } else {
      assert_that(null_allowed == TRUE,
                  msg = msg_to_print
      )
    }
}

#' Print list of options
#'
#' @param regex Character. A regular expression to parse.
#' @param ... Additional arguments passed to the collapse.
#'
#' @noRd
#'
#' @importFrom glue glue_collapse
#'
#' @keywords internal
collapse_transformer <- function(regex = "[*]$", ...) {
  function(code, envir) {
    if (grepl(regex, code)) {
      code <- sub(regex, "", code)
    }
    res <- eval(parse(text = code), envir)
    glue_collapse(res, ...)
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
#'
#' @noRd
#'
#' @keywords internal
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

#' Get deployments with no observations
#'
#' Return subset of deployments without observations. A message is also returned
#' to list the ID of such deployments.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#' 1. `observations`
#' 2. `deployments`
#' 3. `multimedia`
#'
#' and a list with metadata: `datapackage`
#' @param ... filter predicates for filtering on deployments
#' @importFrom dplyr .data %>% anti_join distinct
#' @importFrom  glue glue
#'
#' @export
#'
#' @return a tibble (data.frame) with deployments not linked to any observations
get_dep_no_obs <- function(datapkg, ...) {

  # check input data package
  check_datapkg(datapkg)

  # extract observations and deployments
  observations <- datapkg$observations
  deployments <- datapkg$deployments

  # apply filtering (do not show filtering expression, verbose = FALSE)
  deployments <- apply_filter_predicate(df = deployments, verbose = FALSE, ...)

  # deployment with no observations
  dep_no_obs <-
    deployments %>%
    anti_join(observations %>%
                distinct(.data$deployment_id),
              by = "deployment_id")

  dep_no_obs_ids <- dep_no_obs$deployment_id
  n_dep_no_obs <- length(dep_no_obs_ids)

  if (n_dep_no_obs > 0) {
    max_print <- 20
    # Suppress long messages
    if (length(dep_no_obs_ids) > max_print) {
      options_to_print <- c(dep_no_obs_ids[1:max_print], "others..")
    } else {
      options_to_print <- dep_no_obs_ids
    }
    message(glue("There are {n_dep_no_obs} deployments",
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
#' @importFrom dplyr %>% if_else mutate %>% pull
#' @importFrom assertthat assert_that
#' @importFrom lubridate as.duration as_datetime ddays
#' 
#' @noRd
#' 
#' @keywords internal
calc_daily_effort <- function(deploy_df, calc_start=NULL, calc_end=NULL) {
  # check calc_start or calc_end are passed
  assert_that(
    (is.null(calc_start) & !is.null(calc_end)) | 
      (!is.null(calc_start) & is.null(calc_end)),
    msg = "Either calc_start or calc_end must be defined.")
  deploy_df <- deploy_df %>%
    mutate(edge = if_else(!is.null(calc_start), .data$start, .data$end),
           edge_day = if_else(!is.null(calc_start), .data$start_day, .data$end_day))
  deploy_df %>%
  # calculate the duration of the start/end day (edge day)
  mutate(edge_day_duration = 
           as.duration(as_datetime(.data$edge_day) + 
                         ddays(1) - 
                         as_datetime(.data$edge_day))) %>%
    # calculate the duration of the active part of the start/end day
  mutate(active_edge_day_duration = if_else(
    !is.null(calc_start),
    # start day
    .data$edge_day_duration - as.duration(.data$edge - as_datetime(.data$edge_day)),
    # end day
    .data$edge_day_duration - as.duration(as_datetime(.data$edge_day) + ddays(1) - .data$edge))) %>%
    # calculate the fraction of the duration of the active part 
  mutate(daily_effort = .data$active_edge_day_duration / .data$edge_day_duration) %>%
  pull(.data$daily_effort)
}