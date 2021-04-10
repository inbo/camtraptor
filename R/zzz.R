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
#' @keywords internal
#'
check_datapkg <- function(datapkg) {
  # check validity data package: does it contain all 4 elements?
  tables_absent <- names(camtrapdp)[!names(camtrapdp) %in% names(datapkg)]
  n_tables_absent <- length(tables_absent)
  assert_that(n_tables_absent == 0,
              msg = glue("There are {n_tables_absent} elements not found in",
                         " data package: {tables_absent*}",
                         .transformer = collapse_transformer(
                           sep = ", ",
                           last = " and ")
                         )
              )


  # check observations deployments and multimedia are data.frames
  assert_that(is.data.frame(datapkg$observations))
  assert_that(is.data.frame(datapkg$deployments))
  assert_that(is.data.frame(datapkg$multimedia))

  # check element datapackage (metadata) is a list
  assert_that(is.list(datapkg$datapackage))
}

#' Check input value against list of provided values
#'
#' Will return error message if an input value cannot be found in list of
#' provided values. NULL values can be allowed (default) or not by setting
#' argument `null_allowed` equal to `TRUE` or `FALSE`.
#'
#' @param arg Character. The input argument provided by the user.
#' @param options Character vector of valid inputs for the argument.
#' @param arg_name Character. The name of the argument used in the function to
#'   test.
#'
#' @return If no error, `TRUE`.
#'
#' @keywords internal
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
  msg_to_print <- glue(
    "Invalid value for {arg_name} argument.
        Valid inputs are: {options_to_print*}.",
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
#' @param max_color_scale a number indicating the maximum value of the absolute
#'   color scale (`NULL` if relative scale is used, default)
#' @param prefix a prefix of legend labels
#' @param suffix a suffix of legend labels
#' @param digits the number of digits of numeric values in labels
#' @param big.mark the thousand separator
#' @param transform a function to transform the label value
#'
#' @noRd
#'
#' @keywords internal
labelFormat_scale <- function(max_color_scale = NULL,
                              prefix = "",
                              suffix = "",
                              digits = 3,
                              big.mark = ",",
                              transform = identity) {
  formatNum <- function(x, max_color_scale) {
    cuts_chrs <- format(round(transform(x), digits),
      trim = TRUE,
      scientific = FALSE,
      big.mark = big.mark
    )
    if (!is.null(max_color_scale)) {
      n <- length(x)
      if (x[n] == max_color_scale) {
        cuts_chrs[n] <- paste0(cuts_chrs[n], "+")
      }
    }
    return(cuts_chrs)
  }

  function(type, ...) {
    switch(
      type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum(cuts, max_color_scale), suffix)
      })(...)
    )
  }
}

#' Get deployments with no observations
#'
#' Return subset of deployments without observations. A message is also returned
#' to list the ID of such deployments.
#'
#' @param datapkg a camera trap data package object, as returned by `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#' 1. `observations`
#' 2. `deployments`
#' 3. `multimedia`
#'
#' and a list with metadata: `datapackage`
#'
#' @importFrom dplyr .data %>% anti_join distinct
#' @importFrom  glue glue
#'
#' @export
#'
#' @return a tibble (data.frame) with deployments not linked to any observations
get_dep_no_obs <- function(datapkg) {

  # check input data package
  check_datapkg(datapkg)

  # extract observations and deployments
  observations <- datapkg$observations
  deployments <- datapkg$deployments

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

#' Check validity of the given scientific or vernacular name(s)
#'
#' Check names against all scientific and vernacular names contained in a camera
#' trap data package
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations` 2. `deployments` 3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#'
#' @param species a character vector with scientific or vernacular names.
#'
#' @importFrom purrr map_chr
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @return a character vector with the correspondent scientific names
#'
#' @examples
#' check_species(camtrapdp, c("Mallard", "Norway Rat"))
#' # case insensitive
#' check_species(camtrapdp, "MalLARD")
#'
check_species <- function(datapkg, species) {

  assert_that(!is.null(species) & length(species) > 0,
              msg = "species argument must be specified")

  all_species <- get_species(datapkg)
  check_value(tolower(species),
              c(tolower(all_species$scientific_name),
                tolower(all_species$vernacular_name)),
              "species")

  map_chr(species, function(x) {
    # get scientific name in case a vernacular names is given
    if (tolower(x) %in% tolower(all_species$vernacular_name)) {
      sn <- get_scientific_name(datapkg, x)
      message(glue("Scientific name of {x}: {sn}"))
      sn
    } else {
      x
    }
  })
}
