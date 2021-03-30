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

#' Get prefix table
#'
#' Store prefixes for info shown while hovering over a deployment with the
#' mouse. List fields in deployments as in
#' https://tdwg.github.io/camtrap-dp/data/#deployments
#'
#' Returns a data.frame of all prefixes with the following columns: - `info`:
#' deployment info - `prefix`: prefix to use
#'
#' @importFrom dplyr as_tibble
#'
#' @noRd
#'
#' @usage map_dep_prefixes()
#'
#' @keywords internal
map_dep_prefixes <- function() as_tibble(mapdep_prefixes)

mapdep_prefixes <- structure(list(
  info = c("deployment_id", "location_id", "location_name", "longitude",
           "latitude", "start", "end", "setup_by", "camera_id", "camera_model",
           "camera_interval", "camera_height", "bait_use", "session", "array",
           "feature_type", "habitat", "tags", "comments", "n_species", "n_obs"),
  prefix = c("deployment ID: ",
             "location ID: ",
             "location name: ",
             "longitude: ",
             "latitude: ",
             "start: ",
             "end: ",
             "setup by: ",
             "camera ID: ",
             "camera model: ",
             "camera interval: ",
             "camera height: ",
             "bait use: ",
             "session: ",
             "array: ",
             "feature type: ",
             "habitat: ",
             "tags: ",
             "comments: ",
             "species observed: ",
             "observations: ")
))

#' Retrieve prefixes (fields) for text to show while hovering with mouse over a
#' deployment
#'
#' @param feature character, one of: - `n_species` - `n_obs`
#' @param infos character vector with deployment fields
#'
#' @importFrom dplyr .data %>% filter
#'
#' @noRd
#'
#' @keywords internal
get_prefixes <- function(feature,
                         infos) {
  infos[infos == "n"] <- feature # n can represent #species or #observations
  # get all prefixes
  prefixes <- map_dep_prefixes()
  # return the prefixes we need
  prefixes %>% filter(.data$info %in% infos)
}
