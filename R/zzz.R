#' Check input value against list of provided values
#'
#' Will return error message if an input value cannot be found in list of
#' provided values. NULL values can be allowed (default) or not by setting
#' parameter `null_allowed` equal to `TRUE` or `FALSE`.
#'
#' @param arg Character containing the input parameter provided by the user.
#' @param options Character vector of valid inputs for the parameter.
#' @param arg_name Character with the name of the parameter used in the function
#'   to test.
#' @param null_allowed Logical (`TRUE`, the default, or `FALSE`).
#'   Are NULL values allowed?
#' @return If no error, `TRUE`.
#' @noRd
#' @examples
#' \dontrun{
#' # Valid inputs for species
#' check_value("Canis lupus", c("Canis lupus", "Corvus monedula"), "species")
#'
#' # Invalid inputs for species
#' values <- c(
#'   "Ans streperi", # wrong
#'   "Anas strepera",
#'   "wld end", # wrong
#'   "wilde eend"
#' )
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
    options_to_print <- c(options[1:max_print], "others...")
  } else {
    options_to_print <- options
  }
  
  # Include NULL
  if (null_allowed) {
    options_to_print <- append(options_to_print, "NULL")
  } else if (is.null(wrong_values)) {
    wrong_values <- "NULL"
  }
  
  # Compose error message
  msg_to_print <- glue::glue(
    "Invalid value for {arg_name} parameter: ",
    glue::glue_collapse(wrong_values, sep = ", ", last = " and "),
    ".\nValid inputs are: ",
    glue::glue_collapse(options_to_print, sep = ", ", last = " and ")
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

#' Get version from data package profile
#'
#' This helper functions returns the version of a Camera Trap Data Package by
#' applying a regex to its `profile`.
#' 
#' The regex rule extracts the version by detecting a sequence of digits and dots.
#' If no dots are detected or `"camtrap-dp-profile.json"` is not part of
#' `profile`, the entire `profile` is returned.
#' 
#' @param profile Character containing the profile of the data package.
#' @return Character containing the data package version.
#' @noRd
get_version <- function(profile) {
  pattern_regex <- "\\d+(\\.\\d+){1,2}" # a sequence of digits and dots (max 2)
  extracted_version <- stringr::str_extract(
    string = profile, 
    pattern = pattern_regex
  )
  if (stringr::str_detect(string = profile, 
                          pattern = stringr::fixed("camtrap-dp-profile.json")) & 
      !is.na(extracted_version)) {
    extracted_version
  } else {
    profile
  }
}

#' Check reading issues
#' 
#' This helper function throws a warning if issues while reading datapackage
#' resources (data.frames) are detected. The issues are also returned as
#' data.frame.
#' 
#' @param df Data.frame.
#' @param df_name Character with name of the data.frame passed to `df`.
#' @return Data.frame containing the issues as returned by `readr::problems()`.
#' @noRd
#' 
check_reading_issues <- function(df, df_name) {
  # get problems
  issues_df <- readr::problems(df)
  if (nrow(issues_df) > 0) {
    warning(glue::glue(
      "One or more parsing issues occurred while reading `{df_name}`. ",
      "Check `?read_camtrap_dp()` for examples on how to use ",
      "`readr::problems()`."
    ))
  }
  return(issues_df)
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
    switch(type,
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
  check_package(package, datapkg, "get_dep_no_obs")

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
    by = "deploymentID"
    )

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
    message(glue::glue(
      "There are {n_dep_no_obs} deployments without observations: ",
      glue::glue_collapse(options_to_print, sep = ", ", last = " and ")
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
#' @importFrom dplyr %>% .data
#' @noRd
calc_daily_effort <- function(deploy_df, calc_start = NULL, calc_end = NULL) {
  # check calc_start or calc_end are passed
  assertthat::assert_that(
    (is.null(calc_start) & !is.null(calc_end)) |
      (!is.null(calc_start) & is.null(calc_end)),
    msg = "Either calc_start or calc_end must be defined."
  )
  deploy_df <-
    deploy_df %>%
    dplyr::mutate(
      edge = dplyr::if_else(!is.null(calc_start), .data$start, .data$end),
      edge_day = dplyr::if_else(!is.null(calc_start), .data$start_day, .data$end_day)
    )
  deploy_df %>%
    # calculate the duration of the start/end day (edge day)
    dplyr::mutate(
      edge_day_duration =
        lubridate::as.duration(lubridate::as_datetime(.data$edge_day) +
          lubridate::ddays(1) -
          lubridate::as_datetime(.data$edge_day))
    ) %>%
    # calculate the duration of the active part of the start/end day
    dplyr::mutate(active_edge_day_duration = dplyr::if_else(
      !is.null(calc_start),
      # start day
      .data$edge_day_duration - lubridate::as.duration(.data$edge - lubridate::as_datetime(.data$edge_day)),
      # end day
      .data$edge_day_duration - lubridate::as.duration(lubridate::as_datetime(.data$edge_day) + lubridate::ddays(1) - .data$edge)
    )) %>%
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
  res[res < 0] <- Inf
  return(res)
}

#' Map legend title table
#'
#' Store legend titles for deployment visualizations: RAI, effort, number of
#' observations, etc.
#' Returns a data frame of all titles with the following columns:
#' - `feature`: Deployment feature to visualize.
#' - `legend_title`: Legend title.
#'
#' @noRd
#' @usage map_legend_title()
map_legend_title <- function() dplyr::as_tibble(mapdep_legend_titles)

mapdep_legend_titles <- structure(list(
  feature = c(
    "n_species",
    "n_obs",
    "n_individuals",
    "rai",
    "rai_individuals",
    "effort"
  ),
  legend_title = c(
    "Number of detected species",
    "Number of observations",
    "Number of individuals",
    "RAI",
    "RAI (individuals)",
    "Effort"
  )
))

#' Get legend title for deployment visualizations
#'
#' @param feature Character, one of:
#'   - `n_species`
#'   - `n_obs`
#'   - `rai`
#'   - `effort`
#' @importFrom dplyr .data %>%
#' @noRd
get_legend_title <- function(feat) {
  # get all legend titles
  titles <- map_legend_title()
  # return the legend title we need
  titles %>%
    dplyr::filter(.data$feature == feat) %>%
    dplyr::pull(.data$legend_title)
}

#' Add unit to legend title
#'
#' This function is useful when a unit (e.g. temporal unit) should be added to
#' legend title.
#'
#' @param title A character with legend title.
#' @param unit Character with unit to add to `title`.
#' @param use_brackets Logical.
#'   If `TRUE` (default) `unit` is wrapped between brackets, e.g. `(days)`.
#' @noRd
#' @usage map_legend_title("My title", unit = "day", use_bracket = TRUE)
add_unit_to_legend_title <- function(title, unit = NULL, use_brackets = TRUE) {
  if (is.null(unit)) {
    title
  } else {
    if (use_brackets == TRUE) {
      unit <- paste0("(", unit, ")")
    }
    paste(title, unit)
  }
}

#' Create columns, but only if they don't exist yet!
#'
#' Using dplyr::mutate(), add a new column, but only if it's missing
#'
#' @inherit dplyr::mutate
#' @noRd
#' @examples 
#' \dontrun{
#' # doesn't add a column when it already exists
#' mutate_when_missing(cars, speed = "warp 9")
#' # but does add a column when it doesn't exist yet
#' mutate_when_missing(cars, space = "The final frontier")
#' }
mutate_when_missing <- function(.data,...){
  dots <- substitute(list(...))[-1]
  cols_to_check <- names(sapply(dots, deparse))
  columns_to_add <- cols_to_check[!cols_to_check %in% colnames(.data)]
  if(!rlang::is_empty(columns_to_add)){.data <- dplyr::mutate(.data,...)}
  return(.data)
}

#' Add taxonomic information to observations
#' 
#' This help function adds taxonomic information in `taxonomic` element of
#' metadata to `observations`. Notice that higher classification, i.e. new
#' fields in v1.0, are removed.
#' 
#' @param package Camera trap data package.
#' @return Camera trap data package with taxonomic related cols added to
#'   `.$data$observations`.
#' @noRd
add_taxonomic_info <- function(package) {
  # get taxonomic info from metadata
  taxon_infos <- get_species(package)
  # select only basic taxonomic info as in v0.1.6 (no higher
  # classification)
  taxon_infos <- dplyr::select(
    taxon_infos,
    dplyr::all_of("scientificName"),
    dplyr::any_of(c("taxonID", "taxonIDReference", "taxonRank")),
    dplyr::starts_with("vernacularNames")
  )
  # Add taxon infos to observations
  if (!is.null(taxon_infos)) {
    cols_taxon_infos <- names(taxon_infos)
    observations <-
      dplyr::left_join(
        package$data$observations,
        taxon_infos,
        by = "scientificName"
      )
    if ("taxonID.y" %in% colnames(observations)) {
      # Keep only the taxonID added by join with taxonomy
      observations <-
        observations %>%
        dplyr::rename("taxonID" = "taxonID.y") %>%
        dplyr::select(-"taxonID.x")
    }
    package$data$observations <- observations
  }
  return(package)
}

#' Add speed, radius, angle to a Camtrap DP version 0.1.6
#' 
#' This help function is a patch for adding non-standard columns `speed`,
#' `radius` and `angle` in `observations`. See
#' https://github.com/inbo/camtraptor/issues/185
#' 
#' @param obs Data.frame with `observations` from a Camtrap DP package, version
#'   0.1.6.
#' @return Data.frame with `observations`.
#' @noRd
add_speed_radius_angle <- function(obs){
  obs_col_names <- names(obs)
  if (all(c("X22", "X23", "X24") %in% names(obs))) {
    obs <- obs %>%
      dplyr::rename(speed = "X22", radius = "X23", angle = "X24")
    message(
      paste("Three extra fields in `observations` interpreted as `speed`,",
            "`radius` and `angle`."
      )
    )
  }
  return(obs)
}

#' Convert a Camtrap DP to version 0.1.6
#' 
#' This conversion function takes as input a Camtrap DP and returns 
#' 
#' @param package Camera trap data package object.
#' @param from Character identifying the version of `package`.
#' @param media If `TRUE` (default), read media records into memory. If `FALSE`,
#'   ignore media file to speed up reading larger Camtrap DP packages.
#' @noRd
convert_to_0.1.6 <- function(package, from = "1.0", media = TRUE){
  if (from == "0.1.6") {
    message(glue::glue("package's version: {from}. No conversion needed."))
    return(package)
  }
  # check version
  supported_versions <- c("1.0")
  assertthat::assert_that(
    from %in% supported_versions,
    msg = glue::glue(
      "Only conversion from ", 
      glue::glue_collapse(supported_versions, sep = " ", last = " and "),
      " to `0.1.6` is supported."
    )
  )
  # check data slot is present in package
  assertthat::assert_that(
    "data" %in% names(package),
    msg = "Can't find `data` element in `package`."
  )
  
  # notify about conversion
  message(glue::glue(
    "The dataset uses Camtrap DP version 1.0, it has been converted to 0.1.6.",
    "See https://inbo.github.io/camtraptor/#camtrap-dp for details.",
    .sep = "\n"
  ))
  # convert metadata
  package <- convert_metadata_to_0.1.6(package, from)  
  # convert deployments
  package <- convert_deployments_to_0.1.6(package, from)  
  # convert media
  if (media) {
    package <- convert_media_to_0.1.6(package, from)
  }
  # convert observations
  package <- convert_observations_to_0.1.6(package, from)  
  
  return(package)
}

#' Convert metadata to Camtrap DP version 0.1.6
#' 
#' Convert metadata of a Camtrap DP from version 1.0 to 0.1.6 to avoid
#' breaking changes
#' 
#' @param package Camera trap data package object.
#' @param from Character identifying the version of `package`.
#' @return Camera trap data package object with converted `metadata`.
#' @noRd
#' @importFrom dplyr %>% .data
convert_metadata_to_0.1.6 <- function(package, from = "1.0"){
  authors <- purrr::map_df(package$contributors, unlist)
  if ("role" %in% names(authors)) {
    deprecated_roles <- c("author", "maintainer")
    if (any(deprecated_roles %in% authors$role)) {
      warning(glue::glue(
        "Roles ",
        glue::glue_collapse(deprecated_roles, sep = " ", last = " and "),
        " are deprecated in version {from}."
      ))
    }
  }
  if ("organizations" %in% names(package)) {
    warning(glue::glue(
      "The field `organizations` is deprecated in version {from}."
    ))
  }
  if ("animalTypes" %in% names(package)) {
    warning(glue::glue(
      "The field `animalTypes` is deprecated in version {from}."
    ))
  }
  names(package)[names(package) == "observationLevel"] <- "classificationLevel"
  if ("sequenceInterval" %in% names(package$project)) {
    warning(glue::glue(
      "The field `sequenceInterval` is deprecated in version {from}."
    ))
  }
  package$platform <- package$sources[[1]]
  
  # `title` value of the first contributor with role `rightsHolder`
  package$rightsHolder <- purrr::map_df(package$contributors, unlist) %>%
    dplyr::filter(.data$role == "rightsHolder") %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$title)
  
  # downconvert `captureMethod` to values from Camtrap DP version v0.1.6
  package$project$captureMethod <-
    dplyr::case_match(
      .default = package$project$captureMethod,
      package$project$captureMethod,
      "activityDetection" ~ "motionDetection"
    )
  
  return(package)
}

#' Convert deployments to Camtrap DP version 0.1.6
#' 
#' Convert deployments of a Camtrap DP from version 1.0 to 0.1.6 to avoid
#' breaking changes
#' 
#' @param package Camera trap data package object.
#' @param from Character identifying the version of `package`.
#' @return Camera trap data package object with converted `deployments`.
#' @noRd
#' @importFrom dplyr %>% .data
convert_deployments_to_0.1.6 <- function(package, from = "1.0") {
  
  # check deployments slot is present
  assertthat::assert_that(
    "deployments" %in% names(package$data),
    msg = "Can't find `deployments` element in `package$data`."
  )
  
  deployments <- package$data$deployments
  
  # rename required fields where needed
  deployments <- deployments %>%
    dplyr::relocate("latitude", .after = "longitude")
  deployments <- deployments %>%
    dplyr::rename(start = "deploymentStart",
                  end = "deploymentEnd")
  if ("cameraDelay" %in% names(deployments)) {
    deployments <- deployments %>%
      dplyr::rename(cameraInterval = "cameraDelay")
  }
  # ignore detectionDistance
  deployments$detectionDistance <- NULL
  # ignore cameraDepth
  deployments$cameraDepth <- NULL
  if ("baitUse" %in% names(deployments)) {
    # baitUse values in version 0.1.6
    bait_uses_old <- c("none", "scent", "food", "visual", "acoustic", "other")
    # transform Boolean to character and set FALSE to "none", TRUE to "other".
    # Do not change NAs
    deployments <- deployments %>%
      dplyr::mutate(baitUse = as.character(.data$baitUse)) %>%
      dplyr::mutate(baitUse = dplyr::case_when(
        .data$baitUse == "FALSE" ~ "none", 
        is.na(.data$baitUse) ~ NA_character_,
        .default = "other")
      )
    # retrieve specific bait use info from tags if present
    if ("deploymentTags" %in% names(deployments)) {
      deployments <- deployments %>%
        dplyr::mutate(bait_use = stringr::str_extract(
          string = .data$deploymentTags, 
          pattern = "(?<=bait:).[a-zA-Z]+")) %>%
        #remove whitespaces at the begin and end of the string (there shouldn't be)
        dplyr::mutate(bait_use = stringr::str_trim(.data$bait_use)) %>%
        # set baitUse based on found tags
        dplyr::mutate(baitUse = dplyr::if_else(
          .data$bait_use %in% bait_uses_old,
          .data$bait_use,
          .data$baitUse)) %>%
        dplyr::select(-"bait_use")
    }
    # set baitUse to factor
    deployments <- deployments %>%
      dplyr::mutate(baitUse = factor(.data$baitUse, levels = bait_uses_old))
  }
  if ("session" %in% names(deployments)) {
    warning(glue::glue(
      "The field `session` of deployments is deprecated in version {from}."
    ))
  } else {
    deployments <- deployments %>%
      dplyr::mutate(session = NA)
  }
  if ("deploymentGroups" %in% names(deployments)) {
    # map to session and then remove
    deployments <- deployments %>%
      dplyr::mutate(session = dplyr::case_when(
        !is.na(.data$deploymentGroups) & !is.na(.data$session) ~ 
          stringr::str_c(.data$session, 
                         .data$deploymentGroups, 
                         sep = " | "),
        !is.na(.data$deploymentGroups) & is.na(.data$session) ~
          .data$deploymentGroups,
        is.na(.data$deploymentGroups) & !is.na(.data$session) ~
          .data$session,
        # if there is no value for neither deploymentGroups or session:
        .default = NA)) %>%
      dplyr::select(-"deploymentGroups")
  }
  if ("array" %in% names(deployments)) {
    warning(glue::glue(
      "The field `array` of deployments is deprecated in version {from}."
    ))
  } else {
    deployments <- deployments %>%
      dplyr::mutate(array = NA)
  }
  if ("_id" %in% names(deployments)) {
    warning(glue::glue(
      "The field `_id` of deployments is deprecated in version {from}."
    ))
  } else {
    deployments <- deployments %>%
      dplyr::mutate("_id" = NA)
  }
  if ("deploymentTags" %in% names(deployments)) {
    deployments <- deployments %>%
      dplyr::rename(tags = "deploymentTags")
  }
  if ("deploymentComments" %in% names(deployments)) {
    deployments <- deployments %>%
      dplyr::rename(comments = "deploymentComments")
  }
  
  package$data$deployments <- deployments
  return(package)
}

#' Convert media to Camtrap DP version 0.1.6
#' 
#' Convert media of a Camtrap DP from version 1.0 to 0.1.6 to avoid
#' breaking changes. Notice that this function `MUST` be run before
#' `convert_observations_to_0.1.6()`.
#' 
#' @param package Camera trap data package object.
#' @param from Character identifying the version of `package`.
#' @return Camera trap data package object with converted `media`.
#' @noRd
#' @importFrom dplyr %>% .data
convert_media_to_0.1.6 <- function(package, from = "1.0") {
  
  # check media slot is present
  assertthat::assert_that(
    "media" %in% names(package$data),
    msg = "Can't find `deployments` element in `package$data`."
  )
  
  if (is.null(package$data$media)) {
    return(package)
  }
  
  # check observations slot is present
  assertthat::assert_that(
    "observations" %in% names(package$data),
    msg = "Can't find `observations` element in `package$data`."
  )
  
  media <- package$data$media
  observations <- package$data$observations
  
  # create sequenceID for media linked to event-based observations as 
  # sequenceID is used by `get_record_table()`
  event_obs <- observations %>% 
    dplyr::filter(is.na(.data$mediaID)) %>%
    dplyr::select("eventID", "deploymentID", "eventStart", "eventEnd") %>%
    # eventID is not anymore required in v1.0, remove where not present
    dplyr::filter(!is.na(.data$eventID))
  
  # Join on deploymentID and timestamp between eventStart and eventEnd
  by <- dplyr::join_by("deploymentID", 
                       dplyr::between(x$timestamp, y$eventStart, y$eventEnd))
  # Join media with event-based observations (obs without mediaID)
  media <- media %>%
    dplyr::full_join(event_obs, by) %>%
    dplyr::rename(sequenceID = "eventID") %>%
    dplyr::select(-c("eventStart", "eventEnd")) %>%
    dplyr::relocate("sequenceID", .after = "deploymentID")
  
  if ("filePublic" %in% names(media))  {
    media$filePublic <- NULL
  }
  if ("favorite" %in% names(media)) {
    media <- media %>%
      dplyr::rename(favourite = "favorite")
  }
  if ("mediaComments" %in% names(media)) {
    media <- media %>%
      dplyr::rename(comments = "mediaComments")
  }
  if ("_id" %in% names(media)) {
    warning(glue::glue(
      "The field `_id` of media is deprecated in version {from}."
    ))
  } else {
    media <- media %>%
      dplyr::mutate("_id" = NA)
  }
  
  # convert captureMethod value to v1.6.0 terms
  media <- media %>% 
    dplyr::mutate(
      captureMethod = factor(
        ifelse(.data$captureMethod == "activityDetection",
               "motionDetection",
               as.character(.data$captureMethod))
      )
    )
  
 package$data$media <- media
  return(package)
}

#' Convert observations to Camtrap DP version 0.1.6
#' 
#' Convert observations of a Camtrap DP from version 1.0 to 0.1.6 to avoid
#' breaking changes
#' 
#' @param package Camera trap data package object.
#' @param from Character identifying the version of `package`.
#' @return Camera trap data package object with converted `observations`.
#' @noRd
#' @importFrom dplyr %>% .data
convert_observations_to_0.1.6 <- function(package, from = "1.0") {
  
  # check observations slot is present
  assertthat::assert_that(
    "observations" %in% names(package$data),
    msg = "Can't find `observations` element in `package$data`."
  )
  
  observations <- package$data$observations
  
  # Get media-based observations
  media_observations <- observations %>%
    dplyr::filter(.data$observationLevel == "media")
  
  # Extract first not NA individualPositionRadius and individualPositionAngle
  # for each `eventID`
  obs_first_radius_angle <- 
    media_observations %>%
    dplyr::filter(!is.na(.data$individualPositionRadius),
                  !is.na(.data$individualPositionAngle)) %>%
    dplyr::group_by(.data$eventID, .data$individualID) %>%
    # Take the very first row with the lowest eventStart.
    # Notice that multiple media could have the same value of eventStart
    # Use with_ties = FALSE to be sure to take the very first element.
    dplyr::slice_min(.data$eventStart, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("eventID",
                    "individualID",
                    "individualPositionRadius", 
                    "individualPositionAngle")) %>%
    dplyr::rename_with(~ paste0("media_", .x),
                       dplyr::starts_with("individualPosition")
    )
  
  # Get event-based observations
  event_observations <- observations %>%
    dplyr::filter(.data$observationLevel == "event")
  
  # Add angle/radius to event based observations if missing
  event_observations <- event_observations %>%
    dplyr::left_join(obs_first_radius_angle,
                     by = c("eventID", "individualID")) %>%
    dplyr::mutate(
      individualPositionAngle = dplyr::if_else(
        condition = is.na(.data$individualPositionAngle),
        true = .data$media_individualPositionAngle,
        false = .data$individualPositionAngle),
      individualPositionRadius = dplyr::if_else(
        condition = is.na(.data$individualPositionRadius),
        true = .data$media_individualPositionRadius,
        false = .data$individualPositionRadius)) %>%
    dplyr::select(-c("media_individualPositionAngle",
                     "media_individualPositionRadius")
    )
  
  # only event-type obs are supported
  observations <- event_observations
  
  if ("eventID" %in% names(observations)) {
    observations <- observations %>%
      dplyr::rename(sequenceID = "eventID")
  } else {
    observations <- observations %>%
      dplyr::mutate(sequenceID = NA)
  }
  observations <- dplyr::relocate(observations, 
                                  "sequenceID", 
                                  .after = "deploymentID"
  )
  observations <- observations %>%
    dplyr::rename(timestamp = "eventStart")
  
  observations$eventEnd <- NULL
  observations$observationLevel <- NULL
  
  if ("cameraSetupType" %in% names(observations)) {
    observations <- observations %>%
      dplyr::rename(cameraSetup = "cameraSetupType")
  } else {
    observations <- observations %>%
      dplyr::mutate("cameraSetup" = NA)
  }
  if ("countNew" %in% names(observations)) {
    warning(glue::glue(
      "The field `countNew` of observations is deprecated in version {from}."
    ))
  } else {
    observations <- observations %>%
      dplyr::mutate("countNew" = NA)
  }
  observations <- dplyr::relocate(observations, 
                                  "countNew", 
                                  .after = dplyr::any_of("count")
  )
  if ("behavior" %in% names(observations)) {
    observations <- observations %>%
      dplyr::rename(behaviour = "behavior")
  }
  if ("classificationProbability" %in% names(observations)) {
    observations <- observations %>%
      dplyr::rename(classificationConfidence = "classificationProbability")
  }
  if ("observationTags" %in% names(observations)) {
    observations$observationTags <- NULL
  }
  if ("observationComments" %in% names(observations)) {
    observations <- observations %>%
      dplyr::rename(comments = "observationComments")
  }
  if ("_id" %in% names(observations)) {
    warning(glue::glue(
      "The field `_id` of observations is deprecated in version {from}."
    ))
  } else {
    observations <- observations %>%
      dplyr::mutate("_id" = NA)
  }
  if ("individualSpeed" %in% names(observations)) {
    observations <- observations %>%
      dplyr::rename(speed = "individualSpeed")
  }
  if ("individualPositionRadius" %in% names(observations)) {
    observations <- observations %>%
      dplyr::rename(radius = "individualPositionRadius")
  }
  if ("individualPositionAngle" %in% names(observations)) {
    observations <- observations %>%
      dplyr::rename(angle = "individualPositionAngle")
  }
  # remove bounding box related cols if present
  observations <- observations %>% dplyr::select(-dplyr::starts_with("bbox"))
  # add taxonID if missing
  if(!"taxonID" %in% colnames(observations)){
    observations <- observations %>% 
      dplyr::mutate(taxonID = NA_character_)
  }
  # add taxonIDReference if missing
  if(!"taxonIDReference" %in% colnames(observations)){
    observations <- observations %>% 
      dplyr::mutate(taxonIDReference = NA_character_)
  }
  
  package$data$observations <- observations
  return(package)
}

#' Order the columns of deployments
#' 
#' @param df A data.frame with `deployments`
#' @return Same data.frame as `df` with the right order of columns
#' @noRd
order_cols_deployments <- function(df) {
  # Set right order of columns
  df %>%
    dplyr::relocate(
      dplyr::any_of(c("deploymentID",
                      "locationID",
                      "locationName",
                      "longitude",
                      "latitude",
                      "coordinateUncertainty",
                      "start",
                      "end",
                      "setupBy",
                      "cameraID", 
                      "cameraModel",
                      "cameraInterval",
                      "cameraHeight",
                      "cameraTilt", 
                      "cameraHeading",
                      "timestampIssues",
                      "baitUse",
                      "session",
                      "array",
                      "featureType",
                      "habitat", 
                      "tags",
                      "comments", 
                      "_id")
      )
    )
}

#' Order the columns of media
#' 
#' @param df A data.frame with `media`
#' @return Same data.frame as `df` with the right order of columns
#' @noRd
order_cols_media <- function(df) {
  # Set right order of columns
  df %>%
    dplyr::relocate(
      dplyr::any_of(c("mediaID",
                      "deploymentID",
                      "sequenceID",
                      "captureMethod",
                      "timestamp",
                      "filePath",
                      "fileName",
                      "fileMediatype", 
                      "exifData",
                      "favourite",
                      "comments",
                      "_id")
      )
    )
}

#' Order the columns of observations
#' 
#' @param df A data.frame with `observations`
#' @return Same data.frame as `df` with the right order of columns
#' @noRd
order_cols_observations <- function(df) {
  # Set right order of columns
  df %>%
    dplyr::relocate(
      dplyr::any_of(c("observationID",
                      "deploymentID",
                      "sequenceID",
                      "mediaID",
                      "timestamp",
                      "observationType",
                      "cameraSetup",
                      "taxonID",
                      "taxonIDReference",
                      "scientificName",
                      "taxonRank",
                      dplyr::starts_with("vernacularNames"),
                      "count",
                      "countNew",
                      "lifeStage",
                      "sex",
                      "behaviour",
                      "individualID",
                      "speed",
                      "radius",
                      "angle",
                      "classificationMethod",
                      "classifiedBy",
                      "classificationTimestamp",
                      "classificationConfidence",
                      "comments",
                      "_id")
      )
    )
}

#' Add deployment coordinates to observations
#' 
#' This function adds deployment coordinates to observations based on
#' `deploymentID`.
#' 
#' @param package Camera trap data package object.
#' @return Camera trap data package object with `observations` updated.
#' @noRd
add_coordinates <- function(package) {
  
  deployments <- package$data$deployments
  observations <- package$data$observations
  
  # add coordinates to observations
  observations <- observations %>%
    dplyr::left_join(deployments %>% 
                       dplyr::select("deploymentID", "longitude", "latitude"),
                     by = "deploymentID")
  
  package$data$observations <- observations
  return(package)
}
