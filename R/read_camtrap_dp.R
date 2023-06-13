#' Read a Camtrap DP
#'
#' Reads files from a [Camera Trap Data Package](
#' https://tdwg.github.io/camtrap-dp) into memory.
#' All datetime information is automatically transformed to Coordinated
#' Universal Time (UTC).
#' Vernacular names found in the metadata (`package$taxonomic`) are added to the
#' `observations` data frame.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @param media If `TRUE`, read media records into memory. If `FALSE`, ignore
#'   media file to speed up reading larger Camtrap DP packages.
#' @param path Path to the directory containing the datapackage. Use  `file`
#'   with path or URL to a `datapackage.json` file instead.
#' @return List describing a Data Package (as returned by
#'   [frictionless::read_package()]) containing the original metadata, as well
#'   as a property `data` containing the data as three data frames:
#'   1. `deployments`
#'   2. `media`
#'   3. `observations`
#' @family read functions
#' @export
#' @importFrom dplyr %>% .data
#' @examples
#' \dontrun{
#' # Read Camtrap DP package
#' camtrap_dp_file <- system.file("extdata", "mica", "datapackage.json", package = "camtraptor")
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_file)
#'
#' # Read Camtrap DP package and ignore media file
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_file, media = FALSE)
#'
#' # If parsing issues while reading deployments, observations or media arise,
#' # use readr::problems()
#' camtrap_dp_file_with_issues <- system.file(
#'   "extdata",
#'   "mica_parsing_issues",
#'   "datapackage_for_parsing_issues.json",
#'   package = "camtraptor"
#' )
#' muskrat_coypu_with_issues <- read_camtrap_dp(camtrap_dp_file_with_issues, media = TRUE)
#' readr::problems(muskrat_coypu_with_issues$data$deployments)
#' readr::problems(muskrat_coypu_with_issues$data$observations)
#' readr::problems(muskrat_coypu_with_issues$data$media)
#' }
read_camtrap_dp <- function(file = NULL,
                            media = TRUE,
                            path = lifecycle::deprecated()) {
  # check path (deprecated)
  warning_detail <- paste(
    "Use argument `file` containing the path or URL to the `datapackage.json`",
    "file. The use of parameter `path` with path to the local directory is ",
    "deprecated since version 0.6.0."
  )
  if (lifecycle::is_present(path) | (!is.null(file) && dir.exists(file))) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "read_camtrap_dp(path)",
      details = warning_detail
    )
  }

  # define the right file value
  if (lifecycle::is_present(path)) {
    file <- file.path(path, "datapackage.json")
  }
  # file value is a valid path
  if (dir.exists(file)) {
    file <- file.path(file, "datapackage.json")
  }
  
  # check media
  assertthat::assert_that(
    media %in% c(TRUE, FALSE),
    msg = "`media` must be a logical: TRUE or FALSE"
  )
  
  # read package (metadata)
  package <- frictionless::read_package(file)
  
  # get package version
  profile <- package$profile
  if (profile == "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0-rc.1/deployments-table-schema.json") {
    version <- "1.0-rc.1"
  } else {
    if (profile == "https://raw.githubusercontent.com/tdwg/camtrap-dp/0.1.6/camtrap-dp-profile.json") {
      version <- "0.1.6"
    } else {
      version <- profile
    }
  }
  
  # check that the version of the camtrap-dp is supported by camtraptor. At the
  # moment we support only camtra-dp versions 0.1.6 and 1.0-rc.1
  supported_versions <- c("1.0-rc.1", "0.1.6")
  assertthat::assert_that(
    version %in% supported_versions,
    msg = glue::glue(
      "Version {version} not supported. ",
      "camtraptor supports camtrap-dp versions: ",
      glue::glue_collapse(glue::glue("{supported_versions}"), last = " and "),
      ".",
      .sep = ""
    )
  )
  
  # get resource names
  resource_names <- purrr::map_chr(package$resource, ~.$name)
  
  # transform package metadata formatted using Camtrap DP 0.6 standard to avoid
  # breaking changes
  if (version == "https://raw.githubusercontent.com/tdwg/camtrap-dp/0.6/camtrap-dp-profile.json") {
    names(package)[names(package) == "eventInterval"] <- "sequenceInterval"
  }
  
  # read deployments
  deployments <- frictionless::read_resource(package, "deployments")
  check_reading_issues(deployments, "deployments")
  
  # transform deployments formatted using Camtrap DP 1.0-rc.1 standard to avoid
  # breaking changes
  if (version == "1.0-rc.1") {
    # rename required fields where needed
    deployments <- deployments %>%
      dplyr::relocate(latitude, .after = longitude)
    deployments <- deployments %>%
      dplyr::rename(start = eventStart,
                    end = eventEnd)
    if ("cameraDelay" %in% names(deployments)) {
      deployments <- deployments %>%
        dplyr::rename(cameraInterval = cameraDelay)
    }
    # ignore detectionDistance
    deployments$detectionDistance <- NULL
    if ("baitUse" %in% names(deployments)) {
      # baitUse values in version 0.1.6
      bait_uses_old <- c("none", "scent", "food", "visual", "acoustic", "other")
      # transform Boolean to character and set FALSE to "none", TRUE to "other".
      # Do not change NAs
      deployments <- deployments %>%
        dplyr::mutate(baitUse = as.character(.data$baitUse)) %>%
        dplyr::mutate(baitUse = dplyr::if_else(.data$baitUse == "FALSE", 
                                               "none", 
                                               "other"
                                               )
        )
      # retrieve specific bait use info from tags if present
      if ("deploymentTags" %in% names(deployments)) {
        deployments <- deployments %>%
          dplyr::mutate(bait_use = stringr::str_extract(
            string = .data$deploymentTags, 
            pattern = "(?<=bait:).[a-zA-Z]+"
            )
          )
      }
      # set baitUse based on found tags
      deployments <- deployments %>%
        dplyr::mutate(baitUse = if_else(is.na(.data$baitUse) & 
                                          bait_use %in% bait_uses_old,
                                        deployments$bait_use)) %>%
      # set baitUse to factor
        dplyr::mutate(baitUse = factor(.data$baitUse, levels = bait_uses_old))
    }
    if ("session" %in% names(deployments)) {
      warning(glue::glue("The field `session` of deployments is deprecated in",
                         "version {version} and is left empty.")
      )
      deployments <- deployments %>%
        dplyr::mutate(session = NA)
    }
    if ("array" %in% names(deployments)) {
      warning(glue::glue("The field `_id` of deployments is deprecated in",
                         "version {version} and is left empty.")
      )
      deployments <- deployments %>%
        dplyr::mutate(array = NA)
    }
    if ("_id" %in% names(deployments)) {
      warning(glue::glue("The field `array` of deployments is deprecated in",
                         "version {version} and is left empty.")
      )
      deployments <- deployments %>%
        dplyr::mutate("_id" = NA)
    }
    if ("deploymentTags" %in% names(deployments)) {
      deployments <- deployments %>%
        dplyr::rename(tags = deploymentTags)
    }
    if ("deploymentComments" %in% names(deployments)) {
      deployments <- deployments %>%
        dplyr::rename(comments = deploymentComments)
    }
  }
  
  if (isTRUE(media)) {
    # read media
    media <- frictionless::read_resource(package, "media")
    check_reading_issues(media, "media")
    # transform media formatted using Camtrap DP 1.0-rc.1 standard to avoid
    # breaking changes
    if (version == "1.0-rc.1") {
      media <- media %>%
        dplyr::rename(sequenceID = eventID)
      if ("favorite" %in% names(media)) {
        media <- media %>%
          dplyr::rename(favourite = favorite)
      }
      if ("mediaComments" %in% names(media)) {
        media <- media %>%
          dplyr::rename(comments = mediaComments)
      }
    }
  }
  
  observations <- frictionless::read_resource(package, "observations")
  check_reading_issues(deployments, "observations")
  
  # transform media formatted using Camtrap DP 1.0-rc.1 standard to avoid
  # breaking changes
  if (version == "1.0-rc.1") {
    # only event-type obs are supported
    n_media_obs <- observations %>%
      dplyr::filter(.data$observationLevel == "media")
    if (n_media_obs > 0) {
      msg <- glue::glue(
        "camtraptor has been developed to work with event-based observations. ",
        "{n_media_obs} media-based observations removed."
      )
      message(msg)
    }
    observations <- observations %>%
      dplyr::filter(.data$observationLevel == "event")
  }
  
  # patch for non-standard values speed, radius, angle
  # see https://github.com/inbo/camtraptor/issues/185
  obs_col_names <- names(observations)
  if (all(c("X22", "X23", "X24") %in% names(observations))) {
    observations <- observations %>%
      dplyr::rename(speed = "X22", radius = "X23", angle = "X24")
    message(
      paste("Three extra fields in `observations` interpreted as `speed`,",
            "`radius` and `angle`."
      )
    )
  }
  
  # create first version datapackage with resources in data element
  data <- list(
    "deployments" = deployments,
    "media" = NULL,
    "observations" = observations
  )
  package$data <- data
  
  # get taxonomic info
  taxon_infos <- get_species(package)
  # add vernacular names to observations
  if (!is.null(taxon_infos)) {
    cols_taxon_infos <- names(taxon_infos)
    observations <-
      dplyr::left_join(
        observations,
        taxon_infos,
        by  = c("taxonID", "scientificName")
      )
    observations <-
      observations %>%
      dplyr::relocate(dplyr::one_of(cols_taxon_infos), .after = "cameraSetup")
    # Inherit parsing issues from reading
    attr(observations, which = "problems") <- issues_observations
  }

  # return list resources
  if (is.data.frame(media)) {
    data$media <- media
    package$data <- data
  }
  check_package(package)
}
