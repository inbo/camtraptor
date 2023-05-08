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
                            media = lifecycle::deprecated(),
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

  # check media (deprecated)
  warning_detail <- paste(
    "The use of argument `media` is deprecated since version 0.20.0", 
    "and will be ignored."
  )
  if (lifecycle::is_present(media)) {
    lifecycle::deprecate_warn(
      when = "0.20.0",
      what = "read_camtrap_dp(file, media)",
      details = warning_detail
    )
  }
  
  # read package (metadata)
  package <- frictionless::read_package(file)
  
  
  # read deployments
  deployments <- frictionless::read_resource(package, "deployments")
  issues_deployments <- readr::problems(deployments)
  if (nrow(issues_deployments) > 0) {
    warning(glue::glue(
      "One or more parsing issues occurred while reading deployments. ",
      "See `?read_camtrap_dp()` for examples on how to use ",
      "`readr::problems()`."
    ))
  }
  
  # read observations
  observations <- frictionless::read_resource(package, "observations")
  issues_observations <- readr::problems(observations)
  if (nrow(issues_observations) > 0) {
    warning(glue::glue(
      "One or more parsing issues occurred while reading observations. ",
      "See `?read_camtrap_dp()` for examples on how to use ",
      "`readr::problems()`."
    ))
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
  
  media <- frictionless::read_resource(package, "media")
  issues_media <- readr::problems(media)
  if (nrow(issues_media) > 0) {
    warning(glue::glue(
      "One or more parsing issues occurred while reading media. ",
      "See `?read_camtrap_dp()` for examples on how to use ",
      "`readr::problems()`."
    ))
  }

  # return list resources
  data <- list(
    "deployments" = deployments,
    "media" = media,
    "observations" = observations
  )
  package$data <- data
  package
}
