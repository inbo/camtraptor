#' Read a Camtrap DP
#'
#' Reads files from a [Camera Trap Data Package](
#' https://camtrap-dp.tdwg.org) into memory.
#' All datetime information is automatically transformed to Coordinated
#' Universal Time (UTC).
#' Vernacular names found in the metadata (`package$taxonomic`) are added to the
#' `observations` data frame.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @param media If `TRUE` (default), read media records into memory. If `FALSE`,
#'   ignore media file to speed up reading larger Camtrap DP packages.
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
#' camtrap_dp_file <- system.file(
#'   "extdata", "mica", "datapackage.json", 
#'   package = "camtraptor"
#' )
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
  # Check path (deprecated)
  warning_detail <- paste(
    "Use argument `file` containing the path or URL to the `datapackage.json`",
    "file. The use of parameter `path` with path to the local directory is ",
    "deprecated since version 0.6.0."
  )
  if (lifecycle::is_present(path) | (!is.null(file) && dir.exists(file))) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "camtraptor::read_camtrap_dp(path)",
      details = warning_detail
    )
  }
  # Define the right file value
  if (lifecycle::is_present(path)) {
    file <- file.path(path, "datapackage.json")
  }
  # File value is a valid path
  if (dir.exists(file)) {
    file <- file.path(file, "datapackage.json")
  }
  # Check media arg
  assertthat::assert_that(
    media %in% c(TRUE, FALSE),
    msg = "`media` must be a logical: TRUE or FALSE"
  )
  
  # Read package (metadata)
  package <- frictionless::read_package(file)
  
  # Check Camtrap DP version is supported
  version <- get_version(profile = package$profile)
  supported_versions <- c("0.1.6", "1.0")
  assertthat::assert_that(
    version %in% supported_versions,
    msg = glue::glue(
      "Version `{version}` is not supported. Supported versions: ",
      glue::glue_collapse(supported_versions, sep = " ", last = " and ")
    )
  )
    
  # Get resource names
  resource_names <- frictionless::resources(package)

  # Check needed resources are present
  resources_to_read <- c("deployments", "media", "observations")
  assertthat::assert_that(
    all(resources_to_read %in% resource_names),
    msg = glue::glue(
      "One or more resources among ", 
      glue::glue_collapse(resources_to_read, sep = ", ", last = " and "),
      " is missing."
    )
  )
  
  # Read deployments
  deployments <- frictionless::read_resource(package, "deployments")
  issues_deployments <- check_reading_issues(deployments, "deployments")
  # Read observations (needed to create sequenceID in media)
  observations <- frictionless::read_resource(package, "observations")
  issues_observations <- check_reading_issues(observations, "observations")
  
  if (version == "0.1.6"){
    observations <- add_speed_radius_angle(observations)
  }
  
  # Create first version datapackage with resources in data slot
  data <- list(
    "deployments" = deployments,
    "media" = NULL,
    "observations" = observations
  )
  
  package$data <- data
  
  # Read media if needed
  if (media) {
    media_df <- frictionless::read_resource(package, "media")
    issues_media <- check_reading_issues(media_df, "media")
    data$media <- media_df
  }
 
  package$data <- data
  check_package(package, media = media)
  
  package <- add_taxonomic_info(package)
  
  # Convert to 0.1.6
  if (version == "1.0") {
    package <- convert_to_0.1.6(package, version, media = media)
  }
  
  # Order columns
  package$data$deployments <- order_cols_deployments(package$data$deployments)
  package$data$observations <- order_cols_observations(
    package$data$observations
  )
  if (!is.null(package$data$media)) {
    package$data$media <- order_cols_media(package$data$media)
  }
  
  check_package(package, media = media)
  
  # Inherit parsing issues from reading
  attr(package$data$deployments, which = "problems") <- issues_deployments
  attr(package$data$observations, which = "problems") <- issues_observations
  if (media) {
    attr(package$data$media, which = "problems") <- issues_media
  }

  return(package)
}
