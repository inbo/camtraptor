#' Read a Wildlife Insights export
#'
#' Reads files from an unzipped [Wildlife Insights](
#' https://www.wildlifeinsights.org/) export into memory.
#' Data can be exported from Wildlife Insights as a [public](
#' https://www.wildlifeinsights.org/get-started/data-download/public) or
#' [private](https://www.wildlifeinsights.org/get-started/download/private)
#' download.
#' The function transforms data and metadata to a [Camera Trap Data Package](
#' https://tdwg.github.io/camtrap-dp) which can be written to file with
#' [frictionless::write_package()].
#'
#' **The function has only been tested on image-based projects.**
#'
#' @param directory Path to local directory to read files from.
#'   The function expects `projects.csv`, `deployments.csv`, `cameras.csv`, and
#'   `images.csv`.
#' @return CSV (data) files written to disk.
#' @export
#' @importFrom dplyr %>% .data
#' @family read functions
read_wi <- function(directory = ".") {
  # Check files
  projects_file <- file.path(directory, "projects.csv")
  assertthat::assert_that(file.exists(projects_file))
  cameras_file <- file.path(directory, "cameras.csv")
  assertthat::assert_that(file.exists(cameras_file))
  deployments_file <- file.path(directory, "deployments.csv")
  assertthat::assert_that(file.exists(deployments_file))
  images_file <- file.path(directory, "images.csv")
  assertthat::assert_that(file.exists(images_file))

  # Read data from files
  wi_projects <- readr::read_csv(
    projects_file,
    show_col_types = FALSE, progress = FALSE
  )
  wi_cameras <- readr::read_csv(
    cameras_file,
    show_col_types = FALSE, progress = FALSE
  )
  wi_deployments <- readr::read_csv(
    deployments_file,
    show_col_types = FALSE, progress = FALSE
  )
  wi_images <- readr::read_csv(
    images_file,
    show_col_types = FALSE, progress = FALSE
  )

  # Create project as list
  assertthat::assert_that(
    nrow(wi_projects) == 1,
    msg = "`projects.csv` must contain exactly one project."
  )
  wi_project <- as.list(wi_projects[1:1, ])
  wi_project$ark_id <- stringr::str_extract(
    wi_project$data_citation, "http[s]?:\\/\\/n2t.net\\/ark:\\/\\d*\\/w\\d*"
  )

  # Create package
  package <- frictionless::create_package() # Also sets profile, resources

  # Set metadata properties, see https://tdwg.github.io/camtrap-dp/metadata
  package$name <- basename(directory) # Unique name if unchanged from WI export zip
  package$id <- wi_project$ark_id # (e.g. http://n2t.net/ark:/63614/w12001317)
  package$created <- lubridate::format_ISO8601(lubridate::now())

  # Set license
  metadata_licenses <- stringr::str_split(wi_project$metadata_license, ", ")[[1]]
  metadata_license <- metadata_licenses[1]
  media_licenses <- stringr::str_split(wi_project$image_license, ", ")[[1]]
  media_license <- media_licenses[1]
  if (length(metadata_licenses) > 1) {
    warning(glue::glue(
      "Multiple metadata licenses found: {licenses_collapse}. ",
      "Metadata license will be set to `{metadata_license}`.",
      licenses_collapse = paste(metadata_licenses, collapse = ", ")
    ))
  }
  if (length(media_licenses) > 1) {
    warning(glue::glue(
      "Multiple media licenses found: {licenses_collapse}. ",
      "Media license will be set to `{media_license}`.",
      licenses_collapse = paste(media_licenses, collapse = ", ")
    ))
  }
  package$licenses <- list(
    list(name = metadata_license, scope = "data"),
    list(name = media_license, scope = "media")
  )

  # Set sources
  package$sources <- c(list(
    title = "Wildlife Insights",
    path = "https://www.wildlifeinsights.org/"
  ))

  # Set contributors
  package$contributors <- list(list(
    title = wi_project$project_admin,
    email = wi_project$project_admin_email
  ))

  # Set organizations
  package$organizations <-
    list(list(title = wi_project$project_admin_organization))

  # Set rightsHolder
  package$rightsHolder <- wi_project$project_admin_organization

  # Set bibliographicCitation
  package$bibliographicCitation <-
    stringr::str_replace_all(wi_project$data_citation, "[\r\n]", " ")

  # Set projects
  package$project <- list(
    id = as.character(wi_project$project_id),
    title = wi_project$project_name,
    acronym = wi_project$project_short_name,
    description = wi_project$project_objectives,
    path = wi_project$ark_id,
    samplingDesign = dplyr::recode(wi_project$project_sensor_layout,
      "Systematic" = "systematic random",
      "Randomized" = "simple random",
      "Convenience" = "opportunistic",
      "Targeted" = "targeted"
      # Unknown
    ),
    captureMethod = dplyr::recode(wi_project$project_sensor_method,
      "Sensor Detection" = "motion detection",
      "Time Lapse" = "time lapse",
      "Both" = "both" # Set to vector later
    ),
    animalTypes = ifelse(
      wi_project$project_sensor_method == "yes",
      TRUE,
      FALSE
    ), # TODO: already implements https://github.com/tdwg/camtrap-dp/issues/232
    classificationLevel = ifelse(
      wi_project$project_type == "Image",
      "media",
      "sequence"
    )
    # sequenceInterval = TODO: how to set for images
    # references = not used in WI
  )
  if (package$project$captureMethod == "both") {
    package$project$captureMethod <- c("motion detection", "time lapse")
  }

  # Set spatial
  package$spatial <- list(
    type = "Feature",
    bbox = list(
      min(wi_deployments$longitude),
      min(wi_deployments$latitude),
      max(wi_deployments$longitude),
      max(wi_deployments$latitude)
    ),
    properties = list(),
    geometry = list(
      type = "Polygon",
      coordinates = list(list(
        list(min(wi_deployments$longitude), min(wi_deployments$latitude)),
        list(max(wi_deployments$longitude), min(wi_deployments$latitude)),
        list(max(wi_deployments$longitude), max(wi_deployments$latitude)),
        list(min(wi_deployments$longitude), max(wi_deployments$latitude)),
        list(min(wi_deployments$longitude), min(wi_deployments$latitude))
      ))
    )
  )
  names(package$spatial$properties) <- character(0) # Set as {} in json

  # Set temporal
  package$temporal <- list(
    start = min(wi_deployments$start_date),
    end = max(wi_deployments$end_date)
  )

  # Set taxonomic
  animal_classes <- c(
    "Mammalia", "Aves", "Reptilia", "Amphibia", "Arachnida", "Gastropoda",
    "Malacostraca", "Clitellata", "Chilopoda", "Diplopoda", "Insecta"
  )
  package$taxonomic <-
    wi_images %>%
    dplyr::distinct(.data$wi_taxon_id, .keep_all = TRUE) %>%
    dplyr::filter(.data$class %in% animal_classes) %>%
    dplyr::transmute(
      taxonID = .data$wi_taxon_id,
      taxonIDReference = "https://github.com/ConservationInternational/Wildlife-Insights----Data-Migration/tree/master/WI_Global_Taxonomy",
      scientificName = dplyr::case_when(
        !is.na(.data$species) & !is.na(.data$genus)
        ~ paste(.data$genus, .data$species),
        !is.na(.data$genus) ~ .data$genus,
        !is.na(.data$family) ~ .data$family,
        !is.na(.data$order) ~ .data$order,
        TRUE ~ class
      ),
      taxonRank = dplyr::case_when(
        !is.na(.data$species) & !is.na(.data$genus) ~ "species",
        !is.na(.data$genus) ~ "genus",
        !is.na(.data$family) ~ "family",
        !is.na(.data$order) ~ "order",
        TRUE ~ "class"
      ),
      kingdom = "Animalia",
      # phylum = not present, likely Chordata
      class = .data$class,
      order = .data$order,
      family = .data$family,
      # subfamily = not present
      genus = .data$genus,
      vernacularNames = .data$common_name
    ) %>%
    purrr::transpose() %>%
    # Change "vernacularNames": "fox" to "vernacularNames": ["en" = "fox"]
    purrr::map(function(x) {
      x$vernacularNames <- list(en = x$vernacularNames)
      x
    })

  # Set platform
  package$platform <- list(
    title = "Wildlife Insights",
    path = "https://www.wildlifeinsights.org/"
    # version = "",
    # packageID = ""
  )

  # Create deployments, see https://tdwg.github.io/camtrap-dp/data/#deployments
  deployments <-
    wi_deployments %>%
    dplyr::left_join(wi_cameras, by = c("project_id", "camera_id")) %>%
    dplyr::transmute(
      deploymentID = .data$deployment_id,
      locationID = .data$placename,
      locationName = .data$placename,
      longitude = .data$longitude,
      latitude = .data$latitude,
      coordinateUncertainty = NA_integer_,
      start = .data$start_date,
      end = .data$end_date,
      setupBy = .data$recorded_by,
      cameraID = as.character(.data$camera_id),
      cameraModel = paste(.data$make, .data$model, sep = "-"),
      cameraInterval = .data$quiet_period,
      cameraHeight = dplyr::recode(.data$sensor_height,
        "Chest height" = 1.5,
        "Knee height" = 0.5,
        "Canopy" = 3.0, # Dubious: range depends on forest
        "Unknown" = NA_real_,
        "Other" = NA_real_,
        .default = NA_real_
      ),
      cameraTilt = dplyr::recode(.data$sensor_orientation,
        "Parallel" = as.integer(0),
        "Pointed Downward" = as.integer(-90),
        "Varies" = NA_integer_,
        "Unknown" = NA_integer_,
        "Other" = NA_integer_,
        .default = NA_integer_
      ),
      cameraHeading = NA_integer_,
      detectionDistance = .data$detection_distance,
      timestampIssues = FALSE,
      baitUse = dplyr::recode(.data$bait_type,
        "None" = "none",
        "Scent" = "scent",
        "Meat" = "meat",
        "Visual" = "visual",
        "Acoustic" = "acoustic",
        "Other" = "other"
      ),
      session = NA_character_,
      array = NA_character_,
      featureType = dplyr::recode(.data$feature_type,
        "None" = "none",
        "Road paved" = "road paved",
        "Road dirt" = "road dirt",
        "Trail hiking" = "trail hiking",
        "Trail game" = "trail game",
        "Road underpass" = "road underpass",
        "Road overpass" = "road overpass",
        "Road bridge" = "road bridge",
        "Culvert" = "culvert",
        "Burrow" = "borrow",
        "Nest site" = "nest site",
        "Carcass" = "carcass",
        "Water source" = "water source",
        "Fruiting tree" = "fruiting tree",
        "Other" = "other"
      ),
      habitat = NA_character_,
      tags = .data$subproject_name, # Set subproject as tag
      comments = paste(c(.data$event_description, .data$camera_functioning), collapse = " | "), # TODO: check with other dataset
      `_id` = NA_character_
    )

  # Create media, see https://tdwg.github.io/camtrap-dp/data/#media
  media <-
    wi_images %>%
    dplyr::distinct(.data$location, .keep_all = TRUE) %>%
    dplyr::transmute(
      mediaID = .data$image_id,
      deploymentID = .data$deployment_id,
      sequenceID = if (wi_project$project_type == "Image") .data$image_id else NA_character_, # TODO: Check ID for sequence type
      captureMethod = NA_character_,
      timestamp = .data$timestamp,
      filePath = .data$location,
      fileName = .data$filename,
      fileMediatype = paste0("image/", tolower(tools::file_ext(.data$location))),
      exifData = NA_character_,
      favourite = .data$highlighted,
      comments = NA_character_,
      `_id` = NA_character_
    )

  # Create observations, see https://tdwg.github.io/camtrap-dp/data/#observations
  observations <-
    wi_images %>%
    dplyr::transmute(
      observationID = paste(.data$image_id, .data$wi_taxon_id, sep = ":"), # TODO: not guaranteed unique
      deploymentID = .data$deployment_id,
      sequenceID = if (wi_project$project_type == "Image") .data$image_id else NA_character_, # TODO: Check ID for sequence type
      mediaID = .data$image_id,
      timestamp = .data$timestamp,
      observationType = dplyr::case_when(
        .data$species == "sapiens" ~ "human",
        # common_name == "common_name" ~ "unknown",
        .data$is_blank == 1 ~ "blank",
        .data$class %in% animal_classes ~ "animal",
        .data$class %in% c("CV Needed", "CV Failed", "No CV Result") ~ "unclassified",
        TRUE ~ "unknown"
      ),
      cameraSetup = NA,
      taxonID = ifelse(
        !.data$class %in% animal_classes,
        NA_character_, # Set taxonID for non-animals to NULL
        .data$wi_taxon_id
      ),
      scientificName = dplyr::case_when(
        !.data$class %in% animal_classes ~ NA_character_,
        !is.na(.data$species) & !is.na(.data$genus)
        ~ paste(.data$genus, .data$species),
        !is.na(.data$genus) ~ .data$genus,
        !is.na(.data$family) ~ .data$family,
        !is.na(.data$order) ~ .data$order,
        TRUE ~ class
      ),
      count = .data$number_of_objects,
      countNew = NA_integer_,
      lifeStage = tolower(.data$age),
      sex = tolower(.data$sex),
      behaviour = NA_character_,
      individualID = NA_character_,
      classificationMethod = ifelse(is.na(.data$cv_confidence), "human", "machine"),
      classifiedBy = .data$identified_by,
      classificationTimestamp = NA,
      classificationConfidence = .data$cv_confidence / 100,
      comments = .data$individual_animal_notes,
      `_id` = NA_character_
    )

  # Add data frames as resources (in separate steps for better error handling)
  # TODO: enable as part of https://github.com/inbo/camtraptor/issues/144
  # package <- frictionless::add_resource(
  #   package,
  #   resource_name = "deployments",
  #   data = deployments,
  #   schema = "https://raw.githubusercontent.com/tdwg/camtrap-dp/0.1.7/deployments-table-schema.json"
  # )
  # package <- frictionless::add_resource(
  #   package,
  #   resource_name = "media",
  #   data = media,
  #   schema = "https://raw.githubusercontent.com/tdwg/camtrap-dp/0.1.7/media-table-schema.json"
  # )
  # package <- frictionless::add_resource(
  #   package,
  #   resource_name = "observations",
  #   data = observations,
  #   schema = "https://raw.githubusercontent.com/tdwg/camtrap-dp/0.1.7/observations-table-schema.json"
  # )

  # Attach data to package
  package$data <- list(
    deployments = deployments,
    media = media,
    observations = observations
  )
  package
}
