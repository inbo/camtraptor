#' Read a Wildlife Insights export
#'
#' Reads files from an unzipped [Wildlife Insights (WI)](
#' https://www.wildlifeinsights.org/) export into memory.
#' Data can be exported from Wildlife Insights as a [public](
#' https://www.wildlifeinsights.org/get-started/data-download/public) or
#' [private](https://www.wildlifeinsights.org/get-started/download/private)
#' download.
#' The function transforms data and metadata to a [Camera Trap Data Package](
#' https://tdwg.github.io/camtrap-dp) which can be written to file with
#' [frictionless::write_package()].
#'
#' @param directory Path to local directory to read files from.
#'   The function expects `projects.csv`, `deployments.csv`, `cameras.csv`, and
#'   `images.csv`.
#' @param capture_method How media files were obtained.
#'   Character (vector) with `motion detection` and/or `time lapse`.
#' @return CSV (data) files written to disk.
#' @export
#' @importFrom dplyr %>%
#' @family read functions
read_wi <- function(directory = ".", capture_method = "motion detection") {
  # Check files
  projects_file <- file.path(directory, "projects.csv")
  assertthat::assert_that(file.exists(projects_file))
  cameras_file <- file.path(directory, "cameras.csv")
  assertthat::assert_that(file.exists(cameras_file))
  deployments_file <- file.path(directory, "deployments.csv")
  assertthat::assert_that(file.exists(deployments_file))
  images_file <- file.path(directory, "images.csv")
  assertthat::assert_that(file.exists(images_file))

  # Check capture method
  capture_methods = c("motion detection", "time lapse")
  assertthat::assert_that(
    all(capture_method %in% capture_methods),
    msg = glue::glue(
      "`capture_method` must be `{capture_method_collapse}`",
      capture_method_collapse = paste(capture_methods, collapse = "` and/or `")
    )
  )

  # Read data from files
  wi_projects <- readr::read_csv(
    projects_file, show_col_types = F, progress = FALSE
  )
  wi_cameras <- readr::read_csv(
    cameras_file, show_col_types = FALSE, progress = FALSE
  )
  wi_deployments <- readr::read_csv(
    deployments_file, show_col_types = FALSE, progress = FALSE
  )
  wi_images <- readr::read_csv(
    images_file, show_col_types = FALSE, progress = FALSE
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
  package$name <- basename(directory) # Unique name if unchanged from the WI export zip
  package$id <- wi_project$ark_id # (e.g. http://n2t.net/ark:/63614/w12001317)
  package$created <- lubridate::format_ISO8601(lubridate::now())

  # Set licenses
  media_licenses <-
    wi_images %>%
    dplyr::group_by(license) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::first()
  if (length(media_licenses) > 1) {
    warning(
      glue::glue(
        "`images.csv` contains multiple licenses: `{media_licenses_collapse}`. ",
        "Camtrap DP only supports one media license: `{media_licenses[1]}` ",
        "(the most occurring license) will be assigned to all media.",
        media_licenses_collapse = paste(media_licenses, collapse = "`, `")
      )
    )
  }
  package$licenses <- list(
    list(
      name = wi_project$metadata_license,
      scope = "data"
    ),
    list(
      name = media_licenses[1],
      scope = "media"
    )
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
    samplingDesign = dplyr::case_when(
      wi_project$project_sensor_layout == "Systematic" ~ "systematic random",
      wi_project$project_sensor_layout == "Randomized" ~ "simple random",
      wi_project$project_sensor_layout == "Convenience" ~ "opportunistic",
      wi_project$project_sensor_layout == "Targeted" ~ "targeted"
    ),
    captureMethod = capture_method,
    animalTypes = if (all(is.na(wi_images$markings))) {
      "unmarked"
    } else if (!any(is.na(wi_images$markings))) {
      "marked"
    } else {
      c("marked", "unmarked")
    },
    classificationLevel = ifelse(
      wi_project$project_type == "Image", # TODO: Check how WI exports sequence data
      "media",
      "sequence"
    )
    # sequenceInterval = deployment specific, see https://github.com/tdwg/camtrap-dp/issues/203
    # references = not used in WI
  )

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
  package$taxonomic <-
    wi_images %>%
    dplyr::transmute(
      taxonID = wi_taxon_id,
      taxonIDReference = "https://github.com/ConservationInternational/Wildlife-Insights----Data-Migration/tree/master/WI_Global_Taxonomy",
      scientificName = dplyr::case_when(
        !is.na(species) & !is.na(genus) ~ paste(genus, species),
        !is.na(genus) ~ genus,
        !is.na(family) ~ family,
        !is.na(order) ~ order,
        TRUE ~ class
      ),
      taxonRank = dplyr::case_when(
        !is.na(species) & !is.na(genus) ~ "species",
        !is.na(genus) ~ "genus",
        !is.na(family) ~ "family",
        !is.na(order) ~ "order",
        TRUE ~ "class"
      ),
      kingdom = "Animalia",
      # phylum = not present, likely Chordata
      class = class,
      order = order,
      family = family,
      # subfamily = not present
      genus = genus
    ) %>%
    unique() %>%
    purrr::transpose()

  # Set platform
  package$platform <- list(
    title = "Wildlife Insight",
    path = "https://www.wildlifeinsights.org/"
    # version = "",
    # packageID = ""
  )

  # Create deployments, see https://tdwg.github.io/camtrap-dp/data/#deployments
  deployments <-
    wi_deployments %>%
    dplyr::left_join(wi_cameras, by = c("project_id", "camera_id")) %>%
    dplyr::transmute(
      deploymentID = deployment_id,
      locationID = placename,
      locationName = placename,
      longitude = longitude,
      latitude = latitude,
      coordinateUncertainty = NA_integer_,
      start = start_date,
      end = end_date,
      setupBy = recorded_by,
      cameraID = as.character(camera_id),
      cameraModel = model,
      cameraInterval = quiet_period,
      cameraHeight = dplyr::case_when(
        .data$sensor_height == "Chest height" ~ 150,
        .data$sensor_height == "Knee height" ~ 50,
        .data$sensor_height == "Canopy" ~ 3, # huge range depending on your forest...
        .data$sensor_height == "Unknown" ~ NA_real_,
        .data$sensor_height == "Other" ~ NA_real_, # .data$height_other is a description
      ),
      cameraTilt = NA_integer_,
      cameraHeading = sensor_orientation,
      detectionDistance = NA_real_,
      timestampIssues = FALSE,
      baitUse = tolower(bait_type),
      session = NA_character_,
      array = NA_character_,
      featureType = tolower(feature_type),
      habitat = NA_character_,
      tags = subproject_name, # Set subproject as tag
      comments = event_description, # TODO: check with other dataset
      `_id` = NA_character_
    )

  # Create media, see https://tdwg.github.io/camtrap-dp/data/#media
  media <-
    wi_images %>%
    dplyr::transmute(
      mediaID = image_id,
      deploymentID = deployment_id,
      sequenceID = NA_character_,
      captureMethod = ifelse(
        length(capture_method) == 1,
        capture_method,
        NA_character_
      ),
      timestamp = timestamp,
      filePath = location,
      fileName = filename,
      fileMediatype = paste0("image/", tolower(tools::file_ext(location))),
      exifData = NA_character_,
      favourite = highlighted,
      comments = NA_character_,
      `_id` = NA_character_
    ) %>%
    unique() # Remove the images with multiple observations

  # Create observations, see https://tdwg.github.io/camtrap-dp/data/#observations
  observations <-
    wi_images %>%
    dplyr::transmute(
      observationID = paste(image_id, wi_taxon_id, sep = ":"), # TODO: not guaranteed unique
      deploymentID = deployment_id,
      sequenceID = NA_character_,
      mediaID = image_id,
      timestamp = timestamp,
      observationType = dplyr::case_when(
        species == "sapiens" ~ "human",
        # common_name == "common_name" ~ "unknown",
        is_blank == 1 ~ "blank",
        class %in% c(
          "Mammalia", "Aves", "Reptilia", "Amphibia", "Arachnida", "Gastropoda",
          "Malacostraca", "Clitellata", "Chilopoda", "Diplopoda", "Insecta"
        ) ~ "animal",
        class %in% c("CV Needed", "CV Failed", "No CV Result") ~ "unclassified",
        TRUE ~ "unknown"
      ),
      cameraSetup = NA,
      taxonID = wi_taxon_id,
      scientificName = paste(genus, species),
      count = number_of_objects,
      countNew = NA_integer_,
      lifeStage = tolower(age),
      sex = tolower(sex),
      behaviour = NA_character_,
      individualID = NA_character_, # TODO: individual_id or animal_recognizable
      classificationMethod = ifelse(is.na(cv_confidence), "human", "machine"),
      classifiedBy = identified_by,
      classificationTimestamp = NA,
      classificationConfidence = cv_confidence, # TODO: or uncertainty? not sure of the difference
      comments = individual_animal_notes,
      `_id` = NA_character_
      # Not use: license and markings.
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

  # Check
  package$data <- list(
    deployments = deployments,
    media = media,
    observations = observations
  )

  return(package)
}
