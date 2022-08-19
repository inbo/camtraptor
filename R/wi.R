#' Transform Wildlife Insight camera trap data to Darwin Core
#'
#' Transforms a [Wildlife Insight (WI)](https://www.wildlifeinsights.org/)
#' dataset to Darwin Core CSV that can be uploaded to a
#' [GBIF IPT](https://www.gbif.org/ipt) for publication.
#'
#' Wildlife Insight offers to export your dataset [as private download](
#' https://www.wildlifeinsights.org/get-started/download/private) or
#' [public download](
#' https://www.wildlifeinsights.org/get-started/data-download/public).
#' This function read the zip file from the download and convert the Projects,
#' Cameras, Deployements csv file to Darwin Core standard
#'
#' @param import_directory Path to local directory to read the WI files
#' @param export_directory Path to local directory to write files to.
#' @param rights_holder Acronym of the organization owning or managing the
#' rights over the data.
#' @param coordinateUncertaintyInMeters Uncertainty of the coordinate in meters.
#' @return CSV (data) files written to disk.
#' @export
write_dwc_wi <- function(import_directory = ".",
                         export_directory = ".",
                         rights_holder = "project_admin_organization",
                         coordinateUncertaintyInMeters = 30) {
  obs <- read_wi(import_directory)

  # Create details/remarks from mulitple columns.
  obs <- obs %>%
    dplyr::mutate(
      cameraDetails = paste0(
        ifelse(is.na(.data$make), "", paste0("make: ", .data$make)),
        ifelse(is.na(.data$model), "", paste0(" | model: ", .data$model)),
        ifelse(is.na(.data$serial_number), "", paste0(
          " | serial_number: ",
          .data$serial_number
        )),
        ifelse(is.na(.data$year_purchased), "", paste0(" | make: ", .data$year_purchased))
      )
    ) %>%
    dplyr::mutate(
      deploymentRemark = paste0(
        "bait_type: ", .data$bait_type, ifelse(is.na(.data$bait_description), "",
          paste0(" (", .data$bait_description, ")")
        ),
        " | feature_type: ", .data$feature_type,
        ifelse(is.na(.data$feature_type_methodology), "",
          paste0(" (", .data$feature_type_methodology, ")")
        ),
        " | quiet_period: ", .data$quiet_period,
        " | camera_functioning: ", .data$camera_functioning,
        " | sensor_height: ", ifelse(.data$sensor_height == "Other", .data$height_other,
          .data$sensor_height
        ),
        " | sensor_orientation: ", ifelse(.data$sensor_orientation == "Other",
          .data$orientation_other, .data$sensor_orientation
        )
      ),
      " | camera_id: ", .data$camera_id, ifelse(.data$cameraDetails == "", "",
        paste0("(", .data$cameraDetails, ")")
      )
    ) %>%
    dplyr::mutate(
      dataset_id = stringr::str_extract(
        .data$data_citation,
        "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
      )
    )

  # Create the Darwin Core occurrence table
  dwc_occurrence <- obs %>%
    dplyr::transmute(
      # RECORD-LEVEL
      type = "StillImage",
      license = .data$metadata_license,
      rightsHolder = ifelse(rights_holder == "project_admin_organization",
        .data$project_admin_organization, rights_holder
      ),
      # bibliographicCitation = data_citation,
      datasetID = .data$dataset_id,
      # institutionCode = , project_admin_organization: not a code, but can't find where to place it. Metadata is enough?
      collectionCode = "Wildlife Insights",
      datasetName = .data$project_name, # Can this be a different value? should we filter the export to a single project ID?
      basisOfRecord = "MachineObservation",
      # informationWithheld = 'see metadata',
      # OCCURRENCE
      occurrenceID = .data$image_id,
      # recordedBy = recorded_by,
      individualCount = .data$number_of_objects,
      sex = tolower(.data$sex),
      lifeStage = tolower(.data$age),
      # behavior = ,
      occurrenceStatus = "present",
      occurrenceRemarks = .data$individual_animal_notes,
      # ORGANISM
      organismID = .data$individual_id,
      # EVENT
      eventID = .data$image_id,
      parentEventID = .data$deployment_id,
      eventDate = .data$timestamp,
      # habitat = paste0(feature_type, ifelse(is.na(feature_type_methodology), "", paste0(" (", feature_type_methodology, ")"))),
      samplingProtocol = "camera trap",
      samplingEffort = paste0(
        strftime(.data$start_date, "%Y-%m-%dT%H:%M:%S%z"), "/",
        strftime(.data$end_date, "%Y-%m-%dT%H:%M:%S%z")
      ),
      eventRemarks = .data$deploymentRemark,
      # LOCATION
      # locationID = ,
      locality = .data$placename,
      locationRemarks = paste0(
        .data$feature_type,
        ifelse(is.na(.data$feature_type_methodology), "",
          paste0(" (", .data$feature_type_methodology, ")")
        )
      ),
      decimalLatitude = .data$latitude,
      decimalLongitude = .data$longitude,
      geodeticDatum = "WGS84",
      coordinateUncertaintyInMeters = coordinateUncertaintyInMeters,
      # IDENTIFICATION
      identifiedBy = .data$identified_by,
      # dateIdentified =,
      identificationRemarks = ifelse(.data$identified_by == "Computer Vision",
        .data$cv_confidence, ""
      ), # uncertainty
      # TAXON
      taxonID = .data$wi_taxon_id,
      scientificName = paste0(.data$genus, " ", .data$species),
      kingdom = "Animalia",
      class = .data$class,
      order = .data$order,
      family = .data$family,
      genus = .data$genus,
      # taxonRank =
      vernacularName = .data$common_name,
      # taxonRemarks =
    )

  # Create the Darwin Core Audubon (https://ac.tdwg.org/introduction/)
  dwc_audubon <- obs %>%
    dplyr::transmute(
      occurrenceID = .data$image_id,
      mediaID = .data$image_id,
      rights = .data$image_license,
      type = "StillImage",
      captureDevice = .data$cameraDetails,
      accessURI = .data$location,
      format = tools::file_ext(.data$location),
      CreateDate = .data$timestamp
    )

  # Create export directory
  if (!dir.exists(export_directory)) {
    dir.create(export_directory, recursive = TRUE)
  }

  # Write files
  readr::write_csv(
    dwc_occurrence, file.path(export_directory, "dwc_occurrence.csv"),
    na = ""
  )
  readr::write_csv(
    dwc_audubon, file.path(export_directory, "dwc_audubon.csv"),
    na = ""
  )
}



#' Import Wildlife Insight camera trap data to Camera Trap Data Package
#'
#' Transforms a [Wildlife Insight (WI)](https://www.wildlifeinsights.org/)
#' dataset to [Camera Trap Data Package](https://tdwg.github.io/camtrap-dp/)
#' data exchange format.
#'
#' Wildlife Insight offers to export your dataset [as private download](
#' https://www.wildlifeinsights.org/get-started/download/private) or
#' [public download](
#' https://www.wildlifeinsights.org/get-started/data-download/public).
#' This function read the zip file from the download and convert the Projects,
#' Cameras, Deployements csv file to Darwin Core standard
#'
#' @param import_directory Path to local directory to read the WI files
#' @param export_directory Path to local directory to write files to.
#' @param rights_holder Acronym of the organization owning or managing the
#' rights over the data.
#' @param coordinateUncertaintyInMeters Uncertainty of the coordinate in meters.
#' @return CSV (data) files written to disk.
#' @export
write_camtrap_dp_wi <- function(import_directory = ".",
                                export_directory = ".",
                                rights_holder = "project_admin_organization",
                                coordinateUncertaintyInMeters = 30) {
  obs <- read_wi(import_directory)

  obs <- obs %>% mutate(
    samplingDesign = case_when(
      project_sensor_layout == "Systematic" ~ "systematic random",
      project_sensor_layout == "Randomized" ~ "simple random",
      project_sensor_layout == "Convinience" ~ "opportunistic",
      project_sensor_layout == "Targeted" ~ "targeted",
    ),
    observationType = case_when(
      species == "sapiens" ~ "human",
      # common_name == "common_name" ~ "unknown",
      is_blank == 1 ~ "blank",
      class %in% c(
        "Mammalia", "Aves", "Reptilia", "Amphibia", "Arachnida", "Gastropoda",
        "Malacostraca", "Clitellata", "Chilopoda", "Diplopoda", "Insecta"
      ) ~ "animal",
      class %in% c("CV Needed", "CV Failed", "No CV Result") ~ "unclassified",
      TRUE ~ "unknown"
    )
  )

  # tmp = grDevices::chull(cbind(obs$latitude,obs$longitude))
  # spatial = geojsonio::geojson_json(sp::SpatialPoints(cbind(obs$longitude[tmp],obs$latitude[tmp])), driver='GeoJSON')



  metadata <- list(
    # Data Package properties
    name = NULL, # how is that different from project_name?
    id = NULL, # how is that different from project id?
    profile = "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/camtrap-dp-profile.json",
    created = "", # Sys.Date() ? otherwise not provided
    licenses = obs$metadata_license %>% unique(),
    sources = c(list( # Not sure this is correct actually.
      title = "Wildlife Insight",
      path = "https://www.wildlifeinsights.org/"
    )),
    contributors = c(list(
      title = obs$project_admin %>% unique(),
      email = obs$project_admin_email %>% unique()
    )),
    # resources =  c(list()), Not sure how to deal with this
    organizations = c(title = obs$project_admin_organization %>% unique()),
    rightsHolder = ifelse(rights_holder == "project_admin_organization",
      obs$project_admin_organization %>% unique(), rights_holder
    ),
    bibliographicCitation = obs$data_citation %>% unique(),
    project = list(
      id = obs$project_id %>% unique(),
      title = obs$project_name %>% unique(),
      acronym = obs$project_short_name %>% unique(),
      description = obs$project_objectives %>% unique(),
      # path = "", # provided on WI, but not available in the export (or maybe because we don't have provided one, it doesn't export it)
      samplingDesign = obs$samplingDesign %>% unique(), # see above how this this computed
      # captureMethod = "", # Not sure what this is -> "Method(s) used to capture media files."
      # animalTypes = c(), # Not sure what to put here "Type of observed animals. marked indicates specific animal individuals can be recognized."
      classificationLevel = ifelse(obs$project_type %>% unique() == "Image", "media", "sequence")
      # sequence_interval # obs$quiet_periodis deployement specific
      # references
    ),
    # spatial = see above,
    temporal = list(
      start = min(obs$start_date),
      end = max(obs$end_date)
    ),
    taxonomic = c(),
    platform = list(
      title = "Wildlife Insight",
      path = "https://www.wildlifeinsights.org/",
      version = "",
      packageID = obs$project_id %>% unique()
    )
  )

  deployements <- obs %>%
    group_by(deploymentID = deployment_id) %>%
    summarize(
      deploymentID,
      locationName = placename,
      locationID = placename,
      longitude,
      latitude,
      coordinateUncertainty = coordinateUncertaintyInMeters,
      start = start_date,
      end = end_date,
      setupBy = recorded_by,
      cameraID = camera_id,
      cameraModel = model,
      cameraInterval = quiet_period,
      cameraHeight = sensor_height,
      # cameraTilt = NULL,
      cameraHeading = sensor_orientation,
      # detectionDistance = NULL,
      # timestampIssues = FALSE,
      baitUse = tolower(bait_type),
      # session subproject_name?
      # array ?
      featureType = tolower(feature_type),
      # habitat
      # tags
      # comments = event_description?
      # _id
    ) %>%
    unique()

  media <- obs %>%
    group_by(mediaID = image_id) %>%
    summarize(
      mediaID,
      deploymentID = deployment_id,
      # sequenceID
      # captureMethod = "motion detection",
      timestamp = timestamp,
      filePath = location,
      # fileName =
      fileMediatype = "image/jpeg",
      # exifData
      favorite = highlighted,
      # comments =
      # _id
    ) %>%
    unique()

  observations <- obs %>%
    group_by(observationID = NULL) %>%
    summarise(
      observationID,
      deploymentID = deployment_id,
      # sequenceID
      mediaID = image_id,
      timestamp = timestamp,
      observationType = observationType,
      # cameraSetup
      taxonID = wi_taxon_id,
      scientificName = paste0(.data$genus, " ", .data$species),
      count = count_optional,
      # countNew
      individualCount = number_of_objects,
      lifeStage = tolower(age),
      sex = tolower(sex),
      # behaviour
      # individualID # individual_id animal_recognizable
      classificationMethod = ifelse(is.na(uncertainty), "human", "machine"),
      classifiedBy = identified_by,
      # classificationTimestamp
      classificationConfidence = uncertainty, # cv_confidence?
      comments = individual_animal_notes,
      # _id
    ) %>%
    unique()





  # Create export directory
  if (!dir.exists(export_directory)) {
    dir.create(export_directory, recursive = TRUE)
  }

  # Write files
  write(jsonlite::toJSON(metadata), file.path(export_directory, "datapackage.csv"))
  readr::write_csv(
    deployments, file.path(export_directory, "deployments.csv"),
    na = ""
  )
  readr::write_csv(
    media, file.path(export_directory, "media.csv"),
    na = ""
  )
  readr::write_csv(
    observations, file.path(export_directory, "observations.csv"),
    na = ""
  )
}

















#' Read WI data
#'
#' @param import_directory Path to local directory to read the WI files
#' @return obs
#' @export
read_wi <- function(import_directory = ".") {
  if (!file.exists(import_directory)) {
    stop(paste0("The import directory does not exist: ", import_directory))
  }

  # If zip file, unzip
  if (tools::file_ext(import_directory) == "zip") {
    utils::unzip(import_directory, exdir = dirname(import_directory))
    import_directory <- tools::file_path_sans_ext(import_directory)
  }

  # Get file location and check existance
  deployements_file <- file.path(import_directory, "deployments.csv")
  if (!file.exists(deployements_file)) {
    stop(paste0("The deployements file does not exist: ", deployements_file))
  }
  images_file <- file.path(import_directory, "images.csv")
  if (!file.exists(images_file)) {
    stop(paste0("The images file does not exist: ", images_file))
  }
  cameras_file <- file.path(import_directory, "cameras.csv")
  if (!file.exists(cameras_file)) {
    stop(paste0("The cameras file does not exist: ", cameras_file))
  }
  projects_file <- file.path(import_directory, "projects.csv")
  if (!file.exists(cameras_file)) {
    stop(paste0("The project file does not exist: ", projects_file))
  }

  # Read data from file
  deployments <- readr::read_csv(deployements_file, show_col_types = FALSE)
  images <- readr::read_csv(images_file, show_col_types = FALSE)
  cameras <- readr::read_csv(cameras_file, show_col_types = FALSE)
  projects <- readr::read_csv(projects_file, show_col_types = FALSE)


  # Join all data in a single data.frame
  obs <- images %>%
    dplyr::left_join(deployments, by = c("project_id", "deployment_id")) %>%
    dplyr::left_join(cameras, by = c("project_id", "camera_id")) %>%
    dplyr::left_join(projects, by = "project_id")

  # Check that there is only a single project
  stopifnot(length(unique(obs$project_name)) == 1)

  return(obs)
}
