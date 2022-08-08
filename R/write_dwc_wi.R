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
  
  # Filter species and class to keep only non-human wildlife entries.
  # See https://www.wildlifeinsights.org/get-started/taxonomy
  # https://wildlifeinsights-taxonomy-ewutmovdfa-uc.a.run.app/
  # wi_taxa <- jsonlite::fromJSON("https://api.wildlifeinsights.org/api/v1/taxonomy?fields=class,order,family,genus,species,authority,taxonomyType,uniqueIdentifier,commonNameEnglish&page[size]=30000")
  # unique(wi_taxa$data$class)
  obs <- obs %>%
    dplyr::filter(.data$species != "sapiens") %>% # Remove any humans
    dplyr::filter(.data$common_name != "Unknown species") %>%  # Remove unknown species
    dplyr::filter(.data$class %in% c(
      "Mammalia", "Aves", "Reptilia", "Amphibia", "Arachnida", "Gastropoda",
      "Malacostraca", "Clitellata", "Chilopoda", "Diplopoda", "Insecta"
    ))# This also remove CV Needed, CV Failed, No CV Result, NA, Other and ""
    
  
  # Create details/remarks from mulitple columns.  
  obs <- obs %>%
    dplyr::mutate(
      cameraDetails = paste0(
        ifelse(is.na(.data$make), "", paste0("make: ", .data$make)),
        ifelse(is.na(.data$model), "", paste0(" | model: ", .data$model)),
        ifelse(is.na(.data$serial_number), "", paste0(" | serial_number: ", 
                                                .data$serial_number)),
        ifelse(is.na(.data$year_purchased), "", paste0(" | make: ", .data$year_purchased))
      )
    ) %>%
    dplyr::mutate(
      deploymentRemark = paste0(
        "bait_type: ", .data$bait_type, ifelse(is.na(.data$bait_description), "", 
                                         paste0(" (", .data$bait_description, ")")),
        " | feature_type: ", .data$feature_type, 
        ifelse(is.na(.data$feature_type_methodology), "", 
               paste0(" (", .data$feature_type_methodology, ")")),
        " | quiet_period: ", .data$quiet_period,
        " | camera_functioning: ", .data$camera_functioning,
        " | sensor_height: ", ifelse(.data$sensor_height == "Other", .data$height_other, 
                                     .data$sensor_height),
        " | sensor_orientation: ", ifelse(.data$sensor_orientation == "Other", 
                                          .data$orientation_other, .data$sensor_orientation)
      ),
      " | camera_id: ", .data$camera_id, ifelse(.data$cameraDetails == "", "", 
                                          paste0("(", .data$cameraDetails, ")"))
    ) %>%
    dplyr::mutate(
      dataset_id = stringr::str_extract(
        .data$data_citation, 
        "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
    ) %>% 
    dplyr::mutate(
      access_uri = stringr::str_replace(stringr::str_replace(.data$location, "gs://", "https://storage.googleapis.com/"),"__main","__thumbnails")
    )
  
  # Create the Darwin Core occurrence table
  dwc_occurrence <- obs %>%
    dplyr::transmute(
      # RECORD-LEVEL
      type = "StillImage",
      license = .data$metadata_license,
      rightsHolder = ifelse(rights_holder == "project_admin_organization", 
                            .data$project_admin_organization, rights_holder),
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
      samplingEffort = paste0(strftime(.data$start_date, "%Y-%m-%dT%H:%M:%S%z"), "/", 
                              strftime(.data$end_date, "%Y-%m-%dT%H:%M:%S%z")),
      eventRemarks = .data$deploymentRemark,
      # LOCATION
      # locationID = ,
      locality = .data$placename,
      locationRemarks = paste0(.data$feature_type, 
                               ifelse(is.na(.data$feature_type_methodology), "", 
                                      paste0(" (", .data$feature_type_methodology, ")"
                                             ))),
      decimalLatitude = .data$latitude,
      decimalLongitude = .data$longitude,
      geodeticDatum = "WGS84",
      coordinateUncertaintyInMeters = coordinateUncertaintyInMeters,
      # IDENTIFICATION
      identifiedBy = .data$identified_by,
      # dateIdentified =,
      identificationRemarks = ifelse(.data$identified_by == "Computer Vision", 
                                     .data$cv_confidence, ""), # uncertainty
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
      accessURI = .data$access_uri,
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
