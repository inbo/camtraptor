#' Transform camera trap data to Darwin Core
#'
#' Transforms a Wildlife Insight dataset to Darwin Core CSV that can be uploaded to a
#' [GBIF IPT](https://www.gbif.org/ipt) for publication.
#'
#' Wildlife Insight offers to export your dataset [as private download](https://www.wildlifeinsights.org/get-started/download/private).
#' or [public download](https://www.wildlifeinsights.org/get-started/data-download/public).
#' This function read the zip file from the download and convert the Projects, Cameras, Deployements
#' csv file to Darwin Core standard
#'
#' @param import_directory Path to local directory to read the WI files
#' @param export_directory Path to local directory to write files to.
#' @return CSV (data) files written to disk.
#'
write_dwc_wi <- function(import_directory = ".",
                      export_directory = ".") {
  if (!file.exists(import_directory)) {
    stop(paste0("The import directory does not exist: ", import_directory))
  }

  # If zip file, unzip
  if (tools::file_ext(import_directory) == "zip") {
    utils::unzip(import_directory, exdir = dirname(import_directory))
    import_directory <- tools::file_path_sans_ext(import_directory)
  }

  # Read data from export
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

  # Read files
  deployments <- readr::read_csv(deployements_file, show_col_types = FALSE)
  images <- readr::read_csv(images_file, show_col_types = FALSE)
  cameras <- readr::read_csv(cameras_file, show_col_types = FALSE)
  projects <- readr::read_csv(projects_file, show_col_types = FALSE)


  # Join in a single database
  obs <- images %>%
    left_join(deployments) %>%
    left_join(cameras) %>%
    left_join(projects)

  stopifnot(length(unique(obs$project_name)) == 1)

  # Filter
  # See https://www.wildlifeinsights.org/get-started/taxonomy
  # https://wildlifeinsights-taxonomy-ewutmovdfa-uc.a.run.app/
  # wi_taxa <- jsonlite::fromJSON("https://api.wildlifeinsights.org/api/v1/taxonomy?fields=class,order,family,genus,species,authority,taxonomyType,uniqueIdentifier,commonNameEnglish&page[size]=30000")
  # unique(wi_taxa$data$class)
  obs <- obs %>%
    filter(class %in% c(
      "Mammalia", "Aves", "Reptilia", "Amphibia", "Arachnida", "Gastropoda",
      "Malacostraca", "Clitellata", "Chilopoda", "Diplopoda", "Insecta"
    )) %>% # This also remove CV Needed, CV Failed, No CV Result, NA, Other and ""
    filter(species != "sapiens") %>% # Remove any humans
    filter(common_name != "Unknown species") # Remove unknown species

  obs <- obs %>%
    mutate(
      cameraDetails = paste0(
        ifelse(is.na(make), "", paste0("make: ", make)),
        ifelse(is.na(model), "", paste0(" | model: ", model)),
        ifelse(is.na(serial_number), "", paste0(" | serial_number: ", serial_number)),
        ifelse(is.na(year_purchased), "", paste0(" | make: ", year_purchased))
      )
    ) %>%
    mutate(
      deploymentRemark = paste0(
        "bait_type: ", bait_type, ifelse(is.na(bait_description), "", paste0(" (", bait_description, ")")),
        " | feature_type: ", feature_type, ifelse(is.na(feature_type_methodology), "", paste0(" (", feature_type_methodology, ")")),
        " | quiet_period: ", quiet_period,
        " | camera_functioning: ", camera_functioning,
        " | sensor_height: ", ifelse(sensor_height == "Other", height_other, sensor_height),
        " | sensor_orientation: ", ifelse(sensor_orientation == "Other", orientation_other, sensor_orientation)
      ),
      " | camera_id: ", camera_id, ifelse(cameraDetails == "", "", paste0("(", cameraDetails, ")"))
    )


  dwc_occurrence <- obs %>%
    dplyr::transmute(
      # RECORD-LEVEL
      type = "StillImage",
      license = metadata_license,
      rightsHolder = project_admin_organization,
      # bibliographicCitation = data_citation,
      datasetID = 'project_id',
      # institutionCode = , project_admin_organization: not a code, but can't find where to place it. Metadata is enough?
      collectionCode = 'Wildlife Insights',
      datasetName = project_name, # Can this be a different value? should we filter the export to a single project ID?
      basisOfRecord = "MachineObservation",
      # informationWithheld = 'see metadata',
      # OCCURRENCE
      occurrenceID = image_id,
      # recordedBy = recorded_by,
      individualCount = number_of_objects,
      sex = tolower(sex),
      lifeStage = tolower(age),
      # behavior = ,
      occurrenceStatus = "present",
      occurrenceRemarks = individual_animal_notes,
      # ORGANISM
      organismID = individual_id,
      # EVENT
      eventID = image_id,
      parentEventID = deployment_id,
      eventDate = timestamp,
      # habitat = paste0(feature_type, ifelse(is.na(feature_type_methodology), "", paste0(" (", feature_type_methodology, ")"))),
      samplingProtocol = "camera trap", # project_sensor_layout project_sensor_layout_targeted_type project_stratification project_stratification_type project_sensor_method project_individual_animals
      samplingEffort = paste0(strftime(start_date , "%Y-%m-%dT%H:%M:%S%z"),"/",strftime(end_date , "%Y-%m-%dT%H:%M:%S%z")),
      eventRemarks = deploymentRemark,
      # LOCATION
      # locationID = ,
      locality = placename,
      locationRemarks= paste0(feature_type, ifelse(is.na(feature_type_methodology), "", paste0(" (", feature_type_methodology, ")"))),
      decimalLatitude = latitude,
      decimalLongitude = longitude,
      geodeticDatum = "WGS84",
      # coordinateUncertaintyInMeters = ,
      # IDENTIFICATION
      identifiedBy = identified_by,
      # dateIdentified =,
      identificationRemarks = ifelse(identified_by == "Computer Vision", cv_confidence, ""), # uncertainty
      # TAXON
      taxonID = wi_taxon_id,
      scientificName = paste0(genus, " ", species),
      kingdom = "Animalia",
      class = class,
      order = order,
      family = family,
      genus = genus,
      # taxonRank =
      vernacularName = common_name,
      # taxonRemarks =
    )

  dwc_audubon <- obs %>%
    dplyr::transmute(
      occurrenceID = image_id,
      # identifier = filename
      rights = image_license,
      type = "StillImage",
      captureDevice = cameraDetails,
      # resourceCreationTechnique = ,
      accessURI = location, # e.g. "gs://my_first_project_1587661402925__main/deployment/2032997/4ce8fc0f-dc20-49e1-8b93-583895048f94.JPG"
      format = tools::file_ext(obs$location),
      # CreateDate =
    )

  # Write files
  if (!dir.exists(export_directory)) {
    dir.create(export_directory, recursive = TRUE)
  }

  readr::write_csv(
    dwc_occurrence, file.path(export_directory, "dwc_occurrence.csv"),
    na = ""
  )
  readr::write_csv(
    dwc_audubon, file.path(export_directory, "dwc_audubon.csv"),
    na = ""
  )
}
