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
#' The function allows you to choose which animal class to keep in the data. See [https://www.wildlifeinsights.org/get-started/taxonomy]
#' for more information on the class available. The main ones are: `"Mammalia", "Aves", "Reptilia",
#' "Amphibia", "Arachnida","Gastropoda","Malacostraca","Clitellata","Chilopoda","Diplopoda",
#' "Insecta", "Domestic animal"`. Note that Domestic animal is only for the class level domestic animal (ie, does not include domestic cattle, goat etc...)
#'
#' @param import_directory
#' @param export_directory Path to local directory to write files to.
#' @return CSV (data) files written to disk.
#'
wi_to_dwc <- function(import_directory = ".",
                      export_directory = ".",
                      projects = "",
                      class_keep = c("Mammalia", "Aves", "Reptilia", "Amphibia")) {
  
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

  # Filter
  # See https://www.wildlifeinsights.org/get-started/taxonomy
  # https://wildlifeinsights-taxonomy-ewutmovdfa-uc.a.run.app/
  # wi_taxa <- jsonlite::fromJSON("https://api.wildlifeinsights.org/api/v1/taxonomy?fields=class,order,family,genus,species,authority,taxonomyType,uniqueIdentifier,commonNameEnglish&page[size]=30000")
  # unique(wi_taxa$data$class)
  obs <- obs %>%
    filter(class %in% class_keep) %>% # This also remove CV Needed, CV Failed, No CV Result, NA, Other and ""
    filter(species != "sapiens") %>% # Remove any humans
    filter(common_name != "Unknown species") # Remove unknown species

  # deploymentRemark
  obs <- obs %>%
    mutate(
      cameraDetails = paste0(
        ifelse(is.na(make), "", paste0(" | make: ", make)),
        ifelse(is.na(model), "", paste0(" | model: ", model)),
        ifelse(is.na(serial_number), "", paste0(" | serial_number: ", serial_number)),
        ifelse(is.na(year_purchased), "", paste0(" | make: ", year_purchased))
      ) %>%
        mutate(
          deploymentRemark = paste0(
            "bait_type: ", bait_type, ifelse(is.na(bait_description), "", paste0(" (", bait_description, ")")),
            "bait_type: ", feature_type, ifelse(is.na(feature_type_methodology), "", paste0(" (", feature_type_methodology, ")")),
            " | quiet_period: ", quiet_period,
            " | camera_functioning: ", camera_functioning,
            " | sensor_height: ", ifelse(sensor_height == "Other", height_other, sensor_height),
            " | sensor_orientation: ", ifelse(sensor_orientation == "Other", orientation_other, sensor_orientation)
          ),
          " | camera_id: ", camera_id, ifelse(cameraDetails == "", "", pasteo("(", cameraDetails, ")"))
        )
    )

  dwc_occurrence <- obs %>%
    dplyr::transmute(
      # RECORD-LEVEL
      type = "Event", # Why not StillImage?
      license = license, # take the licence from images.csv (note that there is also a licens for metatdata and the images themselves)
      # rightsHolder = '',
      bibliographicCitation:data_citation,
      # datasetID = , # Note sure this is appropriate:
      # institutionCode = , project_admin_organization: not a code, but cann't find where to place it. Metadata is enough?
      # collectionCode = , initiative_id (https://www.wildlifeinsights.org/get-started/glossary) or project_id
      datasetName = project_name, # Can this be a different value? should we filter the export to a single project ID?
      basisOfRecord = "MachineObservation",
      # informationWithheld = 'see metadata', Is this not assumed if not provided?
      # OCCURRENCE
      occurrenceID = image_id,
      recordedBy = recorded_by,
      individualCount = number_of_objects, # count_optional,
      sex = sex,
      lifeStage = age,
      # behavior = "",
      occurrenceStatus = "present",
      # occurrenceRemarks = , # could include any of the following field: filename markings, highlighted, individual_id, individual_animal_notes
      # ORGANISM
      # organismID = , I don't understand what is the difference with taxon
      # EVENT
      eventID = deployment_id,
      parentEventID = project_id,
      eventDate = timestamp, # Is thi correct? time at which the photo was taken
      habitat = paste0(feature_type, ifelse(is.na(feature_type_methodology), "", paste0(" (", feature_type_methodology, ")"))),
      samplingProtocol = paste0("camera trap", bait), # project_sensor_layout project_sensor_layout_targeted_type project_stratification project_stratification_type project_sensor_method project_individual_animals
      samplingEffort = lubridate::interval(start = start_date, end = end_date),
      eventRemarks = deploymentRemark,
      # LOCATION
      locationID = placename,
      # locality = placename,
      locationRemarks = "", # feature_type, feature_type_description or see eventRemarks
      decimalLatitude = latitude,
      decimalLongitude = longitude,
      geodeticDatum = "WGS84",
      # coordinateUncertaintyInMeters = dep.coordinateUncertainty,
      # IDENTIFICATION
      identifiedBy = identified_by,
      # dateIdentified =,
      # identificationRemarks = "" # uncertainty
      # TAXON
      taxonID = wi_taxon_id,
      scientificName = obs.scientificName,
      kingdom = "Animalia",
      class = class,
      order = order,
      family = family,
      genus = genus,
      genericName = species,
      # taxonRank = ?
      vernacularName = common_name,
      nameAccordingTo = "Wildlife Insight Taxonomy", # ?
      # taxonRemaks = uncertainty
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
    events, file.path(export_directory, "events.csv"),
    na = ""
  )
}
