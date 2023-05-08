#' Transform Camtrap DP data to Darwin Core
#'
#' Transforms data from a [Camera Trap Data Package](
#' https://tdwg.github.io/camtrap-dp/) to [Darwin Core](https://dwc.tdwg.org/).
#' The resulting CSV file(s) can be uploaded to an [IPT](
#' https://www.gbif.org/ipt) for publication to GBIF.
#' A `meta.xml` file is not created.
#' See `write_eml()` to create an `eml.xml` file.
#'
#' @param package A Camtrap DP, as read by [read_camtrap_dp()].
#' @param directory Path to local directory to write file(s) to.
#'   If `NULL`, then a list of data frames is returned instead, which can be
#'   useful for extending/adapting the Darwin Core mapping before writing with
#'   [readr::write_csv()].
#' @return CSV file(s) written to disk or list of data frames when
#'   `directory = NULL`.
#' @family publication functions
#' @export
#' @section Transformation details:
#' Data are transformed into an
#' [Occurrence core](https://rs.gbif.org/core/dwc_occurrence_2022-02-02.xml) and
#' [Audubon Media Description extension](https://rs.gbif.org/extension/ac/audubon_2020_10_06.xml).
#' This **follows recommendations** discussed and created by Peter Desmet,
#' John Wieczorek, Lien Reyserhove, Ben Norton and others.
#' See the [SQL file(s)](https://github.com/inbo/camtraptor/tree/main/inst/sql)
#' used by this function for details.
#'
#' The following terms are set from the `package` metadata:
#' - **datasetName**: Title as provided in `package$title`.
#' - **datasetID**: Identifier as provided in `package$id`.
#'   Can be a DOI.
#' - **rightsHolder**: Rights holder as provided in `package$rightsHolder`.
#' - **collectionCode**: Platform name as provided in `package$platform$title`.
#' - **license**: License with scope `data` as provided in `package$licenses`.
#' - **rights** for media files: License with scope `media` as provided in
#'   `package$licenses`.
#' - **dwc:dataGeneralizations**: "coordinates rounded to
#'   `package$coordinatePrecision` degrees".
#' - **coordinatePrecision**: `package$coordinatePrecision` (e.g. `0.001`).
#'
#' Key features of the Darwin Core transformation:
#' - Deployments (of camera traps) are parent events, with observations
#'   (machine observations) as child events. No information about the parent
#'   event is provided other than its ID, meaning that data can be expressed in
#'   an Occurrence Core with one row per observation and `parentEventID` shared
#'   by all occurrences in a deployment.
#' - Sequence-based observations share an `eventID` per sequence, image-based
#'   observations share an `eventID` per image.
#' - The image(s) an observation is based on are provided in the [Audubon Media
#'   Description extension](
#'   https://rs.gbif.org/extension/ac/audubon_2020_10_06.xml), with a foreign
#'   key to the observation.
#' - Excluded are records that document blank or unclassified media, vehicles
#'   and observations of humans.
write_dwc <- function(package, directory = ".") {
  # Set properties from metadata
  ## use purrr::pluck() to force NA when metadata field is missing
  dataset_name <- purrr::pluck(package,"title", .default = NA)
  dataset_id <- purrr::pluck(package,"id", .default = NA)
  rights_holder <- purrr::pluck(package,"rightsHolder", .default = NA)
  collection_code <- purrr::pluck(package,"platform","title", .default = NA)
  license <- dplyr::coalesce(
    purrr::keep(package$licenses, ~ .$scope == "data")[[1]]$path,
    NA)
  media_license <- dplyr::coalesce(
    purrr::keep(package$licenses, ~ .$scope == "media")[[1]]$path,
    NA)
  coordinate_precision <-
    purrr::pluck(package, "coordinatePrecision", .default = NA)

  # read package data
  deployments <- dplyr::tibble(package$data$deployments)
  media <- dplyr::tibble(package$data$media)
  observations <- dplyr::tibble(package$data$observations)
  # Create database
  message("Reading data and transforming to Darwin Core.")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "deployments", dplyr::tibble(package$data$deployments))
  DBI::dbWriteTable(con, "media", dplyr::tibble(package$data$media))
  DBI::dbWriteTable(con, "observations", dplyr::tibble(package$data$observations))

  # create dwc_occurrence by joining observations on deployments
  ## NOTE we can get rid of a number of fields here already, see dwc_occurrence.sql
  dwc_occurrence <-
    dplyr::filter(observations, observationType == "animal") %>%
    dplyr::left_join(
      deployments,
      by = dplyr::join_by(deploymentID),
      suffix = c(".obs", ".depl")
    ) %>%
    dplyr::mutate(
      .keep = "none",
      type = "Image",
      license = license,
      #missing test coverage
      rightsHolder = rights_holder,
      datasetID = dataset_id,
      collectionCode = collection_code,
      datasetName = dataset_name,
      basisOfRecord = "MachineObservation",
      dataGeneralizations = glue::glue(
        "coordinates rounded",
        " to {coordinate_precision}",
        " degrees",
        .na = NULL
      ),
      occurrenceID = observationID,
      individualCount = count,
      sex,
      lifeStage,
      behavior = behaviour,
      occurrenceStatus = "present",
      occurrenceRemarks = comments.obs,
      organismID = individualID,
      eventID = sequenceID,
      parentEventID = deploymentID,
      eventDate = format(timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
      habitat = habitat,
      samplingProtocol = "camera trap",
      samplingEffort = glue::glue(
        "{start}/{end}",
        start = format(start, format = "%Y-%m-%dT%H:%M:%SZ"),
        end = format(end, format = "%Y-%m-%dT%H:%M:%SZ")
      ),
      # using stringr::sr_squish() to get rid of an extra space after "camera
      # trap" that I can't find the source for
      eventRemarks = stringr::str_squish(glue::glue(
        "{bait_use} {dep_feature_type} {depl_comments}",
        bait_use = dplyr::case_when(
          baitUse == "none" ~ "camera trap without bait",
          !is.na(baitUse) ~ glue::glue("camera trap with {baitUse} bait"),
          TRUE ~ "camera trap"
        ),
        dep_feature_type = dplyr::case_when(
          featureType == "none" ~ "",
          featureType == "other" ~ " near other feature",
          !is.na(featureType) ~ sprintf(" near %s", featureType),
          TRUE ~ ""
        ),
        depl_comments = dplyr::coalesce(
          glue::glue(
            "| tags: {tags} | {comments}",
            tags = tags,
            comments = comments.depl,
            .na = NULL
          ),
          glue::glue("| tags: {tags}",
                     tags = tags,
                     .na = NULL),
          glue::glue("| {comments}",
                     comments = comments.depl,
                     .na = NULL),
          ""
        )
      )),
      locationID,
      locality = locationName,
      decimalLatitude = latitude,
      decimalLongitude = longitude,
      geodeticDatum = "EPSG:4326",
      coordinateUncertaintyInMeters = coordinateUncertainty,
      coordinatePrecision = coordinate_precision,
      identifiedBy = classifiedBy,
      dateIdentified = format(classificationTimestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
      identificationRemarks = dplyr::coalesce(
        glue::glue(
          "classified by {classificationMethod} with",
          " {classificationConfidence} confidence",
          .na = NULL
        ),
        glue::glue("classified by {classificationMethod}",
                   .na = NULL)
      ),
      taxonID,
      scientificName,
      kingdom = "Animalia"
    ) %>%
    #fix/clean up the order after generating, columns that are kept in place are placed
    #at the start of the output df by default
    dplyr::relocate(sex, lifeStage, .after = "individualCount") %>%
    dplyr::relocate(habitat, .after = "eventDate") %>%
    dplyr::relocate(taxonID, scientificName, .after = "identificationRemarks") %>%
    dplyr::relocate(locationID, .before = "locality") %>%
    dplyr::arrange(eventDate)
  
  # create dwc_audubon
  
  observations_animals <- observations %>% 
    dplyr::filter(observationType == 'animal') %>% 
    dplyr::select(observationID,
                  timestamp,
                  sequenceID,
                  dplyr::starts_with("med")) # NOTE do we need to keep Media fields here?
  
  # only keep observationID, timestamp and media columns, use suffix for
  # observation fields
  on_seq <- observations_animals %>%
    dplyr::filter(is.na(mediaID)) %>%
    dplyr::left_join(media,
                     by = dplyr::join_by("sequenceID"),
                     suffix = c(".obs", "")) %>%
    dplyr::select(observationID,timestamp,colnames(media))
  
  on_med <- observations_animals %>% 
    dplyr::filter(!is.na(mediaID)) %>% 
    dplyr::left_join(media,
             by = dplyr::join_by("mediaID"),
             suffix = c(".obs","")) %>% 
    dplyr::select(observationID,timestamp,colnames(media))
  # mapping: merge the joined tables by mediaID and by sequenceID via a union,
  # then map to auduboncore
  
  dwc_audubon <-
  dplyr::union(on_seq, on_med) %>%
    dplyr::left_join(deployments,
                     by = dplyr::join_by("deploymentID"),
                     suffix = c(".obs_med",".dep")) %>% 
    dplyr::mutate(
      .keep = "none",
      occurrenceID = observationID,
      `dcterm:rights` = media_license,
      identifier = mediaID,
      `dc:type` = dplyr::case_when(
        grepl("video", fileMediatype) ~ "MovingImage",
        TRUE ~ "StillImage"
      ),
      providerManagedID = `_id.obs_med`,
      comments = dplyr::case_when(
        !is.na(favourite) &
          !is.na(comments.obs_med) ~ paste("media marked as favourite", comments.obs_med, sep = " | "),
        !is.na(favourite) ~ "media marked as favourite",
        TRUE ~ comments.obs_med
      ),
      captureDevice = cameraModel,
      resourceCreationTechnique = captureMethod,
      accessURI = filePath,
      format = fileMediatype,
      CreateDate = format(timestamp, format = "%Y-%m-%dT%H:%M:%SZ")
    ) %>%
    dplyr::arrange(CreateDate) 
  
  # NOTE columns need to be reordered. 
  
  # Query database
  dwc_occurrence_sql <- glue::glue_sql(
    readr::read_file(
      system.file("sql/dwc_occurrence.sql", package = "camtraptor")
    ),
    .con = con
  )
  dwc_audubon_sql <- glue::glue_sql(
    readr::read_file(
      system.file("sql/dwc_audubon.sql", package = "camtraptor")
    ),
    .con = con
  )
  # dwc_occurrence <- DBI::dbGetQuery(con, dwc_occurrence_sql)
  dwc_audubon <- DBI::dbGetQuery(con, dwc_audubon_sql)
  DBI::dbDisconnect(con)

  # Return object or write files
  if (is.null(directory)) {
    list(
      dwc_occurrence = dplyr::as_tibble(dwc_occurrence),
      dwc_audubon = dplyr::as_tibble(dwc_audubon)
    )
  } else {
    dwc_occurrence_path <- file.path(directory, "dwc_occurrence.csv")
    dwc_audubon_path <- file.path(directory, "dwc_audubon.csv")
    message(glue::glue(
      "Writing data to:",
      dwc_occurrence_path,
      dwc_audubon_path,
      .sep = "\n"
    ))
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
    readr::write_csv(dwc_occurrence, dwc_occurrence_path, na = "")
    readr::write_csv(dwc_audubon, dwc_audubon_path, na = "")
  }
}
