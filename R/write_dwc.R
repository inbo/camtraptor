#' Transform Camtrap DP data to Darwin Core
#'
#' Transforms the data of a [Camera Trap Data Package](
#' https://tdwg.github.io/camtrap-dp/) to [Darwin Core](https://dwc.tdwg.org/)
#' CSV files that can be uploaded to a [GBIF IPT](https://www.gbif.org/ipt) for
#' publication.
#' A `meta.xml` file is not created.
#'
#' @param package A Camtrap DP, as read by [read_camtrap_dp()].
#' @param directory Path to local directory to write file to.
#' @return CSV files written to disk.
#' @family publication functions
#' @export
#' @section Transformation details:
#' Data are transformed following best practices (Reyserhove et al. in prep.)
#' into an
#' [Occurrence core](https://rs.gbif.org/core/dwc_occurrence_2022-02-02.xml) and
#' [Audubon Media Description extension](https://rs.gbif.org/extension/ac/audubon_2020_10_06.xml).
#' See the [SQL files](https://github.com/inbo/camtraptor/tree/main/inst/sql)
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
  dataset_name <- package$title
  dataset_id <- package$id
  rights_holder <- package$rightsHolder
  collection_code <- package$platform$title
  license <- purrr::keep(package$licenses, ~ .$scope == "data")[[1]]$path
  media_license <- purrr::keep(package$licenses, ~ .$scope == "media")[[1]]$path
  coordinate_precision <- package$coordinatePrecision

  # Create database
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "deployments", dplyr::tibble(package$data$deployments))
  DBI::dbWriteTable(con, "media", dplyr::tibble(package$data$media))
  DBI::dbWriteTable(con, "observations", dplyr::tibble(package$data$observations))

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
  dwc_occurrence <- DBI::dbGetQuery(con, dwc_occurrence_sql)
  dwc_audubon <- DBI::dbGetQuery(con, dwc_audubon_sql)
  DBI::dbDisconnect(con)

  # Write files
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  readr::write_csv(
    dwc_occurrence, file.path(directory, "dwc_occurrence.csv"), na = ""
  )
  readr::write_csv(
    dwc_audubon, file.path(directory, "dwc_audubon.csv"), na = ""
  )
}
