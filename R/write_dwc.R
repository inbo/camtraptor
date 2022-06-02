#' Transform camera trap data to Darwin Core
#'
#' Transforms a published [Camera Trap Data Package
#' (Camtrap DP)](https://github.com/tdwg/camtrap-dp) to Darwin Core CSV and EML
#' files that can be uploaded to a [GBIF IPT](https://www.gbif.org/ipt) for
#' publication.
#' A `meta.xml` file is not created.
#'
#' @param package A Camtrap DP, as read by [read_camtrap_dp()].
#' @param directory Path to local directory to write files to.
#' @param doi DOI of the original dataset, used to get metadata.
#' @param contact Person to be set as resource contact and metadata provider.
#'   To be provided as a `person()`.
#' @param rights_holder Acronym of the organization owning or managing the
#'   rights over the data.
#' @return CSV (data) and EML (metadata) files written to disk.
#' @export
#' @section Metadata:
#'
#' Metadata are derived from the original dataset by looking up its `doi` in
#' DataCite ([example](https://doi.org/10.5281/zenodo.5590881)) and transforming
#' these to EML.
#' Uses `movepub::datacite_to_eml()` under the hood.
#' The following properties are set:
#'
#' - TO TEST **title**: Original title + `[subsampled representation]`.
#' - **description**: Automatically created first paragraph describing this is
#'   a derived dataset, followed by the original dataset description.
#' - **creators**: Creators of the original dataset.
#' - **license**: License of the original dataset.
#' - **contact**: `contact` or first creator of the original dataset.
#' - **metadata provider**: `contact` or first creator of the original dataset.
#' - **keywords**: Keywords of the original dataset.
#' - **alternative identifier**: DOI of original dataset. This way, no new DOI
#'   will be created when publishing to GBIF.
#' - TO TEST **external link** (and alternative identifier): URL of the Movebank study.
#'
#' To be set manually in the GBIF IPT: **type**, **subtype**,
#' **update frequency**, and **publishing organization**.
#'
#' Not set: geographic, taxonomic, temporal coverage, associated parties,
#' project data, sampling methods, and citations. Not applicable: collection
#' data.
#'
#' @section Data:
#'
#' `package` is expected to contain the resources `deployments`, `media` and
#' `observations`.
#' Their CSV data are loaded in to a SQLite database,
#' [transformed to Darwin Core using SQL](https://github.com/inbo/camtraptor/tree/main/inst/sql)
#' and written to disk as CSV file(s).
#'
#' Key features of the Darwin Core transformation:
#' - TODO
#' @examples
#' # TODO
write_dwc <- function(package, directory = ".", doi = package$id,
                      contact = NULL, rights_holder = NULL) {
  # TODO: Hotfix to deal with 1 level deep metadata
  package <- package$datapackage

  # Retrieve metadata from DataCite and build EML
  assertthat::assert_that(
    !is.null(doi),
    msg = "No DOI found in `package$id`, provide one in `doi` parameter."
  )
  message("Creating EML metadata.")
  eml <- movepub::datacite_to_eml(doi)

  # Update title
  title <- paste(eml$dataset$title, "[subsampled representation]") # Used in DwC
  eml$dataset$title <- title

  # Update license
  license_url <- eml$dataset$intellectualRights$rightsUri # Used in DwC
  license_code <- eml$dataset$intellectualRights$rightsIdentifier
  eml$dataset$intellectualRights <- NULL # Remove original license elements that make EML invalid
  eml$dataset$intellectualRights$para <- license_code

  # Add extra paragraph to description
  first_author <- eml$dataset$creator[[1]]$individualName$surName
  pub_year <- substr(eml$dataset$pubDate, 1, 4)
  doi_url <- eml$dataset$alternateIdentifier[[1]] # Used in DwC
  first_para <- glue::glue(
    # Add span to circumvent https://github.com/ropensci/EML/issues/342
    "<span></span>This camera trap dataset is derived from ",
    "{first_author} et al. ({pub_year}, <a href=\"{doi_url}\">{doi_url}</a>). ",
    "Data have been standardized to Darwin Core using the ",
    "<a href=\"https://inbo.github.io/camtraptor/\">camtraptor</a> R package ",
    "and exclude observations of humans and absence records. ",
    "The original dataset description follows.",
    .null = ""
  )
  eml$dataset$abstract$para <- purrr::prepend(
    eml$dataset$abstract$para,
    paste0("<![CDATA[", first_para, "]]>")
  )

  # Update contact and set metadata provider
  if (!is.null(contact)) {
    eml$dataset$contact <- EML::set_responsibleParty(
      givenName = contact$given,
      surName = contact$family,
      electronicMailAddress = contact$email,
      userId = if (!is.null(contact$comment[["ORCID"]])) {
        list(directory = "http://orcid.org/", contact$comment[["ORCID"]])
      } else {
        NULL
      }
    )
  }
  eml$dataset$metadataProvider <- eml$dataset$contact

  # Read data from package
  # message("Reading data from `package`.")
  # assertthat::assert_that(
  #   c("reference-data") %in% frictionless::resources(package),
  #   msg = "`package` must contain resource `reference-data`."
  # )
  # assertthat::assert_that(
  #   c("gps") %in% frictionless::resources(package),
  #   msg = "`package` must contain resource `gps`."
  # )
  # ref <- frictionless::read_resource(package, "reference-data")
  # gps <- frictionless::read_resource(package, "gps")

  # Create database
  # message("Creating database and transforming to Darwin Core.")
  # con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # DBI::dbWriteTable(con, "reference_data", ref)
  # DBI::dbWriteTable(con, "gps", gps)

  # Query database
  # dwc_occurrence_sql <- glue::glue_sql(
  #   readr::read_file(
  #     system.file("sql/movebank_dwc_occurrence.sql", package = "movepub")
  #   ),
  #   .con = con
  # )
  # dwc_occurrence <- DBI::dbGetQuery(con, dwc_occurrence_sql)
  # DBI::dbDisconnect(con)

  # Write files
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  EML::write_eml(eml, file.path(directory, "eml.xml"))
  # readr::write_csv(
  #   dwc_occurrence,
  #   file.path(directory, "dwc_occurrence.csv"),
  #   na = ""
  # )
}
