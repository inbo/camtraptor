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
#' - **title**: Original title + `[animal observations]`.
#' - **description**: Automatically created first paragraph describing this is
#'   a derived dataset, followed by the original dataset description.
#' - **license**: License of the original dataset.
#' - **creators**: Creators of the original dataset.
#' - **contact**: `contact` or first creator of the original dataset.
#' - **metadata provider**: `contact` or first creator of the original dataset.
#' - **keywords**: Keywords of the original dataset.
#' - **associated parties**: Organizations as defined in
#'   `package$organizations`.
#' - **geographic coverage**: Bounding box as defined `package$spatial`.
#' - **taxonomic coverage**: Species as defined in `package$taxonomic`.
#' - **temporal coverage**: Date range as defined in `package$temporal`.
#' - **project data**: Title, identifier, description, and sampling design
#'   information as defined in `package$project`.
#' - **alternative identifier**: DOI of the original dataset. This way, no new
#'   DOI will be created when publishing to GBIF.
#' - **external link**: URL of the project as defined in `package$project$path`.
#'
#' To be set manually in the GBIF IPT: **type**, **subtype**,
#' **update frequency**, and **publishing organization**.
#'
#' Not set: sampling methods and citations.
#' Not applicable: collection data.
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
                      contact = NULL, rights_holder = package$rightsHolder) {
  # TODO: Hotfix to deal with 1 level deep metadata
  orig_package <- package
  package <- package$datapackage

  # Retrieve metadata from DataCite and build EML
  assertthat::assert_that(
    !is.null(doi),
    msg = "No DOI found in `package$id`, provide one in `doi` parameter."
  )
  message("Creating EML metadata.")
  eml <- movepub::datacite_to_eml(doi)

  # Set platform
  platform <- package$platform$title # Use in DwC

  # Update title
  title <- paste(eml$dataset$title, "[animal observations]") # Used in DwC
  eml$dataset$title <- title

  # Update license
  license_url <- eml$dataset$intellectualRights$rightsUri # Used in DwC
  license_code <- eml$dataset$intellectualRights$rightsIdentifier
  eml$dataset$intellectualRights <- NULL # Remove original license elements that make EML invalid
  eml$dataset$intellectualRights$para <- license_code

  # Set media license
  media_license_url <- purrr::keep(package$licenses, ~ .$scope == "media")[[1]]$path

  # Add extra paragraph to description
  first_author <- eml$dataset$creator[[1]]$individualName$surName
  pub_year <- substr(eml$dataset$pubDate, 1, 4)
  doi_url <- eml$dataset$alternateIdentifier[[1]] # Used in DwC
  first_para <- glue::glue(
    # Add span to circumvent https://github.com/ropensci/EML/issues/342
    "<span></span>This camera trap dataset is derived from ",
    "{first_author} et al. ({pub_year}, <a href=\"{doi_url}\">{doi_url}</a>), ",
    "a Camera Trap Data Package ",
    "(<a href=\"https://tdwg.github.io/camtrap-dp/\">Camtrap DP</a>). ",
    "Data have been standardized to Darwin Core using the ",
    "<a href=\"https://inbo.github.io/camtraptor/\">camtraptor</a> R package ",
    "and only include observations (and associated media) of animals. ",
    "Excluded are records that document blank or unclassified media, ",
    "vehicles and observations of humans. ",
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

  # Add organizations as associated parties
  eml$dataset$associatedParty <-
    purrr::map(package$organizations, ~ EML::set_responsibleParty(
      givenName = "", # Circumvent https://github.com/ropensci/EML/issues/345
      organizationName = .$title,
      onlineUrl = .$path
    ))

  # Set coverage
  bbox <- package$spatial$bbox
  taxonomy <- get_species(orig_package)
  if ("taxonRank" %in% names(taxonomy)) {
    taxonomy <- dplyr::filter(taxonomy, taxonRank == "species")
  }
  sci_names <-
    dplyr::rename(taxonomy, Species = scientificName) %>%
    dplyr::select(Species)

  eml$dataset$coverage <- EML::set_coverage(
    begin = package$temporal$start,
    end = package$temporal$end,
    west = bbox[1],
    south = bbox[2],
    east = bbox[3],
    north = bbox[4],
    sci_names = sci_names
  )

  # Set project metadata
  project <- package$project
  capture_method <- paste(package$project$captureMethod, collapse = " and ")
  animal_type <- paste(package$project$animalTypes, collapse = " and ")
  design_para <- glue::glue(
    "This project uses a {project$samplingDesign} sampling design, ",
    "with {animal_type} animals and ",
    "camera traps taking media using {capture_method}. ",
    "Media are classified at {project$classificationLevel} level."
  )
  eml$dataset$project <- list(
    id = project$id, # Can be NULL, assigned as <project id="id">
    title = project$title,
    abstract = list(para = project$description), # Can be NULL
    designDescription = list(description = list(para = design_para))
  )

  # Set external link to project URL (can be NULL)
  if (!is.null(project$path)) {
    eml$dataset$distribution = list(
      scope = "document", online = list(
        url = list("function" = "information", project$path)
      )
    )
  }

  # Read data from package
  # Already read with read_camtrap_dp()

  # Create database
  message("Creating database and transforming to Darwin Core.")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "deployments", dplyr::tibble(orig_package$deployments))
  DBI::dbWriteTable(con, "media", dplyr::tibble(orig_package$media))
  DBI::dbWriteTable(con, "observations", dplyr::tibble(orig_package$observations))

  # Query database
  dwc_occurrence_sql <- glue::glue_sql(
    readr::read_file(
      system.file("sql/dwc_occurrence.sql", package = "camtraptor")
    ),
    .con = con
  )
  dwc_multimedia_sql <- glue::glue_sql(
    readr::read_file(
      system.file("sql/dwc_multimedia.sql", package = "camtraptor")
    ),
    .con = con
  )
  dwc_occurrence <- DBI::dbGetQuery(con, dwc_occurrence_sql)
  dwc_multimedia <- DBI::dbGetQuery(con, dwc_multimedia_sql)
  DBI::dbDisconnect(con)

  # Write files
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  EML::write_eml(eml, file.path(directory, "eml.xml"))
  readr::write_csv(
    dwc_occurrence, file.path(directory, "dwc_occurrence.csv"), na = ""
  )
  readr::write_csv(
    dwc_multimedia, file.path(directory, "dwc_multimedia.csv"), na = ""
  )
}
