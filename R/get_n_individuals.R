#' Get number of individuals for each deployment
#'
#' Gets the number of individuals (of a subset of species) per deployment.
#' The number of observed individuals is stored in field `count` of
#' `observations`.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param species Character with scientific names or common names (case
#'   insensitive).
#'   If `"all"` (default) all scientific names are automatically selected.
#'   If `NULL` all observations of all species are taken into account.
#' @param sex Character defining the sex class to filter on, e.g. `"female"`
#'   or `c("male", "unknown")`.
#'   If `NULL` (default) all observations of all sex classes are taken into
#'   account.
#' @param life_stage Character vector defining the life stage class to filter
#'   on, e.g. `"adult"` or `c("subadult", "adult")`.
#'   If `NULL` (default) all observations of all life stage classes are taken
#'   into account.
#' @param datapkg Deprecated.
#'   Use `package` instead.
#' @param ... filter predicates for filtering on deployments
#' @return A tibble data frame with the following columns:
#' - `deploymentID`: Deployment unique identifier.
#' - `scientificName`: Scientific name of the species.
#'   This column is omitted if argument `species = NULL`.
#' - `n`: Number of individuals.
#' @family exploration functions
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' # Get number of observations for each species
#' get_n_individuals(mica)
#'
#' # Get number of obs of all species, not identified individuals as well
#' get_n_individuals(mica, species = NULL)
#'
#' # Get number of observations of Anas platyrhynchos
#' get_n_individuals(mica, species = "Anas platyrhynchos")
#'
#' # Get number of observations of eurasian beaver (vernacular name)
#' get_n_individuals(mica, species = "eurasian beaver")
#'
#' # Mix scientific and vernacular names
#' get_n_individuals(mica, species = c("Anas platyrhynchos", "eurasian beaver"))
#'
#' # Case insensitive
#' get_n_individuals(mica, species = "AnAS PLatyrhyncHOS")
#' get_n_individuals(mica, species = "eurasian BEAVER")
#'
#' # Specify life stage
#' get_n_individuals(mica, life_stage = "adult")
#'
#' # Specify sex
#' get_n_individuals(mica, sex = "female")
#'
#' # Specify both sex and life stage
#' get_n_individuals(mica, sex = "unknown", life_stage = "adult")
#'
#' # Apply filter(s), e.g. deployments with latitude >= 51.18
#' get_n_individuals(mica, pred_gte("latitude", 51.18))
get_n_individuals <- function(package = NULL,
                              ...,
                              species = "all",
                              sex = NULL,
                              life_stage = NULL,
                              datapkg = lifecycle::deprecated()) {
  # check input data package
  package <- check_package(package, datapkg, "get_n_individuals")

  # avoid to call variables like column names to make life easier using filter()
  sex_value <- sex

  # check sex and life stage values
  check_value(sex_value, unique(package$data$observations$sex), "sex")
  check_value(life_stage, unique(package$data$observations$lifeStage), "life_stage")

  # get observations of the selected species
  if (!is.null(species)) {
    # if species == all retrieve all detected species
    if ("all" %in% species) {
      # if also other values are present, they will be ignored
      if (length(species) > 1) {
        ignored_species <- species[!species == "all"]
        warning(glue::glue(
          "Value 'all' found in species.
              All others values are ignored: {ignored_species*}.",
          .transformer = collapse_transformer(
            sep = ", ",
            last = " and "
          )
        ))
      }
      species <- get_species(package)$scientificName
    }
    # check species and get scientific names
    species <- check_species(package, species)
    package$data$observations <-
      package$data$observations %>%
      dplyr::filter(tolower(.data$scientificName) %in% tolower(species))
  }

  # get observations of the specified sex
  if (!is.null(sex)) {
    package$data$observations <-
      package$data$observations %>%
      dplyr::filter(.data$sex %in% sex_value)
  }

  # get observations of the specified life stage
  if (!is.null(life_stage)) {
    package$data$observations <-
      package$data$observations %>%
      dplyr::filter(.data$lifeStage %in% life_stage)
  }

  # extract observations and deployments
  observations <- package$data$observations
  deployments <- package$data$deployments

  # apply filtering
  deployments <- apply_filter_predicate(
    df = deployments,
    verbose = TRUE,
    ...)

  deploymentID <- deployments$deploymentID

  deployments_no_obs <- get_dep_no_obs(
    package,
    pred_in("deploymentID",deploymentID)
  )

  # get number of individuals collected by each deployment for each species
  n_individuals <-
    observations %>%
    dplyr::group_by(.data$deploymentID,
             .data$scientificName) %>%
    dplyr::summarise(n = sum(.data$count)) %>%
    dplyr::ungroup()

  # get all combinations deployments - scientific name
  combinations_dep_species <-
    expand.grid(deployments$deploymentID,
                unique(c(observations$scientificName, species))) %>%
    dplyr::rename(deploymentID = .data$Var1,
           scientificName = .data$Var2) %>%
    dplyr::as_tibble()

  # set 0 to combinations without observed individuals (i.e. n = NA after join)
  n_individuals <-
    combinations_dep_species %>%
    dplyr::left_join(n_individuals,
              by = c("deploymentID", "scientificName")) %>%
    dplyr::mutate(n = ifelse(is.na(.data$n), 0, .data$n)) %>%
    dplyr::mutate(n = as.integer(.data$n))

  if (is.null(species)) {
    # sum all observations per deployment
    n_individuals <-
      n_individuals %>%
      dplyr::group_by(.data$deploymentID) %>%
      dplyr::summarise(n = sum(.data$n)) %>%
      dplyr::ungroup()
  }

  # order result by deployments and following same order as in deployments df
  deployments %>%
    dplyr::select(deploymentID) %>%
    dplyr::left_join(n_individuals, by = "deploymentID")
}
