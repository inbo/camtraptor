#' Get number of observations for each deployment
#'
#' Function to get the number of observations (of a subset of species) per
#' deployment. The number of observations is defined as the number of distinct
#' sequences.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations` 2. `deployments` 3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#'
#' @param species a character with scientific names or common names (case
#'   insensitive). If "all", default, all scientific names are automatically
#'   selected. If `NULL` all observations of all species are taken into account
#' @param ... filter predicates for filtering on deployments
#' @importFrom dplyr .data %>% as_tibble bind_rows group_by n_distinct mutate
#'   rename select summarise ungroup relocate
#' @importFrom glue glue
#' @export

#' @return a tibble (data.frame) with the following columns:
#' - `deployment_id`:  deployment unique identifier
#' - `scientific_name`: scientific name of the species. This column is omitted
#' if argument `species` = NULL
#' - `n`: (integer) number of observations
#'
#' @examples
#'
#' # get number of observations for each species
#' get_n_obs(camtrapdp)
#'
#' # get number of obs of all species, not identified individuals as well
#' get_n_obs(camtrapdp, species = NULL)
#'
#' # get number of observations of Gallinula chloropus
#' get_n_obs(camtrapdp, species = "Gallinula chloropus")
#'
#' # get number of observations of Common Moorhen
#' get_n_obs(camtrapdp, species = "Common Moorhen")
#'
#' # case insensitive
#' get_n_obs(camtrapdp, species = "cOmmon moorhEn")
#'
#' # applying filter(s), e.g. deployments with latitude >= 51.28
#' get_n_obs(camtrapdp, pred_gte("latitude", 51.28))
#'
get_n_obs <- function(datapkg, ..., species = "all") {

  # check input data package
  check_datapkg(datapkg)

  # get observations of the selected species
  if (!is.null(species)) {
    # if species == all retrieve all detected species
    if ("all" %in% species) {
      # if also other values are present, they will be ignored
      if (length(species) > 1) {
        ignored_species <- species[!species == "all"]
        warning(glue(
          "Value 'all' found in species.
              All others values are ignored: {ignored_species*}.",
          .transformer = collapse_transformer(
            sep = ", ",
            last = " and "
          )
        ))
      }
      species <- get_species(datapkg)$scientific_name
    }
    # check species and get scientific names
    species <- check_species(datapkg, species)
    datapkg$observations <-
      datapkg$observations %>%
      filter(tolower(.data$scientific_name) %in% tolower(species))
  }

  # extract observations and deployments
  observations <- datapkg$observations
  deployments <- datapkg$deployments

  # apply filtering
  deployments <- apply_filter_predicate(
    df = deployments,
    verbose = TRUE,
    ...)

  deployment_id <- deployments$deployment_id

  deployments_no_obs <- get_dep_no_obs(
    datapkg,
    pred_in("deployment_id",deployment_id)
  )

  # get number of observations collected by each deployment for each species
  n_obs <-
    observations %>%
    group_by(.data$deployment_id, .data$scientific_name) %>%
    summarise(n = n_distinct(sequence_id)) %>%
    ungroup()

  # get all combinations deployments ID - scientific name
  combinations_dep_species <-
    expand.grid(deployments$deployment_id, unique(observations$scientific_name)) %>%
    rename(deployment_id = .data$Var1,
           scientific_name = .data$Var2) %>%
    as_tibble()

  # set 0 to combinations without observations (i.e. n = NA after join)
  n_obs <-
    combinations_dep_species %>%
    left_join(n_obs,
              by = c("deployment_id", "scientific_name")) %>%
    mutate(n = ifelse(is.na(.data$n), 0, .data$n)) %>%
    mutate(n = as.integer(.data$n))

  if (!is.null(species)) {
    return(n_obs)
  } else {
    # remove group on species and sum all observations per deployment
    n_obs %>%
      group_by(.data$deployment_id) %>%
      summarise(n = sum(.data$n)) %>%
      ungroup()
  }
}
