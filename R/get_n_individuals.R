#' Get number of individuals for each deployment
#'
#' Function to get the number of individuals (of a subset of species) per
#' deployment. The number of observed individuals is stored in field `count` of
#' `observations`.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param species a character with scientific names or common names (case
#'   insensitive). If "all", default, all scientific names are automatically
#'   selected. If `NULL` all observations of all species are taken into account
#' @param sex a character defining the sex class to filter on, e.g. `"female"`
#'   or `c("male", "unknown")`.  If `NULL`, default, all observations of all
#'   sex classes are taken into account.
#' @param life_stage a character vector defining the life stage class to filter on, e.g.
#'   `"adult"` or `c("subadult", "adult")`. If `NULL`, default, all observations
#'   of all life stage classes are taken into account.
#' @param ... filter predicates for filtering on deployments
#' @importFrom dplyr .data %>% as_tibble bind_rows group_by count mutate
#'   rename select summarise ungroup relocate
#' @importFrom glue glue
#' @export

#' @return a tibble (data.frame) with the following columns:
#' - `deploymentID`:  deployment unique identifier
#' - `scientificName`: scientific name of the species. This column is omitted
#' if argument `species` = NULL
#' - `n`: (integer) number of individuals
#'
#' @examples
#'
#' # get number of observations for each species
#' get_n_individuals(mica)
#'
#' # get number of obs of all species, not identified individuals as well
#' get_n_individuals(mica, species = NULL)
#'
#' # get number of observations of Anas platyrhynchos
#' get_n_individuals(mica, species = "Anas platyrhynchos")
#'
#' # get number of observations of eurasian beaver (vernacular name)
#' get_n_individuals(mica, species = "eurasian beaver")
#'
#' # mix scientific and vernacular names
#' get_n_individuals(mica, species = c("Anas platyrhynchos", "eurasian beaver"))
#' 
#' # case insensitive
#' get_n_individuals(mica, species = "AnAS PLatyrhyncHOS")
#' get_n_individuals(mica, species = "eurasian BEAVER")
#'
#' # specify life stage
#' get_n_individuals(mica, life_stage = "adult")
#'
#' # specify sex
#' get_n_individuals(mica, sex = "female")
#'
#' # specify both sex and life stage
#' get_n_individuals(mica, sex = "unknown", life_stage = "adult")
#'
#' # applying filter(s), e.g. deployments with latitude >= 51.18
#' get_n_individuals(mica, pred_gte("latitude", 51.18))
#'
get_n_individuals <- function(datapkg,
                              ...,
                              species = "all",
                              sex = NULL,
                              life_stage = NULL) {
  # check input data package
  check_datapkg(datapkg)

  # avoid to call variables like column names to make life easier using filter()
  sex_value <- sex

  # check sex and life stage values
  check_value(sex_value, unique(datapkg$observation$sex), "sex")
  check_value(life_stage, unique(datapkg$observation$lifeStage), "life_stage")

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
      species <- get_species(datapkg)$scientificName
    }
    # check species and get scientific names
    species <- check_species(datapkg, species)
    datapkg$observations <-
      datapkg$observations %>%
      filter(tolower(.data$scientificName) %in% tolower(species))
  }

  # get observations of the specified sex
  if (!is.null(sex)) {
    datapkg$observations <-
      datapkg$observations %>%
      filter(sex %in% sex_value)
  }

  # get observations of the specified life stage
  if (!is.null(life_stage)) {
    datapkg$observations <-
      datapkg$observations %>%
      filter(lifeStage %in% life_stage)
  }

  # extract observations and deployments
  observations <- datapkg$observations
  deployments <- datapkg$deployments

  # apply filtering
  deployments <- apply_filter_predicate(
    df = deployments,
    verbose = TRUE,
    ...)

  deploymentID <- deployments$deploymentID

  deployments_no_obs <- get_dep_no_obs(
    datapkg,
    pred_in("deploymentID",deploymentID)
  )

  # get number of individuals collected by each deployment for each species
  n_individuals <-
    observations %>%
    group_by(.data$deploymentID,
             .data$scientificName) %>%
    summarise(n = sum(.data$count)) %>%
    ungroup()

  # get all combinations deployments - scientific name
  combinations_dep_species <-
    expand.grid(deployments$deploymentID,
                unique(c(observations$scientificName, species))) %>%
    rename(deploymentID = .data$Var1,
           scientificName = .data$Var2) %>%
    as_tibble()

  # set 0 to combinations without observed individuals (i.e. n = NA after join)
  n_individuals <-
    combinations_dep_species %>%
    left_join(n_individuals,
              by = c("deploymentID", "scientificName")) %>%
    mutate(n = ifelse(is.na(.data$n), 0, .data$n)) %>%
    mutate(n = as.integer(.data$n))

  if (is.null(species)) {
    # sum all observations per deployment
    n_individuals <-
      n_individuals %>%
      group_by(.data$deploymentID) %>%
      summarise(n = sum(.data$n)) %>%
      ungroup()
  }

  # order result by deployments and following same order as in deployments df
  deployments %>%
    select(deploymentID) %>%
    left_join(n_individuals, by = "deploymentID")
}
