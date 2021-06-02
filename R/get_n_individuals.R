#' Get number of individuals for each deployment
#'
#' Function to get the number of individuals (of a subset of species) per
#' deployment. The number of observed individuals is stored in field `count` of
#' `observations`.
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
#' @param sex a character defining the sex class to filter on, e.g. `"female"`
#'   or `c("male", "undefined")`.  If `NULL`, default, all observations of all
#'   sex classes are taken into account.
#' @param age a character vector defining the age class to filter on, e.g.
#'   `"adult"` or `c("subadult", "adult")`. If `NULL`, default, all observations
#'   of all age classes are taken into account.
#' @param ... filter predicates for filtering on deployments
#' @importFrom dplyr .data %>% as_tibble bind_rows group_by count mutate
#'   rename select summarise ungroup relocate
#' @importFrom glue glue
#' @export

#' @return a tibble (data.frame) with the following columns:
#' - `deployment_id`:  deployment unique identifier
#' - `scientific_name`: scientific name of the species. This column is omitted
#' if argument `species` = NULL
#' - `n`: (integer) number of individuals
#'
#' @examples
#'
#' # get number of observations for each species
#' get_n_individuals(camtrapdp)
#'
#' # get number of obs of all species, not identified individuals as well
#' get_n_individuals(camtrapdp, species = NULL)
#'
#' # get number of observations of Gallinula chloropus
#' get_n_individuals(camtrapdp, species = "Gallinula chloropus")
#'
#' # get number of observations of Common Moorhen
#' get_n_individuals(camtrapdp, species = "Common Moorhen")
#'
#' # case insensitive
#' get_n_individuals(camtrapdp, species = "cOmmon moorhEn")
#' get_n_individuals(camtrapdp, species = "galliNULa CHloropUs")
#'
#' # specify age
#' get_n_individuals(camtrapdp, age = "adult")
#'
#' # specify sex
#' get_n_individuals(camtrapdp, sex = "female")
#'
#' # specify both sex and age
#' get_n_individuals(camtrapdp, sex = "undefined", age = "adult")
#'
#' # applying filter(s), e.g. deployments with latitude >= 51.28
#' get_n_individuals(camtrapdp, pred_gte("latitude", 51.28))
#'
get_n_individuals <- function(datapkg,
                              ...,
                              species = "all",
                              sex = NULL,
                              age = NULL) {
  # check input data package
  check_datapkg(datapkg)

  # avoid to call variables like column names to make life easier using filter()
  sex_value <- sex
  age_value <- age


  # check sex and age values
  check_value(sex_value, unique(datapkg$observation$sex), "sex")
  check_value(age_value, unique(datapkg$observation$age), "age")

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

  # get observations of the specified sex
  if (!is.null(sex)) {
    datapkg$observations <-
      datapkg$observations %>%
      filter(sex %in% sex_value)
  }

  # get observations of the specified age
  if (!is.null(age)) {
    datapkg$observations <-
      datapkg$observations %>%
      filter(age %in% age_value)
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

  # get number of individuals collected by each deployment for each species
  n_individuals <-
    observations %>%
    group_by(.data$deployment_id,
             .data$scientific_name) %>%
    summarise(n = sum(.data$count)) %>%
    ungroup()

  # get all combinations deployments - scientific name
  combinations_dep_species <-
    expand.grid(deployments$deployment_id,
                unique(c(observations$scientific_name, species))) %>%
    rename(deployment_id = .data$Var1,
           scientific_name = .data$Var2) %>%
    as_tibble()

  # set 0 to combinations without observed individuals (i.e. n = NA after join)
  n_individuals <-
    combinations_dep_species %>%
    left_join(n_individuals,
              by = c("deployment_id", "scientific_name")) %>%
    mutate(n = ifelse(is.na(.data$n), 0, .data$n)) %>%
    mutate(n = as.integer(.data$n))

  if (is.null(species)) {
    # sum all observations per deployment
    n_individuals <-
      n_individuals %>%
      group_by(.data$deployment_id) %>%
      summarise(n = sum(.data$n)) %>%
      ungroup()
  }

  # order result by deployments and following same order as in deployments df
  deployments %>%
    select(deployment_id) %>%
    left_join(n_individuals, by = "deployment_id")
}
