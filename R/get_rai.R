#' Get Relative Abundance Index (RAI)
#'
#' @name get_rai
#'
#' @description Function to get the RAI (Relative Abundance Index) per
#'   deployment.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#' 1. `observations`
#' 2. `deployments`
#' 3. `multimedia`
#'
#' and a list with metadata: `datapackage`
#'
#' @param species a character with scientific names or common names (case
#'   insensitive). If "all" (default), all scientific names are automatically
#'   selected
#' @param sex a character defining the sex class to filter on, e.g. `"female"`
#'   or `c("male", "undefined")`.  If `NULL`, default, all observations of all
#'   sex classes are taken into account.
#' @param age a character vector defining the age class to filter on, e.g.
#'   `"adult"` or `c("subadult", "adult")`. If `NULL`, default, all observations
#'   of all age classes are taken into account.
#' @param ... filter predicates for filtering on deployments
#'
#' @export

#' @return a tibble (data.frame) with the following columns:
#' - `deployment_id` deployment unique identifier
#' - `scientific_name` scientific name
#' - `rai`: relative abundance index
#'
#' @family RAI functions
#'
#' @examples
#' # calculate RAI for all species
#' get_rai(camtrapdp) # species = "all" by default, so equivalent of
#' get_rai(camtrapdp, species = "all")
#'
#' # selected species
#' get_rai(camtrapdp, species = c("Anas platyrhynchos", "Rattus norvegicus"))
#'
#' # with common names
#' get_rai(camtrapdp, species = c("Mallard", "norway rat"))
#'
#' # mixed scientific and vernacular names
#' get_rai(camtrapdp, species = c("Anas platyrhynchos", "norway rat"))
#'
#' # species argument is case insensitive
#' get_rai(camtrapdp, species = c("ANAS plAtyRhynChOS"))
#'
#' # specify sex
#' get_rai(camtrapdp, sex = "female")
#' get_rai(camtrapdp, sex = c("female", "undefined"))
#'
#' # specify age
#' get_rai(camtrapdp, age = "adult")
#' get_rai(camtrapdp, age = c("adult", "subadult"))
#'
#' # apply filter(s): deployments with latitude >= 51.28
#' get_rai(camtrapdp, pred_gte("latitude", 51.28))
#'
get_rai <- function(datapkg, ...,
                    species = "all",
                    sex = NULL,
                    age = NULL
                    ) {
  get_rai_primitive(datapkg, ...,
                    use = "n_obs",
                    species = species,
                    sex = sex, age = age)
}

#' Get Relative Abundance Index (RAI) based on number of individuals
#'
#' @name get_rai_individuals
#'
#' @description Function to get the RAI (Relative Abundance Index) per
#'   deployment based on number of detected individuals instead of the number of
#'   observations.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#' 1. `observations`
#' 2. `deployments`
#' 3. `multimedia`
#'
#' and a list with metadata: `datapackage`
#'
#' @param species a character with scientific names or common names (case
#'   insensitive). If "all" (default), all scientific names are automatically
#'   selected
#' @param sex a character defining the sex class to filter on, e.g. `"female"`
#'   or `c("male", "undefined")`.  If `NULL`, default, all observations of all
#'   sex classes are taken into account.
#' @param age a character vector defining the age class to filter on, e.g.
#'   `"adult"` or `c("subadult", "adult")`. If `NULL`, default, all observations
#'   of all age classes are taken into account.
#' @param ... filter predicates for filtering on deployments
#'
#' @export

#' @return a tibble (data.frame) with the following columns:
#' - `deployment_id` deployment unique identifier
#' - `scientific_name` scientific name
#' - `rai`: relative abundance index
#'
#' @family RAI functions
#'
#' @examples
#' # calculate RAI based on number of individuals
#' get_rai_individuals(camtrapdp) # species = "all" by default, so equivalent of
#' get_rai_individuals(camtrapdp, species = "all")
#'
#' # selected species
#' get_rai_individuals(camtrapdp,
#'   species = c("Anas platyrhynchos", "Rattus norvegicus")
#' )
#'
#' # with common names
#' get_rai_individuals(camtrapdp, species = c("Mallard", "norway rat"))
#'
#' # mixed scientific and vernacular names
#' get_rai_individuals(camtrapdp, species = c("Anas platyrhynchos", "norway rat"))
#'
#' # species argument is case insensitive
#' get_rai_individuals(camtrapdp, species = c("ANAS plAtyRhynChOS"))
#'
#' # specify sex
#' get_rai_individuals(camtrapdp, sex = "female")
#' get_rai_individuals(camtrapdp, sex = c("female", "undefined"))
#'
#' # specify age
#' get_rai_individuals(camtrapdp, age = "adult")
#' get_rai_individuals(camtrapdp, age = c("adult", "subadult"))
#'
#' # apply filter(s): deployments with latitude >= 51.28
#' get_rai_individuals(camtrapdp, pred_gte("latitude", 51.28))
#'
get_rai_individuals <- function(datapkg, ...,
                    species = "all",
                    sex = NULL,
                    age = NULL
) {
  get_rai_primitive(datapkg, ...,
                    use = "n_individuals",
                    species = species,
                    sex = sex, age = age)
}


#' Primitive function for RAI calculation
#'
#' This function is the primitive function behind `get_rai()` and
#' `get_rai_individuals()` to calculate RAI based on number of observations or
#' number of individuals respectivel
#'
#' @param use character, one of:
#' - `"n_obs"`: calculate RAI based on number of observation (standard)
#' - `"n_individuals"`: calculate RAI based on number of individuals
#'
#' @importFrom dplyr .data %>% group_by left_join select summarise ungroup
#'
#' @keywords internal
#'
#' @noRd
#'
#' @return a data.frame (tibble)
get_rai_primitive <- function(datapkg, use, species, sex, age, ...) {
  # check input data package
  check_datapkg(datapkg)

  # define possible feature values
  uses <- c("n_obs", "n_individuals")

  # check use
  check_value(use, uses, "use", null_allowed = FALSE)
  assert_that(length(use) == 1,
              msg = "use must have length 1")

  # get all identified species if species arg is equal to "all"
  if ("all" %in% species) {
    species <- get_species(datapkg)$scientific_name
  }
  # check species
  species <- check_species(datapkg, species)

  if (use == "n_obs") {
    # get number of observations
    n_df <- get_n_obs(datapkg, species = species, sex = sex, age = age, ...)
  } else {
    # get number of individuals
    n_df <- get_n_individuals(datapkg,
                              species = species,
                              sex = sex,
                              age = age,
                              ...
                              )
  }

  # extract deployments
  deployments <- datapkg$deployments

  # get deployment duration (effort) in seconds (standard duration in lubridate)
  dep_effort <- get_effort(datapkg, unit = NULL, ...)

  # calculate RAI
  n_df %>%
    left_join(dep_effort,
              by = "deployment_id") %>%
    group_by(.data$deployment_id,
             .data$scientific_name) %>%
    summarise(rai = .data$n * 100 / (as.numeric(.data$effort)/24/60/60)) %>%
    ungroup()
}
