#' Get number of observations/individuals for each deployment
#'
#' Gets the number of observations (of a subset of species) per deployment.
#' The number of observations is defined as the number of distinct sequences
#' (`sequenceID`).
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param start Start date.
#'   Default: `NULL`.
#'   If `NULL` the earliest start date among all deployments is used.
#'   If `group_by` unit is not `NULL`, the lowest start value allowed is one
#'   group by unit before the start date of the earliest deployment.
#'   If this condition doesn't hold true, a warning is returned and the earliest
#'   start date among all deployments is used.
#'   If `group_by` unit is `NULL` the start must be later than or equal to the
#'   start date among all deployments.
#' @param end End date.
#'   Default: `NULL`.
#'   If `NULL` the latest end date among all deployments is used.
#'   If `group_by` unit is not `NULL`, the latest end value allowed is one group
#'   by unit after the end date of the latest deployment.
#'   If this condition doesn't hold true, a warning is returned and the latest
#'   end date among all deployments is used.
#'   If `group_by` unit is `NULL` the end must be earlier than or equal to the
#'   end date among all deployments.
#' @param group_by Character, one of `"day"`, `"week"`, `"month"`, `"year"`.
#'   The effort is calculated at the interval rate defined in `group_by`.
#'   Default: `NULL`: no grouping, i.e. the entire interval from `start` to
#'   `end` is taken into account as a whole. Calendar values are used, i.e.
#'   grouping by year will calculate the effort from Jan 1st up to Dec 31st for
#'   each year.   
#' @param species Deprecated.
#'   Use `filter_observations` instead.
#' @param sex Deprecated.
#'   Use `filter_observations` instead.
#' @param life_stage Deprecated.
#'   Use `filter_observations` instead.
#' @param datapkg Deprecated.
#'   Use `package` instead.
#' @param ... Filter predicates for filtering on deployments
#' @return A tibble data frame with the following columns:
#' - `deploymentID`: Deployment unique identifier.
#' - `scientificName`: Scientific name of the species.
#'   This column is omitted if parameter `species = NULL`.
#' - `n`: Number of observations.
#' @family exploration functions
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' 
n_observations <- function(package = NULL,
                      ...,
                      start = NULL,
                      end = NULL,
                      group_by = NULL,
                      species = NULL,
                      sex = NULL,
                      life_stage = NULL,
                      datapkg = lifecycle::deprecated()) {

  # Check start earlier than end
  if (!is.null(start) & !is.null(end)) {
    assertthat::assert_that(start <= end,
                            msg = "`start` must be earlier than `end`."
    )
  }
  
  # Check start and end are both dates
  assertthat::assert_that(
    is.null(start) | all(class(start) == "Date"),
    msg = glue::glue(
      "`start` must be `NULL` or an object of class Date. ",
      "Did you forget to convert a string to Date with `as.Date()`?"
    )
  )
  assertthat::assert_that(
    is.null(end) | all(class(end) == "Date"),
    msg = glue::glue(
      "`end` must be `NULL` or an object of class Date. ",
      "Did you forget to convert a string to Date with `as.Date()`?"
    )
  )
  
  # Define possible group_by values
  group_bys <- c(
    "day",
    "week",
    "month",
    "year"
  )
  durations <- c(
    lubridate::ddays(x = 1),
    lubridate::dweeks(x = 1),
    lubridate::dmonths(x = 1),
    lubridate::dyears(x = 1)
  )
  
  # # Check group_by
  # check_value(group_by, group_bys, "group_by", null_allowed = TRUE)
  # 
  # # Check camera trap data package
  # check_package(package, datapkg, "n_observations")
  # if (is.null(package) & !is.name(datapkg)) {
  #   package <- datapkg
  # }
  
  # avoid to call variables like column names to make life easier using filter()
  sex_value <- sex
  
  # check sex and lifeStage values
  # check_value(sex_value, unique(package$data$observation$sex), "sex")
  # check_value(life_stage, unique(package$data$observation$lifeStage), "lifeStage")
  
  # get observations of the selected species
  if (!is.null(species)) {
    # if species == all retrieve all detected species
    if ("all" %in% species) {
      # if also other values are present, they will be ignored
      if (length(species) > 1) {
        ignored_species <- species[!species == "all"]
        warning(glue::glue(
          "Value `all` found in `species`. All other values are ignored: ",
          glue::glue_collapse(ignored_species, sep = ", ", last = " and ")
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
      dplyr::filter(sex %in% sex_value)
  }
  
  # get observations of the specified life stage
  if (!is.null(life_stage)) {
    package$data$observations <-
      package$data$observations %>%
      dplyr::filter(.data$lifeStage %in% life_stage)
  }
  
  # Extract deployments and observations
  deployments <- package$data$deployments
  observations <- package$data$observations
  
  # Stop function and inform user about deployments with missing `start` date
  no_start_deployments <- deployments[is.na(deployments$start),]$deploymentID
  if (length(no_start_deployments) > 0) {
    stop(
      glue::glue(
        "The deployments with the following deploymentID ",
        "have missing `start` value: ",
        glue::glue_collapse(no_start_deployments, sep = ", ", last = " and "),
        "."
      )
    )
  }
  
  # Stop function and inform user about deployments with missing `end` date
  no_end_deployments <- deployments[is.na(deployments$end),]$deploymentID
  if (length(no_end_deployments) > 0) {
    stop(
      glue::glue(
        "The deployments with the following deploymentID ",
        "have missing `end` value: ",
        glue::glue_collapse(no_end_deployments, sep = ", ", last = " and "),
        "."
      )
    )
  }
  
  # Check start is earlier than end of the latest deployment
  if (!is.null(start)) {
    assertthat::assert_that(
      start <= max(deployments$end),
      msg = glue::glue(
        "`start` value is set too late. ",
        "`start` value must be not later than the end of the ", 
        "latest deployment: {max(dep_effort$date)}."
      )
    )
  }
  
  # Check end is later than begin of the earliest deployment
  if (!is.null(end)) {
    assertthat::assert_that(
      end >= min(deployments$start),
      msg = glue::glue(
        "`end` value is set too early. ",
        "`end` value must be not earlier than the start of the ", 
        "earliest deployment: {min(dep_effort$date)}."
      )
    )
  }
  
  # Check start is not earlier than start first deployment date.
  # Return a warning and set start to first day deployment otherwise.
  if (!is.null(start)) {
    if (lubridate::as_date(start) < min(deployments$start)) {
      start <- min(deployments$start)
      warning(
        glue::glue(
          "`start` value is set too early. ",
          "`start` authomatically set to start date of earliest ",
          "deployment: {start}."
        )
      )
    }
  } else {
    # Set start to date of the earliest deployment
    start <- min(deployments$start, na.rm = T)
  }
  # Check end is not later than end last deployment date.
  # Return a warning and set end to last day deployment otherwise.
  if (!is.null(end)) {
    if (lubridate::as_date(end) > max(deployments$end)) {
      end <- max(deployments$end)
      warning(
        glue::glue(
          "`end` value is set too late. ",
          "`end` authomatically set to end date of latest deployment: {end}."
        )
      )
    }
  } else {
    # Set end to date of the latest deployment
    end <- max(deployments$end, na.rm = T)
  }
  
  # Create df with all dates from start to end
  dates <- lubridate::as_date(seq(start, end, by = "days"))
  
  # apply filtering
  deployments <- apply_filter_predicate(
    df = deployments,
    verbose = TRUE,
    ...
  )
  
  deploymentID <- deployments$deploymentID
  
  deployments_no_obs <- get_dep_no_obs(
    package,
    pred_in("deploymentID", deploymentID)
  )
  
  # get number of observations collected by each deployment for each species
  n_table <- 
    observations %>%
    dplyr::mutate(date = lubridate::as_date(timestamp)) %>%
    dplyr::filter(date >= min(dates), date <= max(dates))
    
  n_table <-
    n_table %>%
    dplyr::group_by(.data$deploymentID, 
                    .data$scientificName, 
                    .data$date) %>%
    dplyr::summarise(n_obs = dplyr::n_distinct(.data$sequenceID),
                     n_ind = sum(.data$count),
                     .groups = "keep") %>%
    dplyr::ungroup()
  
  # get all combinations deployments - scientific name
  combinations_dep_species_dates <-
    expand.grid(
      deployments$deploymentID,
      unique(c(unique(observations$scientificName), species)),
      dates
    ) %>%
    dplyr::rename(deploymentID = "Var1", scientificName = "Var2", date = "Var3") %>%
    dplyr::as_tibble()
  
  # set 0 to combinations without observations (i.e. n = NA after join)
  na_to_zero <- function(x) ifelse(is.na(x), 0, x)
  n_table <-
    combinations_dep_species_dates %>%
    dplyr::left_join(n_table,
                     by = c("deploymentID", "scientificName", "date")
    ) %>%
    dplyr::mutate_at(c("n_obs", "n_ind"), na_to_zero) %>%
    dplyr::mutate_at(c("n_obs", "n_ind"), as.integer)
  
  if (is.null(group_by) & is.null(species)) {
    # sum all observations per deployment
    n_table <-
      n_table %>%
      dplyr::group_by(.data$deploymentID) %>%
      dplyr::summarise_at(c("n_obs", "n_ind"), sum) %>%
      dplyr::ungroup()
  } else if(is.null(group_by) & !is.null(species)) {
    # sum all observations per deployment and species
    n_table <-
      n_table %>%
      dplyr::group_by(.data$deploymentID, .data$scientificName) %>%
      dplyr::summarise_at(c("n_obs", "n_ind"), sum) %>%
      dplyr::ungroup()
  } else if(!is.null(group_by) & is.null(species)) {
    # sum all observations per deployment and grouping variable
    n_table <-
      n_table %>%
      dplyr::mutate(
        begin = lubridate::floor_date(.data$date, unit = group_by)) %>%
      dplyr::group_by(.data$deploymentID, .data$begin) %>%
      dplyr::summarise_at(c("n_obs", "n_ind"), sum) %>%
      dplyr::ungroup()
  } else {
    # sum all observations per deployment, species and grouping variable
    n_table <-
      n_table %>%
      dplyr::mutate(
        begin = lubridate::floor_date(.data$date, unit = group_by)) %>%
      dplyr::group_by(.data$deploymentID, .data$scientificName, .data$begin) %>%
      dplyr::summarise_at(c("n_obs", "n_ind"), sum) %>%
      dplyr::ungroup()
  }
   
  # order result by deployments and follow same order as in deployments df
  deployments %>%
    dplyr::select("deploymentID") %>%
    dplyr::left_join(n_table, by = "deploymentID", multiple = "all")
}



## examples
library(camtraptor)
lapply(list.files("R", full.names = T), source)
data <- camtraptor::mica

obs_table <- n_observations(
  data, 
  species = c("martes foina", "vulpes vulpes"),
  group_by = "year")

effort_table <- get_custom_effort(
  data,
  group_by = "year",
  unit = "day")

# start RAI function from here:
# it should call both camtraptor::get_custom_effort, camtraptor::n_observations
# under the hood, and then perform a left_join and summarize on selected columns
rai_table <- left_join(obs_table, effort_table, by = c("deploymentID", "begin")) %>%
  group_by(begin, locationName, scientificName) %>%
  summarize_at(c("n_obs", "n_ind", "effort"), sum) %>%
  mutate(
    rai_obs = ifelse(effort > 0, n_obs/ effort, 0),
    rai_ind = ifelse(effort > 0, n_ind/ effort, 0))
