#' Get record table
#'
#' This function calculate the record table from a camera trap data package
#' and so tabulating species records. The record table is a concept developed
#' within the camtrapR package: see article
#' https://jniedballa.github.io/camtrapR/articles/camtrapr3.html. See also the
#' camtrapR's function documentation
#' [recordTable](https://jniedballa.github.io/camtrapR/reference/recordTable.html)
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param stationCol (character) name of the column containing stations.
#'   Default: `"locationName"`
#' @param exclude	(character) vector of species names (scientific names or
#'   vernacular names) to be excluded from the record table. Default: `NULL`
#' @param minDeltaTime (integer) time difference between records of the same
#'   species at the same station to be considered independent (in minutes).
#'   Default: 0
#' @param deltaTimeComparedTo (character) `"lastIndependentRecord"` or
#'   `"lastRecord"`. For two records to be considered independent, must the
#'   second one be at least `minDeltaTime` minutes after the last independent
#'   record of the same species (`deltaTimeComparedTo = "lastIndependentRecord"`
#'   ), or `minDeltaTime` minutes after the last record (
#'   `deltaTimeComparedTo = "lastRecord"`)? If `minDeltaTime` is 0,
#'   `deltaTimeComparedTo` must be NULL (deafult)
#' @param ... filter predicates for filtering on deployments
#' @importFrom dplyr .data %>% across arrange bind_cols distinct group_by last
#'   lag left_join mutate rename select starts_with ungroup
#' @importFrom assertthat assert_that
#' @importFrom lubridate date duration
#' @importFrom purrr map
#' @importFrom rlang !! := sym
#' @importFrom stringr str_starts
#' @importFrom tidyr nest replace_na unnest
#' @return A (tibble) data frame containing species records and additional
#'   information about stations, date, time and further metadata, such as
#'   filenames and directories of the images (media) linked to the species
#'   records. Some more details about the columns returned:
#'   1. `Station`: character, station names, as found in the deployment column
#'   defined  in argument `stationCol`
#'   2. `Species`: character, the scientific name of the observed species
#'   3. `DateTimeOriginal`: datetime object, as found in column `timestamp` of
#'   `observations`
#'   4. `Date`: date object, the date part of `DateTimeOriginal`
#'   5. `Time`: character, the time part of `DateTimeOriginal`
#'   6. `delta.time.secs`: numeric, the duration in seconds from the previous
#'   independent record of a given species at a certain location
#'   7. `delta.time.mins`: numeric, the duration in minutes from the previous
#'   independent record of a given species at a certain location
#'   8. `delta.time.hours`: numeric, the duration in hours from the previous
#'   independent record of a given species at a certain location
#'   9.  `delta.time.days`: numeric, the duration in days from the previous
#'   independent record of a given species at a certain location
#'   10. `Directory`: list, file paths of the images linked to the given record,
#'   as defined in column `filePath` of `media`
#'   11. `Filename`: list, file names of the images linked to the given record,
#'   as defined in column `fileName` of `media`
#' @export
#' @examples
#' get_record_table(mica)
#'
#' # set a minDeltaTime of 20 minutes from last independent record for filtering
#' # out not independent observations
#' get_record_table(mica,
#'     minDeltaTime = 20,
#'     deltaTimeComparedTo = "lastIndependentRecord")
#'
#' # set a minDeltaTime of 20 minutes from last record for filtering out not
#' # independent observations
#' get_record_table(mica,
#'     minDeltaTime = 20,
#'     deltaTimeComparedTo = "lastRecord")
#'
#' # exclude observations of brown rat
#' # exclude is case insensitive and vernacular names are allowed
#' get_record_table(mica, exclude = "wilde eend")
#'
#' # specify column to pass station names
#' get_record_table(mica,
#'     stationCol = "locationID",
#'     minDeltaTime = 20,
#'     deltaTimeComparedTo = "lastRecord")
#' # applying filter(s) on deployments, e.g. deployments with latitude >= 51.18
#' get_record_table(mica, pred_gte("latitude", 51.18))
get_record_table <- function(datapkg,
                             ...,
                             stationCol = "locationName",
                             exclude = NULL,
                             minDeltaTime = 0,
                             deltaTimeComparedTo = NULL) {
  # check data package
  check_datapkg(datapkg)

  # check stationCol is a valid column name
  assert_that(stationCol %in% names(datapkg$deployments),
              msg = glue("station column name (stationCol) not valid: ",
                         "it must be one of the deployments column names."))

  # check scientific names of species to be excluded
  if (!is.null(exclude)) {
    exclude <- check_species(datapkg, exclude, arg_name = "exclude")
  }

  # check minDeltaTime
  assert_that(is.numeric(minDeltaTime) & minDeltaTime >= 0,
              msg = "minDeltaTime must be a number greater or equal to 0")
  # minDeltaTime is set to an integer
  if (minDeltaTime != as.integer(minDeltaTime)) {
    minDeltaTime <- as.integer(minDeltaTime)
    message(glue("minDeltaTime has to be an integer. Set to {minDeltaTime}"))
  }

  # make a duration object out of minDeltaTime
  minDeltaTime_duration <- duration(minutes = minDeltaTime)

  # check deltaTimeComparedTo
  if (minDeltaTime > 0) {
    check_value(arg = deltaTimeComparedTo,
                options =  c("lastIndependentRecord", "lastRecord"),
                arg_name = "deltaTimeComparedTo",
                null_allowed = FALSE)
  }
  if (minDeltaTime == 0) {
    assert_that(is.null(deltaTimeComparedTo),
                msg = "minDeltaTime is 0: deltaTimeComparedTo must be NULL")
  }

  # remove observations of unidentified individuals
  obs <- datapkg$observations %>%
    filter(!is.na(.data$scientificName))

  # remove observations of species to be excluded
  obs <- obs %>%
    filter(!.data$scientificName %in% exclude)

  # apply filtering on deployments
  deployments <- apply_filter_predicate(
    df = datapkg$deployments,
    verbose = TRUE,
    ...)
  # remove observations from filtered out deployments
  obs <- obs %>%
    filter(.data$deploymentID %in% deployments$deploymentID)

  # add station column from deployments to observations
  obs <- obs %>%
    left_join(deployments %>% select(.data$deploymentID, !!sym(stationCol)),
              by = "deploymentID")
  # extract needed info from media and set file names and file paths as
  # lists for each sequence id
  grouped_media_info <-
    datapkg$media %>%
    select(.data$sequenceID,
           .data$filePath,
           .data$fileName,
           .data$timestamp) %>%
    group_by(.data$sequenceID) %>%
    summarise(filePath = list(.data$filePath),
              fileName = list(.data$fileName),
              # important if deltaTimeComparedTo is lastRecord
              last_timestamp = last(.data$timestamp))
  # add needed media info from media to observations
  obs <- obs %>%
    left_join(grouped_media_info,
              by = "sequenceID")

  # get record table
  record_table <- obs %>%
    mutate(Date = date(.data$timestamp),
           Time = format(.data$timestamp, format = "%H:%M:%S")) %>%
    group_by(.data$scientificName, !!sym(stationCol)) %>%
    arrange(.data$scientificName, !!sym(stationCol), .data$timestamp)
  if (minDeltaTime == 0) {
    # observations are by default independent
    record_table <- record_table %>%
      mutate(independent = TRUE)
  } else {
    # assess independence
    record_independence <- record_table %>%
      mutate(independent = FALSE) %>%
      nest() %>%
      mutate(data = map(.data$data,
                        assess_temporal_independence,
                        minDeltaTime_duration,
                        deltaTimeComparedTo))
    record_independence <- record_independence %>%
      unnest(cols = c(data))
    # add independence information to record_table
    record_table <- record_table %>%
      left_join(record_independence, by = c("scientificName",
                                            stationCol,
                                            "observationID"))
  }

  # remove not independent observations
  n_dependent_obs <- record_table %>%
    filter(.data$independent == FALSE) %>%
    nrow()
  if (n_dependent_obs > 0) {
    message(glue("Number of not independent observations to be removed: {n_dependent_obs}"))
    record_table <- record_table %>%
      filter(.data$independent == TRUE)
  }

  # get time between obs of two individuals of same species at same location
  record_table <- record_table %>%
    mutate(delta.time = .data$timestamp - lag(.data$timestamp)) %>%
    mutate(delta.time.secs = as.numeric(.data$delta.time)) %>%
    mutate(delta.time.mins = .data$delta.time.secs/60) %>%
    mutate(delta.time.hours = .data$delta.time.mins/60) %>%
    mutate(delta.time.days = .data$delta.time.hours/24) %>%
    mutate(across(starts_with("delta.time."), replace_na, 0)) %>%
    ungroup()

  record_table <- record_table %>%
    rename(Station := !! stationCol,
           Species = .data$scientificName,
           DateTimeOriginal = .data$timestamp,
           Directory = .data$filePath,
           FileName = .data$fileName) %>%
    select(.data$Station,
           .data$Species,
           .data$DateTimeOriginal,
           .data$Date,
           .data$Time,
           .data$delta.time.secs,
           .data$delta.time.mins,
           .data$delta.time.hours,
           .data$delta.time.days,
           .data$Directory,
           .data$FileName)
  return(record_table)
}

#' Assess temporal independence
#'
#' This function filters observations based on the temporal independence. It is
#' a helper function for `get_record_table()`.
#'
#' @param df a data.frame
#' @param minDeltaTime_dur: (duration) time difference between records of the same
#'   species at the same station to be considered independent
#' @param deltaTimeComparedTo: (character) `"lastIndependentRecord"` or
#'   `"lastRecord"`. For two records to be considered independent, must the
#'   second one be at least `minDeltaTime` minutes after the last independent
#'   record of the same species (`deltaTimeComparedTo = "lastIndependentRecord"`
#'   ), or `minDeltaTime` minutes after the last record (
#'   `deltaTimeComparedTo = "lastRecord"`)? If `minDeltaTime` is 0,
#'   `deltaTimeComparedTo` should be NULL
#' @keywords internal
#' @noRd
#' @noMd
assess_temporal_independence <- function(df, minDeltaTime_dur, deltaTimeComparedTo){

  # just initialization (set correctly at i = 1)
  last_indep_timestamp <- df$last_timestamp[1]

  for (i in 1:nrow(df)) {
    if (df$timestamp[i] > last_indep_timestamp | i == 1) {
      df$independent[i] <- TRUE
      if (deltaTimeComparedTo == "lastRecord") {
        last_indep_timestamp <- df$last_timestamp[i]
      } else {
        last_indep_timestamp <- df$timestamp[i]
      }
      last_indep_timestamp <- last_indep_timestamp + minDeltaTime_dur
    }
  }
  return(tibble(observationID = df$observationID,
                independent = df$independent))
}
