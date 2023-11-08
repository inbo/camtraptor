#' Get record table
#'
#' Calculates the record table from a camera trap data package and so tabulating
#' species records.
#' The record table is a concept developed within the camtrapR package, see
#' [this article](
#' https://jniedballa.github.io/camtrapR/articles/camtrapr3.html).
#' See also the function documentation for [camtrapR::recordTable()](
#' https://jniedballa.github.io/camtrapR/reference/recordTable.html).
#' **Note**: All dates and times are expressed in UTC format.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param stationCol Character name of the column containing stations.
#'   Default: `"locationName"`.
#' @param exclude Character vector of species names (scientific names or
#'   vernacular names) to be excluded from the record table.
#'   Default: `NULL`.
#' @param minDeltaTime Time difference between records of the same
#'   species at the same station to be considered independent (in minutes).
#'   Default: 0.
#' @param deltaTimeComparedTo One of `"lastIndependentRecord"` or
#'   `"lastRecord"`.
#'   For two records to be considered independent, the second one must be at
#'   least `minDeltaTime` minutes after the last independent record of the same
#'   species (`deltaTimeComparedTo = "lastIndependentRecord"`), or
#'   `minDeltaTime` minutes after the last record (`deltaTimeComparedTo =
#'   "lastRecord"`).
#'   If `minDeltaTime` is 0, `deltaTimeComparedTo` must be `NULL` (default).
#' @param removeDuplicateRecords Logical.
#'   If there are several records of the same species at the same station at
#'   exactly the same time, show only one?
#' @param datapkg Deprecated.
#'   Use `package` instead.
#' @param ... Filter predicates for filtering on deployments
#' @return A tibble data frame containing species records and additional
#'   information about stations, date, time and further metadata, such as
#'   filenames and directories of the images (media) linked to the species
#'   records.
#'   Some more details about the columns returned:
#'   - `Station`: Character, station names, as found in the deployment column
#'   defined in parameter `stationCol`.
#'   - `Species`: Character, the scientific name of the observed species.
#'   - `DateTimeOriginal`: Datetime object, as found in column `timestamp` of
#'   `observations`, in UTC format.
#'   - `Date`: Date object, the date part of `DateTimeOriginal`, in UTC format.
#'   - `Time`: Character, the time part of `DateTimeOriginal` in UTC format.
#'   - `delta.time.secs`: Numeric, the duration in seconds from the previous
#'   independent record of a given species at a certain location.
#'   - `delta.time.mins`: Numeric, the duration in minutes from the previous
#'   independent record of a given species at a certain location.
#'   - `delta.time.hours`: Numeric, the duration in hours from the previous
#'   independent record of a given species at a certain location.
#'   -  `delta.time.days`: Numeric, the duration in days from the previous
#'   independent record of a given species at a certain location.
#'   - `Directory`: List, file paths of the images linked to the given record,
#'   as defined in column `filePath` of `media`.
#'   - `Filename`: List, file names of the images linked to the given record,
#'   as defined in column `fileName` of `media`.
#' @family exploration functions
#' @importFrom dplyr .data %>%
#' @importFrom rlang !! :=
#' @export
#' @examples
#' get_record_table(mica)
#'
#' # Set a minDeltaTime of 20 minutes from last independent record for filtering
#' # out not independent observations
#' mica_dependent <- mica
#' mica_dependent$data$observations[4,"timestamp"] <- lubridate::as_datetime("2020-07-29 05:55:00")
#' get_record_table(
#'   mica_dependent,
#'   minDeltaTime = 20,
#'   deltaTimeComparedTo = "lastIndependentRecord"
#' )
#'
#' # Set a minDeltaTime of 20 minutes from last record for filtering out not
#' # independent observations
#' get_record_table(
#'   mica_dependent,
#'   minDeltaTime = 20,
#'   deltaTimeComparedTo = "lastRecord"
#' )
#'
#' # Exclude observations of mallard
#' # Exclude is case insensitive and vernacular names are allowed
#' get_record_table(mica, exclude = "wilde eend")
#'
#' # Specify column to pass station names
#' get_record_table(
#'   mica,
#'   stationCol = "locationID",
#'   minDeltaTime = 20,
#'   deltaTimeComparedTo = "lastRecord"
#' )
#' # Applying filter(s) on deployments, e.g. deployments with latitude >= 51.18
#' get_record_table(mica, pred_gte("latitude", 51.18))
get_record_table <- function(package = NULL,
                             ...,
                             stationCol = "locationName",
                             exclude = NULL,
                             minDeltaTime = 0,
                             deltaTimeComparedTo = NULL,
                             removeDuplicateRecords = TRUE,
                             datapkg = lifecycle::deprecated()) {
  # check data package
  check_package(package, datapkg, "get_record_table", media = TRUE)
  if (is.null(package) & !is.name(datapkg)) {
    package <- datapkg
  }
  
  # check stationCol is a valid column name
  assertthat::assert_that(
    stationCol %in% names(package$data$deployments),
    msg = glue::glue(
      "Station column name `{stationCol}` not valid: ",
      "It must be one of the deployments column names."
    )
  )

  # check scientific names of species to be excluded
  if (!is.null(exclude)) {
    exclude <- check_species(package, species = exclude, arg_name = "exclude")
  }

  # check minDeltaTime
  assertthat::assert_that(is.numeric(minDeltaTime) & minDeltaTime >= 0,
    msg = "`minDeltaTime` must be a number greater or equal to 0."
  )
  # minDeltaTime is set to an integer
  if (minDeltaTime != as.integer(minDeltaTime)) {
    minDeltaTime <- as.integer(minDeltaTime)
    message(glue::glue(
      "`minDeltaTime` has to be an integer. Set to `{minDeltaTime}`."
    ))
  }

  # make a duration object out of minDeltaTime
  minDeltaTime_duration <- lubridate::duration(minutes = minDeltaTime)

  # check deltaTimeComparedTo
  if (minDeltaTime > 0) {
    check_value(
      arg = deltaTimeComparedTo,
      options = c("lastIndependentRecord", "lastRecord"),
      arg_name = "deltaTimeComparedTo",
      null_allowed = FALSE
    )
  }
  if (minDeltaTime == 0) {
    assertthat::assert_that(is.null(deltaTimeComparedTo),
      msg = "minDeltaTime is 0: deltaTimeComparedTo must be NULL"
    )
  }

  assertthat::assert_that(
    is.logical(removeDuplicateRecords) & !is.na(removeDuplicateRecords),
    msg = "removeDuplicateRecords must be a logical: TRUE or FALSE."
  )

  # remove observations of unidentified individuals
  obs <- package$data$observations %>%
    dplyr::filter(!is.na(.data$scientificName))

  # remove observations of species to be excluded
  obs <- obs %>%
    dplyr::filter(!.data$scientificName %in% exclude)

  # apply filtering on deployments
  deployments <- apply_filter_predicate(
    df = package$data$deployments,
    verbose = TRUE,
    ...
  )
  # remove observations from filtered out deployments
  obs <- obs %>%
    dplyr::filter(.data$deploymentID %in% deployments$deploymentID)

  # add station column from deployments to observations
  obs <- obs %>%
    dplyr::left_join(deployments %>% 
                       dplyr::select("deploymentID", !!rlang::sym(stationCol)),
      by = "deploymentID"
    )
  # extract needed info from media and set file names and file paths as
  # lists for each sequence id
  grouped_media_info <-
    package$data$media %>%
    dplyr::select(
      "sequenceID",
      "filePath",
      "fileName",
      "timestamp"
    ) %>%
    dplyr::group_by(.data$sequenceID) %>%
    dplyr::summarise(
      filePath = list(.data$filePath),
      fileName = list(.data$fileName),
      # important if deltaTimeComparedTo is lastRecord
      last_timestamp = dplyr::last(.data$timestamp)
    )
  # add needed media info from media to observations
  obs <- obs %>%
    dplyr::left_join(grouped_media_info,
      by = "sequenceID"
    )

  # get record table
  record_table <-
    obs %>%
    dplyr::mutate(
      Date = lubridate::date(.data$timestamp),
      Time = format(.data$timestamp, format = "%H:%M:%S")
    ) %>%
    dplyr::group_by(.data$scientificName, !!rlang::sym(stationCol)) %>%
    dplyr::arrange(.data$scientificName, !!rlang::sym(stationCol), .data$timestamp)
  if (minDeltaTime == 0) {
    # observations are by default independent
    record_table <- record_table %>% dplyr::mutate(independent = TRUE)
  } else {
    # assess independence
    record_independence <- record_table %>%
      dplyr::mutate(independent = FALSE) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(
        .data$data,
        assess_temporal_independence,
        minDeltaTime_duration,
        deltaTimeComparedTo
      ))
    record_independence <- record_independence %>%
      tidyr::unnest(cols = c("data"))
    # add independence information to record_table
    record_table <- record_table %>%
      dplyr::left_join(record_independence,
        by = c("scientificName", stationCol, "observationID")
      )
  }

  # remove not independent observations
  n_dependent_obs <- record_table %>%
    dplyr::filter(.data$independent == FALSE) %>%
    nrow()
  if (n_dependent_obs > 0) {
    message(glue::glue(
      "Number of not independent observations to be removed: {n_dependent_obs}"
    ))
    record_table <- record_table %>%
      dplyr::filter(.data$independent == TRUE)
  }

  # get time between obs of two individuals of same species at same location
  record_table <- record_table %>%
    dplyr::mutate(delta.time = .data$timestamp - dplyr::lag(.data$timestamp)) %>%
    dplyr::mutate(delta.time.secs = as.numeric(.data$delta.time)) %>%
    dplyr::mutate(delta.time.mins = .data$delta.time.secs / 60) %>%
    dplyr::mutate(delta.time.hours = .data$delta.time.mins / 60) %>%
    dplyr::mutate(delta.time.days = .data$delta.time.hours / 24) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("delta.time."),
      .fns = function(x) tidyr::replace_na(x, 0)
    )) %>%
    dplyr::ungroup()

  record_table <- record_table %>%
    dplyr::rename(Station := !!stationCol,
      Species = "scientificName",
      DateTimeOriginal = "timestamp",
      Directory = "filePath",
      FileName = "fileName",
      n = "count"
    ) %>%
    dplyr::select(
      "Station",
      "Species",
      "n",
      "DateTimeOriginal",
      "Date",
      "Time",
      "delta.time.secs",
      "delta.time.mins",
      "delta.time.hours",
      "delta.time.days",
      "Directory",
      "FileName"
    )
  # remove duplicates if needed
  if (isTRUE(removeDuplicateRecords)) {
    record_table <- record_table %>%
      dplyr::group_by(
        .data$Station,
        .data$Species,
        .data$DateTimeOriginal,
        .data$Date,
        .data$Time,
        .data$Directory,
        .data$FileName
      ) %>%
      dplyr::mutate(row_number = dplyr::row_number()) %>%
      dplyr::filter(.data$delta.time.secs == max(.data$delta.time.secs) &
        .data$row_number == max(.data$row_number)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"row_number")
  }
  return(record_table)
}

#' Assess temporal independence
#'
#' Filters observations based on the temporal independence.
#' It is a helper function for `get_record_table()`.
#'
#' @param df A data frame.
#' @param minDeltaTime_dur: Duration, time difference between records of the same
#'   species at the same station to be considered independent.
#' @param deltaTimeComparedTo: Character, `"lastIndependentRecord"` or
#'   `"lastRecord"`.
#'   For two records to be considered independent, must the second one be at
#'   least `minDeltaTime` minutes after the last independent record of the same
#'   species (`deltaTimeComparedTo = "lastIndependentRecord"`), or
#'   `minDeltaTime` minutes after the last record (`deltaTimeComparedTo =
#'   "lastRecord"`)?
#'   If `minDeltaTime` is 0, `deltaTimeComparedTo` should be NULL.
#' @noRd
assess_temporal_independence <- function(df, minDeltaTime_dur, deltaTimeComparedTo) {

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
  return(dplyr::tibble(
    observationID = df$observationID,
    independent = df$independent
  ))
}
