#' Assess temporal independence
#'
#' Filters observations based on the temporal independence.
#' It is a helper function for `camtrapR_recordTable()`.
#'
#' @param df A data frame.
#' @param minDeltaTime_dur: Duration, time difference between records of the
#' same species at the same station to be considered independent.
#' @param deltaTimeComparedTo: Character, `"lastIndependentRecord"` or
#'   `"lastRecord"`.
#'   For two records to be considered independent, must the second one be at
#'   least `minDeltaTime` minutes after the last independent record of the same
#'   species (`deltaTimeComparedTo = "lastIndependentRecord"`), or
#'   `minDeltaTime` minutes after the last record (`deltaTimeComparedTo =
#'   "lastRecord"`)?
#'   If `minDeltaTime` is 0, `deltaTimeComparedTo` should be NULL.
#' @noRd
assess_temporal_independence <- function(
    df, minDeltaTime_dur, deltaTimeComparedTo) {
  # just initialization (set correctly at i = 1)
  last_indep_timestamp <- df$last_timestamp[1]
  event_start <- df$eventStart[1]
  for (i in 1:nrow(df)) {
    if (df$eventStart[i] > last_indep_timestamp | # independent
        # obs occurring at the same time (called "duplicate) but independent
        df$eventStart[i] == event_start
    ) {
      df$independent[i] <- TRUE
      event_start <- df$eventStart[i]
      if (deltaTimeComparedTo == "lastRecord") {
        last_indep_timestamp <- df$last_timestamp[i]
      } else {
        last_indep_timestamp <- df$eventStart[i]
      }
      last_indep_timestamp <- last_indep_timestamp + minDeltaTime_dur
    }
  }
  return(dplyr::tibble(
    observationID = df$observationID,
    independent = df$independent
  ))
}

#' Get record table
#'
#' Calculates the record table from a camera trap data package and so tabulating
#' species records. Only event-based observations and their corresponding media
#' are taken into account. The record table is a concept developed within the
#' camtrapR package, see [this article](
#' https://jniedballa.github.io/camtrapR/articles/camtrapr3.html). See also the
#' function documentation for [camtrapR::recordTable()](
#' https://jniedballa.github.io/camtrapR/reference/recordTable.html).
#' **Note**: All dates and times are expressed in UTC format.
#'
#' @param stationCol Character name of the column containing stations.
#'   Default: `"locationName"`.
#' @param exclude Character vector of scientific names to be excluded from the
#'   record table. Default: `NULL`.
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
#' @param removeDuplicateRecords Logical. If there are several records of the
#'   same species, but e.g. different `sex` or `lifeStage`, at the same station
#'   at exactly the same time, show only one? Default: `TRUE`. Duplicates are
#'   removed by keeping only the first observation in the observation table.
#' @inheritParams summarize_deployments
#' @return A tibble data frame containing species records and additional
#'   information about stations, date, time and further metadata, such as
#'   filenames and directories of the images (media) linked to the species
#'   records.
#'   Some more details about the columns returned:
#'   - `Station`: Character, station names, as found in the deployment column
#'   defined in parameter `stationCol`.
#'   - `Species`: Character, the scientific name of the observed species.
#'   - `n`: Numeric, the number of observed individuals (renamed from 
#'   [`count`](https://camtrap-dp.tdwg.org/data/#observations.count) in the 
#'   observations table).
#'   - `DateTimeOriginal`: Datetime object, as found in column `eventStart` of
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
#'   - `Latitude`: Numeric, latitude of the station, based on `deploymentID` of the observations.
#'   - `Longitude`: Numeric, longitude of the station, based on `deploymentID` of the observations.
#'   - `clock`: Numeric, clock time in radians.
#'   - `solar`: Numeric, solar time in radians. Calculated using `overlap::sunTime`, which essentially uses the approach described in [Nouvellet et al. (2012)](https://doi.org/10.1111/j.1469-7998.2011.00864.x).
#' @family camtrapR-derived functions
#' @export
#' @examples
#' library(lubridate)
#' 
#' x <- example_dataset()
#' camtrapR_recordTable(x)
#'
#' # Create a new camera trap data package with dependent observations only for
#' # demonstration.
#' obs <- observations(x)
#' obs[obs$observationID == "9e191d10",]$scientificName <- "Martes foina"
#' x_dep <- x
#' observations(x_dep) <- obs
#' 
#' # Set a minDeltaTime of 100 minutes from last record
#' camtrapR_recordTable(
#'   x_dep,
#'   minDeltaTime = 100,
#'   deltaTimeComparedTo = "lastRecord"
#' )
#' 
#' # Differences can occur between `deltaTimeCoparedTo` = `"lastRecord"` and
#' # `"lastIndependentRecord"`
#' obs <- observations(x)
#' obs[obs$eventID == "02ae9f43", "eventStart"] <- as_datetime("2020-08-02 05:10:20")
#' 
#' med <- media(x) 
#' rows_to_update <- which(med$eventID == "02ae9f43") 
#' med[rows_to_update, "timestamp"] <- as_datetime("2020-08-02 05:10:20") 
#' 
#' x_modified <- x
#' observations(x_modified) <- obs
#' media(x_modified) <- med
#' 
#' rec_last_indep <- camtrapR_recordTable(
#'   x_modified,
#'   minDeltaTime = 10,
#'   deltaTimeComparedTo = "lastIndependentRecord"
#' )
#' 
#' rec_last <- camtrapR_recordTable(
#'   x_modified,
#'   minDeltaTime = 10,
#'   deltaTimeComparedTo = "lastRecord"
#' )
#'
#' # Exclude observations of Anas platyrhynchos.
#' camtrapR_recordTable(x, exclude = "Anas platyrhynchos")
#'
#' # Specify column to pass station names
#' camtrapR_recordTable(x, stationCol = "locationID")
#'
#' # Include "duplicates", i.e. records of same species at same time, but
#' # different attributes, such as life stage or sex.
#' camtrapR_recordTable(
#'  x,
#'  removeDuplicateRecords = FALSE
#' )
camtrapR_recordTable <- function(x,
                                 stationCol = "locationName",
                                 exclude = NULL,
                                 minDeltaTime = 0,
                                 deltaTimeComparedTo = NULL,
                                 removeDuplicateRecords = TRUE) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)

  # Check `stationCol` is a valid column name
  assertthat::assert_that(
    stationCol %in% names(deployments(x)),
    msg = glue::glue(
      "Station column name `{stationCol}` not valid: ",
      "It must be one of the deployments column names."
    )
  )

  # Check scientific names of species to be excluded
  if (!is.null(exclude)) {
    all_taxa <- taxa(x)
    not_found <- exclude[!exclude %in% all_taxa$scientificName]
    assertthat::assert_that(
      all(exclude %in% all_taxa$scientificName),
      msg = glue::glue(
        "The following species in `exclude` argument are not present in the ",
        "camera trap data package: ",
        glue::glue_collapse(
          glue::backtick(not_found), sep = ", ", last = " and "
        ),
        "."
      )
    )
  }

  # Check `minDeltaTime`
  assertthat::assert_that(is.numeric(minDeltaTime) & minDeltaTime >= 0,
    msg = "`minDeltaTime` must be a number greater or equal to 0."
  )
  # `minDeltaTime` is set to an integer
  if (minDeltaTime != as.integer(minDeltaTime)) {
    minDeltaTime <- as.integer(minDeltaTime)
    message(glue::glue(
      "`minDeltaTime` has to be an integer. Set to `{minDeltaTime}`."
    ))
  }

  # Make a duration object out of `minDeltaTime`
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

  # Use event-based observations only
  x <- x %>%
    filter_observations(.data$observationLevel == "event")
  
  # Remove observations of unidentified individuals and species to be excluded
  x <- x %>%
    filter_observations(!is.na(scientificName),
                        !.data$scientificName %in% exclude)
  
  # Remove observations without `eventStart` and returns a warning message
  if (any(is.na(purrr::pluck(observations(x), "eventStart")))) {
    warning("Some observations have no `eventStart` and will be removed.")
    x <- x %>%
      filter_observations(!is.na(eventStart))
  }
  
  # Remove media without `timestamp` and returns a warning message
  if (any(is.na(purrr::pluck(media(x), "timestamp")))) {
    warning("Some media have no `timestamp` and will be removed.")
    x <- x %>%
      filter_media(!is.na(timestamp))
  }
  
  # Add coordinates to observations
  x <- add_coordinates(x)
  
  # Extract observations and deployments
  obs <- observations(x)
  deployments <- deployments(x)

  # Add station column from deployments to observations
  obs <- obs %>%
    dplyr::left_join(
      deployments %>%
        dplyr::select("deploymentID", !!rlang::sym(stationCol)),
      by = "deploymentID"
    )
  # Extract needed info from media and set file names and file paths as
  # lists for each sequence id
  grouped_media_info <-
    media(x) %>%
    dplyr::select(
      "eventID",
      "filePath",
      "fileName",
      "timestamp"
    ) %>%
    dplyr::group_by(.data$eventID) %>%
    dplyr::summarise(
      filePath = list(.data$filePath),
      fileName = list(.data$fileName),
      # important if deltaTimeComparedTo is lastRecord
      last_timestamp = dplyr::last(.data$timestamp)
    )
  # Add needed media info from media to observations
  obs <- obs %>%
    dplyr::left_join(
      grouped_media_info,
      by = "eventID"
    )

  # Get record table
  record_table <-
    obs %>%
    dplyr::mutate(
      Date = lubridate::date(.data$eventStart),
      Time = format(.data$eventStart, format = "%H:%M:%S")
    ) %>%
    dplyr::group_by(.data$scientificName, !!rlang::sym(stationCol)) %>%
    dplyr::arrange(
      .data$scientificName, !!rlang::sym(stationCol), .data$eventStart
    )
  if (minDeltaTime == 0) {
    # Observations are by default independent
    record_table <- record_table %>% dplyr::mutate(independent = TRUE)
  } else {
    # Assess independence
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
    # Add independence information to record_table
    record_table <- record_table %>%
      dplyr::left_join(record_independence,
        by = c("scientificName", stationCol, "observationID")
      )
  }

  # Remove not independent observations
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

  # Get time between obs of two individuals of same species at same location
  record_table <- record_table %>%
    dplyr::mutate(
      delta.time = .data$eventStart - dplyr::lag(.data$eventStart)
    ) %>%
    dplyr::mutate(delta.time.secs = as.numeric(.data$delta.time)) %>%
    dplyr::mutate(delta.time.mins = .data$delta.time.secs / 60) %>%
    dplyr::mutate(delta.time.hours = .data$delta.time.mins / 60) %>%
    dplyr::mutate(delta.time.days = .data$delta.time.hours / 24) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("delta.time."),
      .fns = function(x) tidyr::replace_na(x, 0)
    )) %>%
    dplyr::ungroup()

  # Add clock time in radians
  record_table <- record_table %>%
    dplyr::mutate(clock = activity::gettime(.data$eventStart))
  # Add solar time in radians
  matrix_coords <- matrix(c(record_table$longitude, record_table$latitude),
                          ncol = 2)
  record_table <- record_table %>%
    dplyr::mutate(solar = overlap::sunTime(
      clockTime = .data$clock,
      Dates = .data$eventStart,
      Coords = matrix_coords
      )
    )
  
  # Finalize `record_table`
  record_table <- record_table %>%
    dplyr::rename(Station := !!stationCol,
      Species = "scientificName",
      DateTimeOriginal = "eventStart",
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
      "FileName",
      "latitude",
      "longitude",
      "clock",
      "solar"
    )
  
  # Remove duplicates if needed
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
      dplyr::filter(.data$delta.time.secs == max(.data$delta.time.secs)) %>%
      dplyr::filter(.data$row_number == max(.data$row_number)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"row_number")
  }
  return(record_table)
}
