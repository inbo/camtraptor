#' Get the detection history of a species
#'
#' This function creates a detection history matrix for a species based on the
#' record table and the camera operation matrix. The detection history matrix is
#' a binary matrix where rows represent camera stations and columns represent
#' occasions. The matrix is filled with 1s and 0s, where 1 indicates that the
#' species was detected at a station on a given occasion and 0 indicates that
#' the species was not detected. The function also returns the effort matrix,
#' which contains the number of days that each station was active on each
#' occasion, and the dates matrix, which contains the dates of the occasions.
#' @param recordTable A data frame with the camera trap records. The data frame
#'   should contain the columns 'Station', 'Date', 'Species' and 'n'. 'Station'
#'   is the camera station ID, 'Date' is the date of the record, 'Species' is
#'   the species name, 'n' is the number of observations, and 'n_ind' is the
#'   number of individuals detected.
#' @param camOp A matrix with camera operation data. Rows represent camera
#'   stations and columns represent occasions. The matrix should contain the
#'   number of days that each station was active on each occasion.
#' @param species Character. The species name.
#' @param output Character. The type of output. Choose one of: `"binary"`,
#'   `"n_observations"`, `"n_individuals"`.
#' @param occasionLength Integer. The length of the occasions in days. No
#'   decimals allowed. Default: 1.
#' @param minActiveDaysPerOccasion Integer. Minimum number of active trap days
#'   for occasions to be included. Default: `NULL`. If used, it must be smaller
#'   than or equal to `occasionLength`.
#' @param maxNumberDays Integer. Maximum number of trap days per station.
#'   Default: `NULL`. If used, it must be greater than or equal to
#'   `occasionLength`.
#' @param day1 Character. Day occasions should begin: station setup date
#'   (`"station"`) or a specific date (e.g. `"2015-12-31"`). Default: "station".
#' @param buffer Integer. It makes the first occasion begin a number of days
#'   after station setup. `buffer` can be used only in combination with `day1` =
#'   `"station"`. Default: `NULL`. A warning is returned if some records are
#'   removed because taken during the buffer period.
#' @return A list with three elements:
#' - `detection_history`: the detection history matrix
#' - `effort`: the effort matrix
#' - `dates`: the dates matrix
#' 
#' @details
#' This function doesn't take as input a camera trap data package object, but a
#' camera operation matrix and a record table, which are both calculated based
#' on a camera trap data package object. For more information, see the
#' [get_camOp()] and [get_record_table()] functions.
#' 
#' @family camtrapR-derived functions
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' camOp <- get_cam_op(mica)
#' recordTable <- get_record_table(mica)
#' 
#' # Binary output
#' get_detection_history(
#'   recordTable,
#'   camOp,
#'   species = "Anas platyrhynchos",
#'   output = "binary"
#' )
#' 
#' # Number of observations output
#' get_detection_history(
#'  recordTable,
#'  camOp,
#'  species = "Anas platyrhynchos",
#'  output = "n_observations"
#' )
#'  
#' # Number of individuals output
#' get_detection_history(
#'  recordTable,
#'  camOp,
#'  species = "Anas platyrhynchos",
#'  output = "n_individuals"
#' )
#' 
#' # Occasion length of 7 days
#' get_detection_history(
#'  recordTable,
#'  camOp,
#'  species = "Anas platyrhynchos",
#'  output = "n_individuals",
#'  occasionLength = 7
#' )
#' 
#' # Use a `minActiveDaysPerOccasion` of 5 days
#' get_detection_history(
#'  recordTable,
#'  camOp,
#'  species = "Anas platyrhynchos",
#'  output = "n_individuals",
#'  occasionLength = 7,
#'  minActiveDaysPerOccasion = 5
#' )
#' 
#' # Use a `maxNumberDays` of 5 days
#' get_detection_history(
#'  recordTable,
#'  camOp,
#'  species = "Anas platyrhynchos",
#'  output = "n_individuals",
#'  maxNumberDays = 5
#' )
#' 
#' # Specify start date via `day1`
#' get_detection_history(
#'   recordTable,
#'   camOp,
#'   species = "Anas platyrhynchos",
#'   output = "binary",
#'   day1 = "2020-06-22"
#' )
#' 
#' # Use a `buffer` of 2 days
#' get_detection_history(
#'  recordTable,
#'  camOp,
#'  species = "Anas platyrhynchos",
#'  output = "n_individuals",
#'  buffer = 2
#' )
get_detection_history <- function(recordTable,
                                  camOp,
                                  species,
                                  output,
                                  occasionLength = 1,
                                  minActiveDaysPerOccasion = NULL,
                                  maxNumberDays = NULL,
                                  day1 = "station",
                                  buffer = NULL) {
  # Check camera operation matrix, `camOp`
  assertthat::assert_that(
    is.matrix(camOp),
    msg = "`camOp` must be a matrix."
  )
  # Check record table, `recordTable`
  assertthat::assert_that(is.data.frame(recordTable),
                          msg = "`recordTable` must be a tibble data.frame."
  )
  assertthat::assert_that(
    "tbl_df" %in% class(recordTable),
    msg = "`recordTable` must be a tibble data.frame."
  )
  assertthat::assert_that(
    all(c("Station", "Date", "Species", "n") %in% names(recordTable)),
    msg = paste0("Invalid record table. Must contain at least the columns: ",
                 "`Station`, `Date`, `Species` and `n`.")
  )
  # Check `species`
  species_in_recordTable <- recordTable %>% 
  dplyr::pull(.data$Species) %>%
    unique()
  assertthat::assert_that(
    rlang::is_string(species),
    msg = "`species` must be a character vector of lenght 1.")
  check_value(arg = species,
              options = species_in_recordTable,
              arg_name = "species",
              null_allowed = FALSE
  )
  # Check `output`
  assertthat::assert_that(
    rlang::is_string(output),
    msg = "`output` must be a character vector of lenght 1."
  )
  check_value(arg = output,
              options = c("binary", "n_observations", "n_individuals"),
              arg_name = "output",
              null_allowed = FALSE
  )
  # Check `occasionLength`
  assertthat::assert_that(
    rlang::is_scalar_integerish(occasionLength),
    msg = "Invalid `occasionLength`. Must be an integer vector of length 1."
  )
  assertthat::assert_that(
    occasionLength > 0,
    msg = "Invalid `occasionLength`. Must be greater than 0."
  )
  # Check `minActiveDaysPerOccasion`
  assertthat::assert_that(
    is.null(minActiveDaysPerOccasion) | 
      rlang::is_scalar_integerish(minActiveDaysPerOccasion),
    msg = paste0("Invalid `minActiveDaysPerOccasion`. If defined, it must be ",
                 "an integer vector of length 1."
    )
  )
  if (!is.null(minActiveDaysPerOccasion)) {
    assertthat::assert_that(
      minActiveDaysPerOccasion > 0,
      msg = paste0("Invalid `minActiveDaysPerOccasion`. If defined, it must ",
                   "be greater than 0."
      )
    )
    assertthat::assert_that(
      minActiveDaysPerOccasion <= occasionLength,
      msg = paste0("Invalid `minActiveDaysPerOccasion`. If defined, ", 
                   "it must be smaller than or equal to `occasionLength`."
      )
    )
  }
  # Check `maxNumberDays`
  assertthat::assert_that(
    is.null(maxNumberDays) | rlang::is_scalar_integerish(maxNumberDays),
    msg = paste0("Invalid `maxNumberDays`. If defined, it must be an integer ",
                 "vector of length 1."),
    fixed = TRUE
  )
  if (!is.null(maxNumberDays)) {
    assertthat::assert_that(
      maxNumberDays > 0,
      msg = "Invalid `maxNumberDays`. Must be greater than 0."
    )
    assertthat::assert_that(
      maxNumberDays >= occasionLength,
      msg = paste0(
        "Invalid `maxNumberDays`. If defined, it must be greater than or ",
        "equal to `occasionLength`."
      )
    )
    assertthat::assert_that(
      maxNumberDays <= ncol(camOp),
      msg = paste0(
        "Invalid `maxNumberDays`. Must be smaller than or equal to the number ",
        "of columns of `camOp`."
      )
    )
  }
  # Check `day1`
  assertthat::assert_that(
    rlang::is_string(day1),
    msg = "Invalid `day1`. Must be a character vector of length 1."
  )
  if (day1 != "station") {
    # `day1` must be equal to "station" or a string representing a valid
    # date in ISO 8601 format
    tryCatch(
      day1 <- as.character(as.Date(day1)), # Use custom error message
      error = function(e) {
        stop(paste0(
          "Invalid `day1`. Must be equal to 'station' or a string representing ",
          "a valid date in ISO 8601 format."
          ),
          call. = FALSE
        )
      }
    )
    # If `day1` is a string representing a valid date check it's not after the
    # dates in camera operation matrix
    assertthat::assert_that(
      as.Date(day1) <= max(as.Date(colnames(camOp))),
      msg = paste0(
        "Invalid `day1`. Must be a date lower or equal to the last date ",
        "in the camera operation matrix."
      )
    )
    # If `day1` is a string representing a valid date check it's not before the
    # dates in camera operation matrix
    assertthat::assert_that(
      as.Date(day1) >= min(as.Date(colnames(camOp))),
      msg = paste0(
        "Invalid `day1`. Must be a date greater or equal to the first date ",
        "in the camera operation matrix."
      )
    )
  }
  # Check `buffer`
  assertthat::assert_that(
    is.null(buffer) | rlang::is_scalar_integerish(buffer),
    msg = "Invalid `buffer`. If defined, it must be an integer of length 1."
  )
  if (!is.null(buffer)) {
    assertthat::assert_that(
      buffer > 0,
      msg = "Invalid `buffer`. If defined, it must be 1 or higher."
    )
  }
  # For calculation it's better to have buffer = 0 instead of `NULL`
  if (is.null(buffer)) {
    buffer <- 0
  }

  # Total number of records of given species in `recordTable`
  tot_records <- recordTable %>%
    dplyr::filter(.data$Species == species) %>%
    nrow()
  
  # Create a help data.frame with `Station`, first day and last day.
  # Define first a function to get the indices of non-NA values in a vector
  which_not_na <- function(x) which(!is.na(x))
  periods_df <- dplyr::tibble(
    Station = rownames(camOp))
  if (day1 != "station") {
    # If `day1` is a string representing a valid date, add it as first day.
    periods_df <- periods_df %>%
      dplyr::mutate(
        first_day = lubridate::as_date(day1)
      )
  } else {
    # If `day1` is "station" add the first day of each station + `buffer` (if
    # defined) to the data.frame.
    periods_df <- periods_df %>%
      dplyr::mutate(
        first_day = lubridate::as_date(
          apply(
            camOp, 
            1,
            function(x) colnames(camOp)[which_not_na(x)[1]]
          )
        ) + lubridate::duration(buffer, units = "days")
      )
  }
  # Add last_day to `periods_df`
  periods_df <- periods_df  %>%
    dplyr::mutate(
      last_day = lubridate::as_date(
        apply(
          camOp, 
          1,
          function(x) {
            cam_op_row_without_na <- which_not_na(x)
            colnames(camOp)[cam_op_row_without_na[length(cam_op_row_without_na)]]
          }
        )
      )
    )
  # Notice that `maxNumberDays` counts from first day of each station: `buffer`
  # is not taken into account. Set `last_day` based on `maxNumberDays` if
  # defined.
  if (!is.null(maxNumberDays)) {
    periods_df <- periods_df %>%
      dplyr::mutate(
        last_day_max_number_days = lubridate::as_date(
          apply(camOp, 1, function(x) colnames(camOp)[which_not_na(x)[1]])
          ) +
          lubridate::duration(maxNumberDays - 1, units = "days")
        ) %>%
      dplyr::mutate(last_day = dplyr::if_else(
        .data$last_day > .data$last_day_max_number_days,
        .data$last_day_max_number_days,
        .data$last_day
      )) %>%
      dplyr::select(-"last_day_max_number_days")
  }
  # Check that `buffer` is not too big and/or `maxNumberDays` is not too small:
  # `first_day` must be <= `last_day` for at least one station. We have already
  # checked that day1 is <= latest available date in `camOp` so it can be an
  # issue due to the buffer only, `buffer`- `maxNumberDays` combination or
  # `day1` -  `maxNumberDays` combination.
  assertthat::assert_that(
    any(periods_df$first_day <= periods_df$last_day),
    msg = if (is.null(maxNumberDays) & (day1 == "station")) {
      paste0(
      "In all stations, the occasions begin after retrieval. ",
      "Choose a smaller `buffer` argument.")
    } else {
      # `day1` is a string representing a valid date and `maxNumberDays` is
      # defined
      if (day1 != "station") {
        paste0(
          "In all stations, the occasions begin after retrieval. ",
          "Choose an earlier `day1` date or a larger `maxNumberDays` argument.")
      } else {
        # `day1` = "station" and `maxNumberDays` is defined
        paste0(
          "In all stations, the occasions begin after retrieval. ",
          "Choose a smaller `buffer` argument or ",
          "a larger `maxNumberDays` argument.")
      }
    }
  )
  
  # Check `recordTable`: the warnings/errors communicate only about records of
  # the given species.
  tot_records <- recordTable %>%
    dplyr::filter(.data$Species == species)
  n_tot_records <- nrow(tot_records)
  # Return warning/error if some/all records are before `day1` if `day1` is a
  # string representing a valid date.
  if (day1 != "station") {
    records_to_remove <- tot_records %>%
      dplyr::filter(.data$Date < lubridate::as_date(day1))
  } else {
    records_to_remove <- tot_records %>%
      dplyr::left_join(periods_df, by = "Station") %>%
      dplyr::filter(.data$Date < .data$first_day)
  }
  n_records_to_remove <- nrow(records_to_remove)
  if (n_records_to_remove > 0) {
    assertthat::assert_that(
      n_records_to_remove < n_tot_records,
      msg = paste0("No more records after removing records before survey ",
                   "begin. The detection history would be empty."
      )
    )
    if (day1 != "station") {
      warning(
        glue::glue(
          "{n_records_to_remove} record(s) (out of {n_tot_records}) are ",
          "removed because they were taken before `day1` ({day1}), e.g.:",
          "\n{records_to_remove$Station[1]}: {records_to_remove$Date[1]}."
        )
      )
    } else {
      warning(
        glue::glue(
          "{n_records_to_remove} record(s) (out of {n_tot_records}) are ",
          "removed because they were taken during the buffer period of ",
          "{buffer} day(s), e.g.:",
          "\n{records_to_remove$Station[1]}: ",
          "{records_to_remove$Date[1]}."
        )
      )
    }
  }
  
  # Return warnings/errors if some/all records are after `maxNumberDays`
  if (!is.null(maxNumberDays)) {
    records_to_remove <- tot_records %>%
      dplyr::left_join(periods_df, by = "Station") %>%
      dplyr::filter(.data$Date > .data$last_day)
    n_records_to_remove <- nrow(records_to_remove)
    if (n_records_to_remove > 0) {
      assertthat::assert_that(
        n_records_to_remove < n_tot_records,
        msg = glue::glue(
          "All records removed because they are ",
          "taken after `maxNumberDays` ({maxNumberDays} days). ",
          "The detection history would be empty."
        )
      )
      warning(
          glue::glue(
            "{n_records_to_remove} record(s) (out of {n_tot_records}) are ",
            "removed because they were taken after `maxNumberDays` ",
            "({maxNumberDays} days) the first day of each station, e.g.:",
            "\n{records_to_remove$Station[1]}: {records_to_remove$Date[1]}."
          )
      )
    }
  }
  
  # Add first_day and last_day columns to `recordTable`
  recordTable <- recordTable %>%
    dplyr::left_join(periods_df, by = "Station") %>%
    dplyr::mutate(Date = lubridate::as_date(.data$Date))
  
  # Remove records before `first_day`
  recordTable <- recordTable %>% dplyr::filter(.data$Date >= .data$first_day)
  
  # Remove records after `maxNumberDays` (if `maxNumberDays` is defined)
  if (!is.null(maxNumberDays)) {
    recordTable <- recordTable %>% dplyr::filter(.data$Date <= .data$last_day)
  }
  # Remove records of other species
  recordTable <- recordTable %>% 
    dplyr::filter(.data$Species == species)
  
  # Check that `recordTable` is not empty
  assertthat::assert_that(
    nrow(recordTable) > 0,
    msg = glue::glue(
      "All records removed. The detection history would be empty. ",
      "Check that `recordTable` contains records of the species ",
      "and that the dates are within the range of the camera ",
      "operation matrix. Check also the comibnation of arguments ",
      "`buffer` ({buffer}), `day1` ({day1}) and ",
      "`maxNumberDays` ({maxNumberDays})."
    )
  )
  
  # Remove columns (days) before `day1` if `day1` is a string representing a
  # valid date (if `day1` = "station" we will do it later in the code)
  if (day1 != "station") {
    camOp <- camOp[, colnames(camOp) >= day1]
  }
  
  # Calculate the detection history information for each station
  station_records <- recordTable %>%
    dplyr::mutate(period_start = 
      .data$first_day + 
        occasionLength * 
        floor(as.numeric(.data$Date - .data$first_day)/occasionLength)
    ) %>%
    dplyr::group_by(.data$Station, period_start) %>%
    dplyr::summarize(z = 1,
                     n_obs = dplyr::n(),
                     n_ind = sum(.data$n),
                     .groups = "drop")
  
  # Transform camera operation matrix, `camOp` to a long format (tibble)
  camOp_long <- dplyr::as_tibble(
    reshape2::melt(camOp,
                   varnames = c("Station", "Date"),
                   value.name = "Effort")
  )
  
  camOp_long <- camOp_long %>%
    dplyr::left_join(
      periods_df,
      by = "Station"
    ) %>%
    dplyr::mutate(Date = lubridate::as_date(Date))
  
  # Apply `maxNumberDays` to `camOp_long` by comparing `last_day` with `Date`
  camOp_long <- camOp_long %>%
    dplyr::mutate(Effort = dplyr::if_else(.data$Date > .data$last_day,
                                          NA_real_,
                                          .data$Effort))
  
  # Group by `occasionLength` days and calculate the sum of effort for each
  # station and period
  camOp_long_grouped <- camOp_long %>%
    dplyr::mutate(period_start = 
                    .data$first_day + 
                    occasionLength * 
                    floor(
                      as.numeric(.data$Date - .data$first_day)/occasionLength
                    )
    ) %>%
    dplyr::group_by(Station, period_start) %>%
    dplyr::summarise(
      Effort = ifelse(
        all(is.na(.data$Effort)), NA, sum(.data$Effort, na.rm = TRUE)
      ),
      .groups = "drop")
  
  # Maximum number of occasions along all stations, i.e. the maximum number of
  # rows with effort not NA over all stations. If buffer is not 0 remove buffer
  # days as well.
  max_occasions <- camOp_long_grouped %>%
    dplyr::filter(!is.na(.data$Effort)) %>%
    # Add `first_day` column to take buffer days into account
    dplyr::left_join(periods_df, by = "Station") %>%
    # Do not count buffer days
    dplyr::filter(.data$period_start >= .data$first_day) %>%
    dplyr::group_by(.data$Station) %>%
    dplyr::summarise(
      max_occasions = max(dplyr::row_number()),
      .groups = "drop") %>%
    dplyr::pull(max_occasions)
  max_occasions <- max(max_occasions)
  
  # Calculate a global detection history information for all stations
  det_hist_all_info <- dplyr::left_join(
    camOp_long_grouped,
    station_records,
    by = c("Station", "period_start")) %>%
    dplyr::mutate(across(c("z", "n_obs", "n_ind"), ~tidyr::replace_na(.x, 0)))
  
  # Create a list with all detection history information for each station
  stations <- unique(det_hist_all_info$Station)
  det_hist_list <- purrr::map(stations, function(x) {
    det_hist_all_info %>%
      dplyr::filter(.data$Station == x) %>%
      dplyr::filter(!is.na(.data$Effort)) %>%
      # Add `first_day` column to take buffer days into account
      dplyr::left_join(periods_df, by = "Station") %>%
      # Do not count buffer days
      dplyr::filter(.data$period_start >= .data$first_day) %>%
      dplyr::select("period_start", "Effort", "z", "n_obs", "n_ind")
  })
  names(det_hist_list) <- stations
  
  # Remove occasions with less than `minActiveDaysPerOccasion` active days
  det_hist_list <- purrr::map(
    det_hist_list,
    function(x) {
      if (!is.null(minActiveDaysPerOccasion)) {
        x <- x %>%
          dplyr::filter(.data$Effort >= minActiveDaysPerOccasion)
      }
      x
    }
  )
  
  # Get detection history asked by user in the form of a list of vectors
  det_hist <- purrr::map(
    det_hist_list,
    function(x) {
      if (output == "binary") {
        dh <- x %>%
          dplyr::pull(z)
      } else if (output == "n_observations") {
        dh <- x %>%
          dplyr::pull("n_obs")
      } else if (output == "n_individuals") {
        dh <- x %>%
          dplyr::pull("n_ind")
      }
      # Pad the detection history with NAs to match the max number of occasions
      c(dh, rep(NA, max_occasions - length(dh)))
    }
  )
  
  # Get effort for the dates of the occasions per station
  eff <- purrr::map(det_hist_list, function(x) {
    e <- x %>%
      dplyr::pull("Effort")
    # Pad each effort vector, `e`, with NAs to match the max number of occasions
    c(e, rep(NA, max_occasions - length(e)))
    }
  )

  # Get dates of occasions per station
  dates <- purrr::map(det_hist_list, function(x) {
      d <- x %>%
        dplyr::rename("Date" = "period_start") %>%
        dplyr::pull(.data$Date) %>%
        as.character.Date()
      # Pad the dates of occasions per station with NAs to match the max number
      # of occasions
      d <- c(d, rep(NA, max_occasions - length(d)))
    }
  )
  
  # Return the detection history, effort and dates matrices as a list of
  # matrices
  list(detection_history = det_hist, effort = eff, dates = dates) %>%
    purrr::map(function(x) {
      # Transform the list of padded vectors into a matrix
      x <- do.call(rbind, x)
      # Assign the station names as rownames
      rownames(x) <- stations
      # Assign progressive number of occasions as column names. Use prefix "o".
      colnames(x) <- paste0("o", seq_len(ncol(x)))
      x
      }
    )
}
