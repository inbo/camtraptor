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
#' @param occasionLength Integer. The length of the occasions in days. No decimals
#'   allowed. Default: 1.
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
#' # Specify start date via `day1`
#' get_detection_history(
#'   recordTable,
#'   camOp,
#'   species = "Anas platyrhynchos",
#'   output = "binary",
#'   day1 = "2020-06-22"
#' )
get_detection_history <- function(recordTable,
                                  camOp,
                                  species,
                                  output,
                                  occasionLength = 1,
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
  # Check occasionLength
  assertthat::assert_that(
    rlang::is_scalar_integerish(occasionLength),
    msg = "Invalid `occasionLength`. Must be an integer vector of length 1."
  )
  assertthat::assert_that(
    occasionLength > 0,
    msg = "Invalid `occasionLength`. Must be greater than 0."
  )

  # Check `day1`
  assertthat::assert_that(
    rlang::is_string(day1),
    msg = "`day1` must be a character vector of length 1."
  )
  if (day1 != "station") {
    # `day1` must be equal to "station" or a string representing a valid
    # date in ISO 8601 format
    tryCatch(
      day1 <- as.character(as.Date(day1)), # Use custom error message
      error = function(e) {
        stop(paste0(
          "`day1` must be equal to 'station' or a string representing ",
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
      msg = paste0("`day1` must be a date lower or equal to the last date ",
                   "in the camera operation matrix."
      )
    )
    # If `day1` is a string representing a valid date check it's not before the
    # dates in camera operation matrix
    assertthat::assert_that(
      as.Date(day1) >= min(as.Date(colnames(camOp))),
      msg = paste0("`day1` must be a date greater or equal to the first date ",
                   "in the camera operation matrix."
      )
    )
  }
  
  # Check `buffer`
  assertthat::assert_that(
    is.null(buffer) | rlang::is_scalar_integerish(buffer),
    msg = paste0("Invalid `buffer`. If buffer is defined, ",
                 "it must be an integer of length 1.")
  )
  if (!is.null(buffer)) {
    assertthat::assert_that(
      buffer > 0,
      msg = "Invalid `buffer`. If `buffer` is defined, it must be 1 or higher."
    )
  }
  # For calculation it's better to have buffer = 0 instead of `NULL`
  if (is.null(buffer)) {
    buffer <- 0
  }

  # Remove records of other species
  recordTable <- recordTable %>% 
    dplyr::filter(.data$Species == species)
  # Total number of records in `recordTable`
  tot_records <- nrow(recordTable)
  
  # Remove columns (days) before `day1` if `day1` is a string representing a
  # valid date
  if (day1 != "station") {
    camOp <- camOp[, colnames(camOp) >= day1]
  }
  
  # Create a help data.frame with `Station` and first day
  periods_df <- dplyr::tibble(
    Station = rownames(camOp))
  # If `day1` is a string representing a valid date add it to the data.frame
  if (day1 != "station") {
    periods_df <- periods_df %>%
      dplyr::mutate(
        first_day = lubridate::as_date(day1)
      )
  } else {
    # If `day1` is "station" add the first day of each station + `buffer` (if
    # defined) to the data.frame.
    # Define first a function to get the indices of non-NA values in a vector
    which_not_na <- function(x) which(!is.na(x))
    periods_df <- periods_df %>%
      dplyr::mutate(
        first_day = lubridate::as_date(
          apply(camOp, 
                1,
                function(x) colnames(camOp)[which_not_na(x)[1]]
          )
        ) + lubridate::duration(buffer, units = "days")
      )
  }
  
  # Remove records in `recordTable` recorded before `day1` if `day1` is a string
  # representing a valid date
  if (day1 != "station") {
    records_to_remove <- recordTable %>%
      dplyr::filter(.data$Date < lubridate::as_date(day1))
    n_records_to_remove <- nrow(records_to_remove)
    if (nrow(records_to_remove) > 0) {
      warning(
        glue::glue(
          "{n_records_to_remove} record(s) (out of {tot_records}) are removed ",
          "because they were taken before `day1` ({day1}), e.g.:",
          "\n{records_to_remove$Station[1]}: {records_to_remove$Date[1]}."
        )
      )
    }
    recordTable <- recordTable %>%
      dplyr::filter(.data$Date >= lubridate::as_date(day1))
  }
  
  # Remove records in `recordTable` recorded during the `buffer` days.
  # No records will be removed in this step if buffer = 0
  records_to_remove <- recordTable %>%
    dplyr::left_join(periods_df, by = "Station") %>%
    dplyr::filter(.data$Date < .data$first_day)
  n_records_to_remove <- nrow(records_to_remove)
  assertthat::assert_that(
    n_records_to_remove < tot_records,
    msg = paste0(
      "In all stations, the occasions begin after retrieval. ",
      "Choose a smaller buffer argument."
    )
  )
  if (nrow(records_to_remove) > 0) {
    warning(
      glue::glue(
        "{n_records_to_remove} record(s) (out of {tot_records}) are removed ",
        "because they were taken during the buffer period of ",
        "{buffer} day(s), e.g.:",
        "\n{records_to_remove$Station[1]}: {records_to_remove$Date[1]}."
      )
    )
  }
  recordTable <- recordTable %>%
    dplyr::left_join(periods_df, by = "Station") %>%
    dplyr::filter(.data$Date >= .data$first_day)
  
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
