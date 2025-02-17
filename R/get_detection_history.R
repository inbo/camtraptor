#' Get the detection history of a species
#'
#' This function creates a detection history matrix for a species based on the record table and the camera operation matrix.
#' The detection history matrix is a binary matrix where rows represent camera stations and columns represent occasions.
#' The matrix is filled with 1s and 0s, where 1 indicates that the species was detected at a station on a given occasion and 0 indicates that the species was not detected.
#' The function also returns the effort matrix, which contains the number of days that each station was active on each occasion, and the dates matrix, which contains the dates of the occasions.
#' @param cam_op A matrix with camera operation data. Rows represent camera stations and columns represent occasions. The matrix should contain the number of days that each station was active on each occasion.
#' @param rec_table A data frame with the camera trap records. The data frame should contain the columns 'Station', 'Date', 'Species' and 'n'. 'Station' is the camera station ID, 'Date' is the date of the record, 'Species' is the species name, 'n' is the number of observations, and 'n_ind' is the number of individuals detected.
#' @param species Character. The species name.
#' @param output Character. The type of output. Choose one of: `"binary"`, `"n_observations"`, `"n_individuals"`.
#' @param occasion_length The length of the occasions in days. No decimals allowed. Default: 1.
#' @return A list with three elements:
#' - `detection_history`: the detection history matrix
#' - `effort`: the effort matrix
#' - `dates`: the dates matrix
#' 
#' @details
#' This function doesn't take as input a camera trap data package object, but a camera operation matrix and a record table, which are both calculated based on a camera trap data package object. For more information, see the [get_cam_op()] and [get_record_table()] functions.
#' 
#' @family camtrapR-derived functions
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' cam_op <- get_cam_op(mica)
#' rec_table <- get_record_table(mica)
#' get_detection_history(cam_op, rec_table, "Anas platyrhynchos", "binary")
get_detection_history <- function(cam_op,
                                  rec_table,
                                  species,
                                  output,
                                  occasion_length = 1) {
  # Check camera operation matrix, `cam_op`
  assertthat::assert_that(
    is.matrix(cam_op),
    msg = "`cam_op` must be a matrix."
  )
  # Check record table, `rec_table`
  assertthat::assert_that(is.data.frame(rec_table),
                          msg = "`rec_table` must be a tibble data.frame."
  )
  assertthat::assert_that(
    "tbl_df" %in% class(rec_table),
    msg = "`rec_table` must be a tibble data.frame."
  )
  assertthat::assert_that(
    all(c("Station", "Date", "Species", "n") %in% names(rec_table)),
    msg = paste0("Invalid record table. Must contain at least the columns: ",
                 "`Station`, `Date`, `Species` and `n`.")
  )
  # Check `species`
  species_in_rec_table <- rec_table %>% 
  dplyr::pull(.data$Species) %>%
    unique()
  assertthat::assert_that(
    rlang::is_string(species),
    msg = "`species` must be a character vector of lenght 1.")
  check_value(arg = species,
              options = species_in_rec_table,
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
  # Check occasion_length
  if (class(occasion_length) == "integer") {
    occasion_length <- as.numeric(occasion_length)
  }
  assertthat::assert_that(
    rlang::is_scalar_double(occasion_length),
    msg = "Invalid `occasion_length`. Must be an integer vector of length 1."
  )
  assertthat::assert_that(
    occasion_length > 0,
    msg = "Invalid `occasion_length`. Must be greater than 0."
  )
  assertthat::assert_that(
    round(occasion_length) == occasion_length,
    msg = "Invalid `occasion_length`. Must be an integer."
  )
  
  # Function to get the indices of non-NA values in a vector
  which_not_na <- function(x) which(!is.na(x))
  # Get the indices of non-NA values of the camera operation matrix for each
  # station
  cam_op_list <- apply(cam_op, 1, which_not_na)
  # Maximum number of occasions along all stations
  max_occasions <- max(purrr::map_dbl(cam_op_list, length))
  
  # Calculate the detection history information
  station_records <- rec_table %>% 
    dplyr::filter(.data$Species == species) %>%
    dplyr::group_by(.data$Station, .data$Date) %>%
    dplyr::summarize(z = 1,
                     n_obs = dplyr::n(),
                     n_ind = sum(.data$n),
                     .groups = "drop")
  
  # Create a list with all detection history information for each station
  det_hist_all <- purrr::imap(cam_op_list, function(x, y) {
    dplyr::left_join(
      data.frame(
        Station = y, 
        Date = lubridate::as_date(names(x))
      ),
      station_records %>%
        dplyr::filter(.data$Station == y),
      by = c("Station", "Date")
    ) %>%
      dplyr::mutate(across(c("z", "n_obs", "n_ind"), ~tidyr::replace_na(.x, 0)))
  })
  
  # Get detection history asked by user
  det_hist <- purrr::map(
    det_hist_all,
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
    })
  
  # Get effort for the dates of the occasions per station
  eff <- purrr::imap(cam_op_list, function(x, y) {
    eff <- cam_op[y, x]
    names(eff) <- NULL
    # Pad each effort vector with NAs to match the max number of occasions
    c(eff, rep(NA, max_occasions - length(eff)))
  })
  
  # Get dates of occasions per station
  dates <- purrr::map(
    names(cam_op_list),
    function(station) {
      d <- det_hist_all %>% 
        purrr::pluck(station) %>%
        dplyr::pull(.data$Date) %>%
        as.character.Date()
      # Pad the dates of occasions per station with NAs to match the max number of
      # occasions
      d <- c(d, rep(NA, max_occasions - length(d)))
    })
  
  # Return the detection history, effort and dates matrices
  list(detection_history = det_hist, effort = eff, dates = dates) %>%
    purrr::map(function(x) {
      # Transform the list of padded vectors into a matrix
      x <- do.call(rbind, x)
      # Assign the station names as rownames
      rownames(x) <- names(cam_op_list)
      x
      }
    )
}