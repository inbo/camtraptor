#' Get the detection history of a species
#'
#' This function creates a detection history matrix for a species from a camera trap dataset.
#' The detection history matrix is a binary matrix where rows represent camera stations and columns represent occasions.
#' The matrix is filled with 1s and 0s, where 1 indicates that the species was detected at a station on a given occasion and 0 indicates that the species was not detected.
#' The function also returns the effort matrix, which contains the number of days that each station was active on each occasion, and the dates matrix, which contains the dates of the occasions.
#' @param cam_op A matrix with camera operation data. Rows represent camera stations and columns represent occasions. The matrix should contain the number of days that each station was active on each occasion.
#' @param rec_table A data frame with the camera trap records. The data frame should contain the columns 'Station', 'Date', 'Species', 'n', and 'n_ind'. 'Station' is the camera station ID, 'Date' is the date of the record, 'Species' is the species name, 'n' is the number of observations, and 'n_ind' is the number of individuals detected.
#' @param species The species name.
#' @param output The type of output. Choose one of: 'binary', 'n_observations', 'n_individuals'.
#' @param occasion_length The length of the occasions in days. Default: 1.
#' @return A list with three elements: 'detection_history' is the detection history matrix, 'effort' is the effort matrix, and 'dates' is the dates matrix.
#' @family exploration functions
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' cam_op <- get_cam_op(mica)
#' rec_table <- get_record_table(mica)
#' get_detection_history(cam_op, rec_table, "Anas platyrhynchos", "binary")
get_detection_history <- function(cam_op, rec_table, species, output, occasion_length = 1) {
  # Check camera operation matrix, `cam_op`
  assertthat::assert_that(is.matrix(cam_op))
  # Check record table, `rec_table`
  assertthat::assert_that(is.data.frame(rec_table))
  assertthat::assert_that(
    all(c("Station", "Date", "Species", "n", "n_ind") %in% colnames(rec_table)),
    msg = paste0("Invalid record table. Must contain at least the columns: ",
                 "`Station`, `Date`, `Species`, `n`, and `n_ind`.")
  )
  assertthat::assert_that(is.character(species))
  # Check output
  assertthat::assert_that(is.character(output))
  assertthat::assert_that(output %in% c("binary", "n_observations", "n_individuals"),
                          msg = "Invalid output. Choose one of: `binary`, `n_observations`, `n_individuals`")
  # Check occasion_length
  assertthat::assert_that(is.numeric(occasion_length), msg = "Invalid `occasion_length`. Must be numeric.")
  assertthat::assert_that(occasion_length > 0, msg = "Invalid `occasion_length`. Must be greater than 0.")
  assertthat::assert_that(round(occasion_length) == occasion_length, msg = "Invalid `occasion_length`. Must be an integer."))
  
  which_not_na <- function(x) which(!is.na(x))
  cam_op_list <- apply(cam_op, 1, which_not_na)
  max_occasions <- max(sapply(cam_op_list, length))
  dh <- eff <- dates <- matrix(data = NA, nrow = nrow(cam_op), ncol = max_occasions)
  station_records <- rec_table %>% 
    dplyr::filter(.data$Species == species) %>%
    dplyr::group_by(.data$Station, .data$Date) %>%
    dplyr::summarize(z = 1, n_obs = n(), n_ind = sum(n), .groups = "keep")
  
  for (i in 1:nrow(cam_op)) {
    n_occasions <- length(cam_op_list[[i]])
    dat <- dplyr::left_join(
      data.frame(
        Station = names(cam_op_list)[i], 
        Date = as.Date(names(cam_op_list[[i]]))
      ),
      station_records %>%
        dplyr::filter(.data$Station == names(cam_op_list)[i]),
      by = c("Station", "Date")
    )
    
    # Detection matrix
    if (output == "binary") {  
      # Species detected at least once
      z <- dat %>% dplyr::pull(.data$z)
      z[is.na(z)] <- 0
      dh[i, 1:n_occasions] <- z
    } 
    else if (output == "n_observations") {  # Number of species observations
      n_obs <- dat %>% dplyr::pull(.data$n_obs)
      n_obs[is.na(n_obs)] <- 0
      dh[i, 1:n_occasions] <- n_obs
    }
    else if (output == "n_individuals") {  # number of individuals detected
      n_ind <- dat %>% dplyr::pull(n_ind)
      n_ind[is.na(n_ind)] <- 0
      dh[i, 1:n_occasions] <- n_ind
    }
    
    # effort in days at each occasion
    eff[i, 1:n_occasions] <- cam_op[i, c(cam_op_list[[i]])]
    
    # dates of the occasions in the detection history
    d <- pull(dat, Date)
    dates[i, 1:n_occasions] <- as.character.Date(d)
  }
  rownames(dh) <- rownames(eff) <- rownames(dates) <- names(cam_op_list)
  return(list(detection_history = dh, effort = eff, dates = dates))
}