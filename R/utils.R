#' Check input value against list of provided values
#'
#' Will return error message if an input value cannot be found in list of
#' provided values. NULL values can be allowed (default) or not by setting
#' parameter `null_allowed` equal to `TRUE` or `FALSE`.
#'
#' @param arg Character containing the input parameter provided by the user.
#' @param options Character vector of valid inputs for the parameter.
#' @param arg_name Character with the name of the parameter used in the function
#'   to test.
#' @param null_allowed Logical (`TRUE`, the default, or `FALSE`).
#'   Are NULL values allowed?
#' @returns If no error, `TRUE`.
#' @noRd
#' @keywords internal
#' @examples
#' # Valid inputs for species
#' check_value("Canis lupus", c("Canis lupus", "Corvus monedula"), "species")
#'
#' # Invalid inputs for species
#' values <- c(
#'   "Ans streperi", # wrong
#'   "Anas strepera",
#'   "wld end", # wrong
#'   "wilde eend"
#' )
#' check_value(values, c("Anas strepera", "wilde eend"), "species")
check_value <- function(arg, options = NULL, arg_name, null_allowed = TRUE) {
  max_print <- 20
  
  # Drop NA
  options <- options[!is.na(options)]
  
  # Wrong values
  wrong_values <- arg[!(arg %in% options)]
  
  # Suppress long messages with valid options
  if (length(options) > max_print) {
    options_to_print <- c(options[1:max_print], "others...")
  } else {
    options_to_print <- options
  }
  
  # Include NULL
  if (null_allowed) {
    options_to_print <- append(options_to_print, "NULL")
  } else if (is.null(wrong_values)) {
    wrong_values <- "NULL"
  }
  
  # Compose error message
  msg_to_print <- glue::glue(
    "Invalid value for {arg_name} parameter: ",
    glue::glue_collapse(wrong_values, sep = ", ", last = " and "),
    ".\nValid inputs are: ",
    glue::glue_collapse(options_to_print, sep = ", ", last = " and ")
  )
  
  # Provide user message
  if (!is.null(arg)) {
    assertthat::assert_that(
      all(arg %in% options),
      msg = msg_to_print
    )
  } else {
    assertthat::assert_that(
      null_allowed == TRUE,
      msg = msg_to_print
    )
  }
}

#' Check `group_time_by` parameter
#'
#' Checks if the `group_time_by` parameter is valid. If not, an error message is
#' returned. It is built on top of the `check_value()` function.
#' @param group_time_by Character containing the input parameter provided by the
#' user.
#' @param group_time_bys Character vector of valid inputs for the parameter.
#' @returns If no error, `TRUE`.
#' @noRd
#' @keywords internal
#' @examples
#' # Valid inputs for group_time_by
#' check_group_time_by("day", c("day", "week", "month"))
check_group_time_by <- function(group_time_by, group_time_bys) {
  assertthat::assert_that(
    is.null(group_time_by) | length(group_time_by) == 1,
    msg = "`group_time_by` must have length 1 or NULL."
  )
  check_value(
    group_time_by,
    group_time_bys,
    "group_time_by",
    null_allowed = TRUE
  )
}

#' Check a data frame is output of `summarize_observations()`.
#' 
#' @param grouped_df A grouped tibble data frame as returned by
#'   `summarize_observations()`.
#' @returns If no error, `TRUE`.
#' @noRd
#' @keywords internal
check_summary <- function(grouped_df) {
  # Check input is a data frame
  assertthat::assert_that(
    is.data.frame(grouped_df),
    msg = "The summary must be a data frame."
  )
  # Check input is a grouped data frame
  assertthat::assert_that(
    inherits(grouped_df, "grouped_df"),
    msg = "The summary must be a grouped data frame."
  )
  
  all_columns <- colnames(grouped_df)
  grouping_cols <- dplyr::group_vars(grouped_df)
  features <- setdiff(all_columns, grouping_cols)
  
  # Check the columns used for grouping are valid grouping cols
  wrong_grouping_cols <- setdiff(
    grouping_cols,
    c(.group_bys_deployments, .group_bys_observations, .group_time_bys)
  )
  assertthat::assert_that(
    length(wrong_grouping_cols) == 0,
    msg = glue::glue(
      "Invalid grouping columns in the summary: ",
      glue::glue_collapse(
        glue::backtick(wrong_grouping_cols), sep = ", ", last = " and "
      ),
      ".\n",
      "Valid deployment grouping columns: ",
      glue::glue_collapse(
        glue::backtick(
          c(.group_bys_deployments, .group_bys_observations, .group_time_bys)
        ),
        sep = ", ",
        last = " and "
      ),
      "."
    )
  )
  # Only one time grouping present
  time_grouping_cols <- intersect(grouping_cols, .group_time_bys)
  assertthat::assert_that(
    length(time_grouping_cols) <= 1,
    msg = glue::glue(
      "Only one time grouping column is allowed in the summary. ",
      "Found: ",
      glue::glue_collapse(
        glue::backtick(time_grouping_cols), sep = ", ", last = " and "
      ),
      "."
    )
  )
  
  # Check `grouped_df` contains the right features
  wrong_features <- setdiff(
    features,
    c(.features_observations, .features_deployments)
  )
  assertthat::assert_that(
    length(wrong_features) == 0,
    msg = glue::glue(
      "Invalid features in the summary: ",
      glue::glue_collapse(
        glue::backtick(wrong_features), sep = ", ", last = " and "
      ),
      ".\nValid features from `summarize_observations()`: ",
      glue::glue_collapse(
        glue::backtick(.features_observations), sep = ", ", last = " and "
      ),
      ".\nValid features from `summarize_deployments()`: ",
      glue::glue_collapse(
        glue::backtick(.features_deployments), sep = ", ", last = " and "
      ),
      "."
    )
  )
}
