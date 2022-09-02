#' Round coordinates to generalize camera trap locations
#'
#' Rounds deployment coordinates to a certain number of digits to fuzzy/
#' generalize camera trap locations.
#' This function can be used before publishing data in order to protect
#' sensitive species and/or prevent theft of active cameras.
#'
#' @param package A Camtrap DP, as read by [read_camtrap_dp()].
#' @param digits Number of decimal places to round coordinates to (`1`, `2` or
#'   `3`).
#' @return `package` with rounded coordinates as well as updated
#'   `coordinateUncertainty`.(in deployments) and `coordinatePrecision` (in
#'   metadata).
#' @family export functions
#' @export
#' @importFrom dplyr %>%
#' @section Details:
#' Rounding coordinates is a recommended method to generalize sensitive
#' biodiversity information (see
#' [Section 4.2](https://doi.org/10.15468/doc-5jp4-5g10#s-generalization)
#' in Chapman 2020).
#' Choose a number of digits that aligns with the sensitivity of the data and
#' notice the effect on precision and uncertainty.
#' Publish the coordinates as is (i.e. do not use this function) if the data
#' are not sensitive.
#'
#' sensitivity | digits | coordinatePrecision | coordinateUncertainty
#' --- | --- | --- | ---
#' high | 1 | 0.1 | 15,961 m + original uncertainty
#' medium | 2 | 0.01 | 1,570 m + original uncertainty
#' low | 3 | 0.001 | 157 m + original uncertainty
#'
#' For records with `coordinateUncertainty = NA` the function will assume the
#' coordinates were obtained by GPS and use `30 m` as original uncertainty,
#' before adding uncertainty caused by rounding.
#' @examples 
#' library(camtraptor)
#' # coordinate precision missing
#' mica$coordinatePrecision
#' # coordinateUncertainty of deployments missing (NA)
#' mica$data$deployments$coordinateUncertainty
#' # set precision to 3 digits
#' mica <- round_coordinates(mica)
#' # check new coordinatePrecision
#' mica$coordinatePrecision
#' # check new coordinateUncertainty
#' mica$data$deployments$coordinateUncertainty
round_coordinates <- function(package, digits = 3) {
  assertthat::assert_that(
    digits %in% c(1, 2, 3),
    msg = "`digits` must be 1, 2 or 3."
  )

  deployments <- package$data$deployments

  # Detect original number of digits from coordinatePrecision or data
  original_precision <- package$coordinatePrecision
  if (!is.null(original_precision)) {
    original_digits <- -log10(original_precision) # 0.001 -> 3
    assertthat::assert_that(
      digits <= original_digits, # 0.1 > 0.01
      msg = glue::glue(
        "Can't round from {original_digits} to {digits} digits.",
        "`{original_digits}` is derived from the `package$coordinatePrecision={original_precision}`.",
        .sep = "\n",
      )
    )
  } else {
    original_digits <- deployments %>%
      dplyr::mutate(
        longitude_digits = nchar(stringr::str_extract(longitude, "\\d+$"))
      ) %>%
      dplyr::summarize(max(longitude_digits)) %>%
      dplyr::pull()
    assertthat::assert_that(
      digits <= original_digits, # 0.1 > 0.01
      msg = glue::glue(
        "Can't round from {original_digits} to {digits} digits.",
        "`{original_digits}` is the maximum number of decimals for longitude in the data.",
        .sep = "\n",
      )
    )
  }

  # Set uncertainties
  uncertainty <- c(15961, 1570, 157) # In order for 1, 2, 3, digits

  # Update longitude, latitude and coordinateUncertainty
  package$data$deployments <-
    dplyr::mutate(deployments,
      longitude = round(longitude, digits),
      latitude = round(latitude, digits),
      coordinateUncertainty = dplyr::case_when(
        # No uncertainty in data: assume 30, add rounding uncertainty
        is.na(coordinateUncertainty) ~ 30 + uncertainty[digits],
        # No precision in metadata: original uncertainty, add rounding uncertainty
        is.null(original_precision) ~ coordinateUncertainty + uncertainty[digits],
        # Otherwise: subtract old rounding uncertainty, add new rounding uncertainty
        TRUE ~ coordinateUncertainty - uncertainty[original_digits] + uncertainty[digits]
      )
    )

  # Update coordinatePrecision
  package$coordinatePrecision <- 1/10^digits

  package
}
