#' Visualize temporal coverage of the datapackage
#'
#' This function visualizes the temporal coverage of the datapackage on a figure
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param yaxis Variable to show on the y-axis, typically one of `locationName`,
#'   `deployementID`.
#' @return ggplot figure
#' @family visualization functions
#' @importFrom dplyr .data %>%
#' @export
plot_time <- function(package = NULL, yaxis = "locationName") {
  
  assertthat::assert_that(
    yaxis %in% names(package$data$deployments),
    msg = "`yaxis` must be a column of deployements"
  )
  
  obs <- dplyr::left_join(
    package$data$observations,
    package$data$deployments,
    by = "deploymentID"
  ) %>%
    dplyr::group_by(!!dplyr::sym(yaxis)) %>%
    dplyr::mutate(
      group_start = dplyr::first(.data$start),
      group_label = as.character(dplyr::cur_group())
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$group_start, .data$group_label) %>%
    dplyr::mutate(
      group_id = cumsum(dplyr::lag(.data$group_label) != .data$group_label | 
                          is.na(dplyr::lag(.data$group_label)))
    )

  dep <- obs %>%
    dplyr::group_by(.data$group_id, .data$group_label) %>%
    dplyr::summarise(
      start = dplyr::first(.data$start),
      end = dplyr::last(.data$end),
      .groups = "drop"
    )

  ggplot2::ggplot() +
    # Horizontal bars for deployments
    ggplot2::geom_segment(
      data = dep,
      ggplot2::aes(
        x = .data$start, xend = .data$end,
        y = .data$group_id, yend = .data$group_id
      ),
      linewidth = 1,
      color = "blue"
    ) +
    # Vertical lines for observations
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(
        x = .data$timestamp,
        y = .data$group_id,
      ),
      color = "red",
      size = 0.4
    ) +
    # Custom y-axis labels with colors
    ggplot2::scale_y_continuous(
      breaks = dep$group_id,
      labels = dep$group_label
    ) +
    ggplot2::labs(
      x = "Time", y = yaxis,
      title = "Data Package Coverage"
    ) +
    ggplot2::theme_minimal()
}
