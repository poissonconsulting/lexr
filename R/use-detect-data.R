#' Use Detect Data
#'
#' Calculates Percent Use from Detect data.
#'
#' @param data The detect_data object to merge.
#' @param alive_only A flag indicating wether to discard detections after last movement.
#' @return A tbl data frame.
#' @export
use_detect_data <-  function(data, alive_only = TRUE) {
  data %<>% check_detect_data()

  data %<>% filter_detect_data(alive_only = alive_only)
  data %<>% merge_detect_data()

  data %<>% dplyr::group_by_(~Section, ~Habitat, ~Area, ~Bounded, ~EastingSection, ~NorthingSection,
                     ~ColorCode) %>%
    dplyr::summarise_(.dots = list(Use = ~sum(DetectedFish) / first(Area) / mean(Coverage),
              Coverage = ~mean(Coverage))) %>%
    dplyr::ungroup() %>%  dplyr::filter_(~Coverage > 0)

  data %<>% dplyr::mutate_(.dots = list(Use = ~Use / sum(Use)))
  check_vector(data$Use, c(0, 1))
  data
}

#' Plot Use
#'
#' Plots habitat use for a \code{detect_data} object.
#'
#' @inheritParams use_detect_data
#' @export
plot_use_detect <- function(data, alive_only = TRUE) {
  data %<>% check_detect_data()
  polygon <- section_polygon(data$section)

  data %<>% use_detect_data(alive_only = alive_only)
  ggplot2::ggplot(data = data, ggplot2::aes_(x = ~EastingSection / 1000, y = ~NorthingSection / 1000)) +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~!hole), ggplot2::aes_(
      x = ~EastingSection / 1000, y = ~NorthingSection / 1000, group = ~Section), color = "grey25", fill = "transparent") +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~hole), ggplot2::aes_(
      x = ~EastingSection / 1000, y = ~NorthingSection / 1000, group = ~Section), color = "grey25", fill = "transparent") +
      ggplot2::geom_point(ggplot2::aes_(color = ~ColorCode, size = ~Use), alpha = 0.75) +
   ggplot2::coord_equal() +
   ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
   ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma) +
   ggplot2::scale_size(name = "Habitat Use (%)", labels = scales::percent) +
   ggplot2::scale_color_identity()
}

