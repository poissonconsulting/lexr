#' Plot Captures
#'
#' Plots captures for a \code{detect_data} object.
#'
#' @param data The detect_data object to plot.
#' @export
plot_capture <- function(data) {
  data %<>% check_detect_data()
  polygon <- section_polygon(data$section)
  capture <- data$capture
  capture %<>% dplyr::group_by_(~SectionCapture) %>% dplyr::summarise_(Captures = ~n()) %>%
    dplyr::ungroup() %>% dplyr::rename_(Section = ~SectionCapture) %>%
    dplyr::mutate_(Captures = ~Captures/sum(Captures))
  data <- dplyr::inner_join(data$section@data, capture, by = "Section")

  ggplot2::ggplot(data = data, ggplot2::aes_(x = ~EastingSection / 1000, y = ~NorthingSection / 1000, group = ~Section)) +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~!hole), ggplot2::aes_(
      x = ~EastingSection / 1000, y = ~NorthingSection / 1000), color = "grey25", fill = "transparent") +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~hole), ggplot2::aes_(
      x = ~EastingSection / 1000, y = ~NorthingSection / 1000, group = ~group), color = "grey25", fill = "transparent") +
    ggplot2::geom_point(ggplot2::aes_(color = ~ColorCode, size = ~Captures), alpha = 0.75) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma) +
    ggplot2::scale_size(name = "Captures (%)", labels = scales::percent) +
    ggplot2::scale_color_identity()
}

#' Plot Recaptures
#'
#' Plots recaptures for a \code{detect_data} object.
#'
#' @param data The detect_data object to plot.
#' @export
plot_recapture <- function(data) {
  data %<>% check_detect_data()
  polygon <- section_polygon(data$section)
  recapture <- data$recapture
  if (!nrow(recapture)) {
    warning("no recaptures to plot", call. = FALSE)
    return(NULL)
  }
  recapture %<>% dplyr::filter_(~!is.na(SectionRecapture))
  if (!nrow(recapture)) {
    warning("no recaptures with known sections to plot", call. = FALSE)
    return(NULL)
  }
  recapture %<>% dplyr::group_by_(~SectionRecapture) %>% dplyr::summarise_(Recaptures = ~n()) %>%
    dplyr::ungroup() %>% dplyr::rename_(Section = ~SectionRecapture) %>%
    dplyr::mutate_(Recaptures = ~Recaptures/sum(Recaptures))
  data <- dplyr::inner_join(data$section@data, recapture, by = "Section")
  ggplot2::ggplot(data = data, ggplot2::aes_(x = ~EastingSection / 1000, y = ~NorthingSection / 1000, group = ~Section)) +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~!hole), ggplot2::aes_(
      x = ~EastingSection / 1000, y = ~NorthingSection / 1000), color = "grey25", fill = "transparent") +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~hole), ggplot2::aes_(
      x = ~EastingSection / 1000, y = ~NorthingSection / 1000, group = ~group), color = "grey25", fill = "transparent") +
    ggplot2::geom_point(ggplot2::aes_(color = ~ColorCode, size = ~Recaptures), alpha = 0.75) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma) +
    ggplot2::scale_size(name = "Recaptures (%)", labels = scales::percent) +
    ggplot2::scale_color_identity()
}
