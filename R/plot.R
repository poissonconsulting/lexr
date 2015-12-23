tidy_section_polygons <- function(section_polygons) {
  if (!is.null(section_polygons)) {
    section_polygons %<>% broom::tidy()
    section_polygons <- list(
      ggplot2::geom_polygon(data = dplyr::filter_(section_polygons, ~!hole),
                            ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~id),
                            alpha = 1/5, color = "grey50"),
      ggplot2::geom_polygon(data = dplyr::filter_(section_polygons, ~!hole),
                            ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~id),
                            alpha = 1/5, color = "grey50"))
  }
  section_polygons
}

#' Plot Section
#'
#' @param section A data frame of the section data.
#' @param section_polygons A xx.
#' @return A ggplot2 object.
#' @export
#' @examples
#' plot_section(qlexdatr::section, qlexdatr::section_polygons)
plot_section <- function(section, section_polygons = NULL) {

  ggplot2::ggplot(data = section, ggplot2::aes_(x = ~SectionX / 1000,
                                                y = ~SectionY / 1000)) +
    tidy_section_polygons(section_polygons) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes_(x = ~SectionX / 1000,
                                     label = ~Section), nudge_x = 1.5) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

#' Plot Section
#'
#' @param station A data frame of the section data.
#' @param section_polygons A xx.
#' @return A ggplot2 object.
#' @export
#' @examples
#' plot_station(qlexdatr::section, qlexdatr::section_polygons)
plot_station <- function(station, section_polygons = NULL) {
  ggplot2::ggplot(data = station, ggplot2::aes_(x = ~StationX / 1000,
                                                y = ~StationY / 1000)) +
    tidy_section_polygons(section_polygons) +
    ggplot2::geom_point() +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}
