#' Tidy Section Polygons
#'
#' @param section_polygons A spatial polygons data frame of the sections
#' or NULL.
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
#' @inheritParams tidy_section_polygons
#' @param section A data frame of the section data.
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

#' Plot Station
#'
#' @inheritParams tidy_section_polygons
#' @param station A data frame of the station data.
#' @return A ggplot2 object.
#' @export
#' @examples
#' plot_station(qlexdatr::station, qlexdatr::section_polygons)
plot_station <- function(station, section_polygons = NULL) {
  ggplot2::ggplot(data = station, ggplot2::aes_(x = ~StationX / 1000,
                                                y = ~StationY / 1000)) +
    tidy_section_polygons(section_polygons) +
    ggplot2::geom_point() +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

#' Plot Station
#'
#' @inheritParams plot_station
#' @param station_deployment A data frame of the receiver deployment data.
#' @return A ggplot2 object.
#' @export
#' @examples
#' plot_station_deployment(qlexdatr::station_deployment, qlexdatr::station)
plot_station_deployment <- function(station_deployment, station) {

  station_deployment %<>% dplyr::inner_join(station, by = "Station")
  print(station_deployment)

  tz <- lubridate::tz(station_deployment$ReceiverDateTimeIn[1])

  ggplot2::ggplot(data = station_deployment, ggplot2::aes_(y = ~Station)) +
    ggplot2::facet_grid(Section~., scales = "free_y", space = "free_y") +
  ggplot2::geom_segment(ggplot2::aes_(
    x = ~ReceiverDateTimeIn, xend = ~ReceiverDateTimeOut, yend = ~Station)) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_continuous(name = "Station", labels = scales::date_format("%b %Y", tz))
}

#' Plot Lex Data
#'
#' Plots lake exploitation package data to a pdf.
#'
#' @inheritParams load_lex_data
#' @param file A string of the pdf file name.
#' @export
plot_lex_data <- function(package, file = paste0(package, ".pdf")) {
  load_lex_data(package)

  if (!assertthat::is.string(file)) check_stop("file must be a string")

  pdf(file)
  on.exit(dev.off())

  print(plot_section(section, section_polygons))
  print(plot_station(station, section_polygons))
  print(plot_station_deployment(station_deployment, station))
}
