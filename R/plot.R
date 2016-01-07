tidy_section <- function(section) {
  if (!is.null(section)) {
    section %<>% broom::tidy()
    section <- list(
      ggplot2::geom_polygon(data = dplyr::filter_(section, ~!hole),
                            ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~id),
                            alpha = 1/5, color = "grey50"),
      ggplot2::geom_polygon(data = dplyr::filter_(section, ~!hole),
                            ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~id),
                            alpha = 1/5, color = "grey50"))
  }
  section
}

plot_section <- function(section) {

  ggplot2::ggplot(data = section@data, ggplot2::aes_(x = ~SectionX / 1000,
                                                y = ~SectionY / 1000)) +
    tidy_section(section) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes_(x = ~SectionX / 1000,
                                     label = ~Section), nudge_x = 1.5) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_station <- function(station, section = NULL) {
  ggplot2::ggplot(data = station, ggplot2::aes_(x = ~StationX / 1000,
                                                y = ~StationY / 1000)) +
    tidy_section(section) +
    ifelse(is.null(section),
           list(ggrepel::geom_text_repel(ggplot2::aes_(label = ~Station), size = 3, alpha = 0.5)),
           list(ggplot2::geom_point())) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_deployment <- function(deployment, station) {

  deployment %<>% dplyr::inner_join(station, by = "Station")
  tz <- lubridate::tz(deployment$ReceiverDateTimeIn[1])

  ggplot2::ggplot(data = deployment, ggplot2::aes_(y = ~Station)) +
    ggplot2::facet_grid(Section~., scales = "free_y", space = "free_y") +
  ggplot2::geom_segment(ggplot2::aes_(
    x = ~ReceiverDateTimeIn, xend = ~ReceiverDateTimeOut, yend = ~Station),
    alpha = 1/2, size = 3) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_continuous()
}

#' @export
plot.lex_data <- function(x) {
  if (!is.null(x$section))
    plot_section(x$section) %>% print()
  if (!is.null(x$station))
    plot_station(x$station, x$section) %>% print()
#  if (!is.null(x$deployment) && !is.null(x$station))
#    plot_deployment(x$deployment, x$station) %>% print()
  invisible(NULL)
}
