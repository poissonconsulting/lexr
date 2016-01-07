tidy_section <- function(section) {
  if (!is.null(section)) {
    section %<>% broom::tidy()
    section <- list(
      ggplot2::geom_polygon(data = dplyr::filter_(section, ~!hole),
                            ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~id),
                            alpha = 1/5, color = "grey50"),
      ggplot2::geom_polygon(data = dplyr::filter_(section, ~hole),
                            ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~id),
                            color = "white"))
  }
  section
}

plot_section <- function(section) {

  ggplot2::ggplot(data = section@data, ggplot2::aes_(x = ~SectionX / 1000,
                                                     y = ~SectionY / 1000)) +
    tidy_section(section) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes_(x = ~SectionX / 1000, label = ~Section),
                       size = 3, alpha = 0.5, nudge_x = 1.5) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_station <- function(station, section) {
  ggplot2::ggplot(data = station, ggplot2::aes_(x = ~StationX / 1000,
                                                y = ~StationY / 1000)) +
    tidy_section(section) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes_(label = ~Station),
                       size = 3, alpha = 0.5, nudge_x = 1.5) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_deployment <- function(deployment) {
  tz <- lubridate::tz(deployment$ReceiverDateTimeIn[1])
  ggplot2::ggplot(data = deployment, ggplot2::aes_(y = ~Station)) +
    ggplot2::geom_segment(ggplot2::aes_(
      x = ~ReceiverDateTimeIn, xend = ~ReceiverDateTimeOut, yend = ~Station),
      alpha = 1/2, size = 3) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_continuous()
}

plot_detection <- function(detection) {
  detection %<>% dplyr::mutate_(.dots = list("Date" = ~as.Date(DetectionDateTime)))

  detection %<>% dplyr::group_by_(~Date) %>%
    dplyr::summarise_(.dots = list("Detections" = ~sum(Detections))) %>%
    dplyr::ungroup()

  ggplot2::ggplot(data = detection, ggplot2::aes_(x = ~Date, y = ~Detections)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(name = "Date", labels = scales::date_format("%b %Y")) +
    ggplot2::scale_y_continuous(name = "Total Daily Detections", labels = scales::comma)
}

#' @export
plot.lex_data <- function(x, ...) {
  if (!is.null(x$section)) {
    plot_section(x$section) %>% print()
    if (!is.null(x$station))
      plot_station(x$station, x$section) %>% print()
  }
  if (!is.null(x$deployment))
    plot_deployment(x$deployment) %>% print()
  if (!is.null(x$detection))
    plot_detection(x$detection) %>% print()
  invisible(NULL)
}
