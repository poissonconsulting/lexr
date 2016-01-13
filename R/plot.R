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
    ggplot2::geom_point(alpha = 1/3) +
    ggrepel::geom_text_repel(ggplot2::aes_(x = ~SectionX / 1000, label = ~Section),
                       size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_station <- function(station, section = NULL) {
  ggplot2::ggplot(data = station, ggplot2::aes_(x = ~StationX / 1000,
                                                y = ~StationY / 1000)) +
    tidy_section(section) +
    ggplot2::geom_point(alpha = 1/3) +
    ggrepel::geom_text_repel(ggplot2::aes_(label = ~Station), size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_receiver <- function(receiver) NULL

plot_deployment <- function(deployment) {
  tz <- lubridate::tz(deployment$ReceiverDateTimeIn[1])
  ggplot2::ggplot(data = deployment, ggplot2::aes_(y = ~Station)) +
    ggplot2::geom_segment(ggplot2::aes_(
      x = ~ReceiverDateTimeIn, xend = ~ReceiverDateTimeOut, yend = ~Station),
      alpha = 1/2, size = 3) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_continuous()
}

plot_capture <- function(capture) {
  capture %<>% dplyr::mutate_(.dots = list("Year" = ~lubridate::year(CaptureDateTime)))

  ggplot2::ggplot(data = capture, ggplot2::aes_(x = ~Length)) +
    ggplot2::facet_grid(Species~Year) +
    ggplot2::geom_histogram(binwidth = 50, color = "white") +
    ggplot2::scale_x_continuous(name = "Fork Length (mm)") +
    ggplot2::scale_y_continuous(name = "Captures")
}

plot_recapture <- function(recapture) {
  tz <- lubridate::tz(recapture$RecaptureDateTime[1])
  recapture %<>% dplyr::mutate_(.dots = list("Section" = ~factor(Section)))

  ggplot2::ggplot(data = recapture, ggplot2::aes_(x = ~RecaptureDateTime, y = ~Section)) +
    ggplot2::geom_point(ggplot2::aes_(shape = ~Released), alpha = 1/2, size = 4) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz))
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

plot_depth <- function(depth) {
  tz <- lubridate::tz(depth$DepthDateTime[1])
  ggplot2::ggplot(data = depth, ggplot2::aes_(x = ~DepthDateTime, y = ~Depth * -1)) +
    ggplot2::geom_point(alpha = 1/3) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_continuous(name = "Depth (m)")
}

plot_data_name <- function(data) {
  name <- names(data)
  expr <- paste0("data$", name, " <- plot_", name, "(data$", name, ")")
  eval(parse(text = expr))
  invisible(data)
}

#' @export
plot.lex_data <- function(x, ...) {
  x %<>% purrr::lmap(fun_data_name, fun = "plot")
  lapply(x, print)
  invisible(NULL)
}
