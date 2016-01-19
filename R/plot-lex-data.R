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

plot_lex_section <- function(section) {

  ggplot2::ggplot(data = section@data, ggplot2::aes_(x = ~EastingSection / 1000,
                                                     y = ~NorthingSection / 1000)) +
    tidy_section(section) +
    ggplot2::geom_point(alpha = 1/3) +
    ggrepel::geom_text_repel(ggplot2::aes_(x = ~EastingSection / 1000, label = ~Section),
                       size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_lex_station <- function(station, section = NULL) {
  ggplot2::ggplot(data = station, ggplot2::aes_(x = ~EastingStation / 1000,
                                                y = ~NorthingStation / 1000)) +
    tidy_section(section) +
    ggplot2::geom_point(alpha = 1/3) +
    ggrepel::geom_text_repel(ggplot2::aes_(label = ~Station), size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_lex_deployment <- function(deployment) {
  tz <- lubridate::tz(deployment$DateTimeReceiverIn[1])
  ggplot2::ggplot(data = deployment, ggplot2::aes_(y = ~Station)) +
    ggplot2::geom_segment(ggplot2::aes_(
      x = ~DateTimeReceiverIn, xend = ~DateTimeReceiverOut, yend = ~Station),
      alpha = 1/2, size = 3) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_discrete()
}

plot_lex_capture <- function(capture) {
  capture %<>% dplyr::mutate_(.dots = list("Year" = ~lubridate::year(DateTimeCapture)))

  ggplot2::ggplot(data = capture, ggplot2::aes_(x = ~Length)) +
    ggplot2::facet_grid(Species~Year) +
    ggplot2::geom_histogram(binwidth = 50, color = "white") +
    ggplot2::scale_x_continuous(name = "Fork Length (mm)") +
    ggplot2::scale_y_continuous(name = "Captures")
}

plot_lex_recapture <- function(recapture) {
  tz <- lubridate::tz(recapture$DateTimeRecapture[1])
  recapture %<>% dplyr::mutate_(.dots = list("SectionRecapture" = ~factor(SectionRecapture)))

  ggplot2::ggplot(data = recapture, ggplot2::aes_(x = ~DateTimeRecapture, y = ~SectionRecapture)) +
    ggplot2::geom_point(ggplot2::aes_(shape = ~Released), alpha = 1/2, size = 4) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz))
}

plot_lex_detection <- function(detection) {
  detection %<>% dplyr::mutate_(.dots = list("Date" = ~as.Date(DateTimeDetection)))

  detection %<>% dplyr::group_by_(~Date) %>%
    dplyr::summarise_(.dots = list("Detections" = ~sum(Detections))) %>%
    dplyr::ungroup()

  ggplot2::ggplot(data = detection, ggplot2::aes_(x = ~Date, y = ~Detections)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(name = "Date", labels = scales::date_format("%b %Y")) +
    ggplot2::scale_y_continuous(name = "Total Daily Detections", labels = scales::comma)
}

plot_lex_depth <- function(depth) {
  tz <- lubridate::tz(depth$DateTimeDepth[1])
  ggplot2::ggplot(data = depth, ggplot2::aes_(x = ~DateTimeDepth, y = ~Depth * -1)) +
    ggplot2::geom_point(alpha = 1/3) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_continuous(name = "Depth (m)")
}

#' @export
plot.lex_data <- function(x, ...) {
  x %<>% purrr::lmap(fun_data_name, fun = "plot_lex")
  lapply(x, print)
  invisible(NULL)
}
