plot_detect_coverage <- function(coverage, interval) {
  coverage %<>% dplyr::inner_join(interval, by = "Interval")

  coverage %<>% dplyr::mutate_(.dots = list(Year = "lubridate::year(Date)",
                                            DayteTime = "DateTime"))
  lubridate::year(coverage$DayteTime) <- 2000
  ggplot2::ggplot(data = coverage, ggplot2::aes_(x = ~DayteTime,
                                                 y = ~Coverage)) +
    ggplot2::facet_grid(Section~Year) +
    ggplot2::geom_area() +
    ggplot2::scale_x_datetime(name = "Date", breaks = scales::date_breaks("6 months"), labels = scales::date_format("%b")) +
    ggplot2::scale_y_continuous(name = "Coverage (%)", breaks = c(0.5,1), labels = scales::percent) +
    ggplot2::expand_limits(y = c(0,1))
}

plot_detect_distance <- function(distance, section) {
  distance %<>% dplyr::filter_(~Distance == 1L)
  from <- inner_join(distance, section, by = c(SectionFrom = "Section"))
  to <- inner_join(distance, section, by = c(SectionTo = "Section"))
  to %<>% dplyr::select_(.dots = list(SectionTo = "SectionTo", EastingTo = "EastingSection",
                                      NorthingTo = "NorthingSection"))
  from %<>% dplyr::select_(.dots = list(SectionFrom = "SectionFrom", EastingFrom = "EastingSection",
                                        NorthingFrom = "NorthingSection"))

  distance <- dplyr::bind_cols(from, to)

  ggplot2::ggplot(data = section, ggplot2::aes_(
    x = ~EastingSection / 1000, y = ~NorthingSection / 1000)) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(data = distance, ggplot2::aes_(
      x = ~EastingFrom / 1000, y = ~NorthingFrom / 1000,
      xend = ~EastingTo / 1000, yend = ~NorthingTo / 1000),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"), type = "closed"), alpha = 1/2) +
    ggrepel::geom_text_repel(data = section, ggplot2::aes_(label = ~Section),
                             size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_detect_overview <- function(capture, recapture, detection, section, interval) {

  tz <- lubridate::tz(interval$DateTime)
  first_year <- lubridate::year(interval$DateTime[1])
  last_year <- lubridate::year(interval$DateTime[nrow(interval)])
  capture %<>% dplyr::inner_join(section, by = c(SectionCapture = "Section"))
  recapture %<>% dplyr::inner_join(section, by = c(SectionRecapture = "Section"))
  detection %<>% dplyr::inner_join(section, by = c(Section = "Section"))

  capture %<>% dplyr::inner_join(interval, by = c(IntervalCapture = "Interval"))
  capture %<>% dplyr::inner_join(dplyr::select_(interval, .dots = list(Interval = "Interval", DateTimeTagExpire = "DateTime")),
                                 by = c(IntervalTagExpire = "Interval"))
  recapture %<>% dplyr::inner_join(interval, by = c(IntervalRecapture = "Interval"))
  detection %<>% dplyr::inner_join(interval, by = c(IntervalDetection = "Interval"))

  recapture %<>% dplyr::inner_join(dplyr::select_(capture, ~Capture, ~Species), by = "Capture")
  detection %<>% dplyr::inner_join(dplyr::select_(capture, ~Capture, ~Species), by = "Capture")

  recapture$Released %<>% factor()
  levels(recapture$Released) %<>% list(Released = "TRUE", Retained = "FALSE")
  #   location$Fish %<>% as.integer()
  #   fish$Fish %<>% as.integer()
  #   recapture$Fish %<>% as.integer()
  #
  ggplot2::ggplot(data = detection, ggplot2::aes_string(x = "DateTime", y = "Capture")) +
    ggplot2::facet_grid(Species~. , scales = "free_y", space = "free_y") +
    ggplot2::geom_segment(data = capture, ggplot2::aes_string(xend = "DateTimeTagExpire", yend = "Capture"), alpha = 1/2) +
    ggplot2::geom_point(ggplot2::aes_string(color = "ColorCode"), alpha = 1/3) +
    ggplot2::geom_point(data = capture, color = "red") +
    ggplot2::geom_point(data = recapture, ggplot2::aes_string(shape = "Released"), color = "black", size = 3) +
    ggplot2::scale_x_datetime(name = "Date", expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_color_identity() +
    ggplot2::scale_shape_manual(values = c(17,15)) +
    ggplot2::expand_limits(x = as.POSIXct(paste0(c(first_year, last_year + 1), "-01-01"), tz = tz)) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   legend.position = "none")
}

#' @export
plot.detect_data <- function(x, all = FALSE, ...) {
  #  print(plot_detect_coverage(x$coverage, x$interval))
  #  print(plot_detect_distance(x$distance, x$section))
  print(plot_detect_overview(x$capture, x$recapture, x$detection, x$section, x$interval))
  if (all) {
  }
  invisible(NULL)
}
