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

plot_detect_overview <- function (capture, recapture, detection, interval) {
  capture %<>% inner_join(interval, by = c(IntervalCapture = "Interval"))
  recapture %<>% inner_join(interval, by = c(IntervalRecapture = "Interval"))
  detection %<>% inner_join(interval, by = c(IntervalDetection = "Interval"))


}

#' @export
plot.detect_data <- function(x, all = FALSE, ...) {
  print(plot_detect_coverage(x$coverage, x$interval))
  print(plot_detect_distance(x$distance, x$section))
  if (all) {
    print(plot_detect_overview(x$capture, x$recapture, x$detection, x$interval))
  }
  invisible(NULL)
}
