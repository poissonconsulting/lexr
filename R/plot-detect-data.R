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

plot_detect_overview <- function(capture, recapture, detection, interval) {
  capture %<>% inner_join(interval, by = c(IntervalCapture = "Interval"))
  recapture %<>% inner_join(interval, by = c(IntervalRecapture = "Interval"))
  detection %<>% inner_join(interval, by = c(IntervalDetection = "Interval"))

#   location %<>% dplyr::inner_join(fish, by = "Fish")
#
#   recapture <- klesdatr::recapture %>% dplyr::inner_join(fish, by = "Fish")
#
#   location$Section %<>% as.integer()
#   recapture$Recapture <- factor(recapture$Released)
#   levels(recapture$Recapture) %<>% list(Released = "TRUE", Retained = "FALSE")
#   location$Fish %<>% as.integer()
#   fish$Fish %<>% as.integer()
#   recapture$Fish %<>% as.integer()
#
#   ggplot2::ggplot(data = location, ggplot2::aes_string(x = "DetectionDate", y = "Fish")) +
#     ggplot2::facet_grid(Species~. , scales = "free_y", space = "free_y") +
#     ggplot2::geom_segment(data = fish, ggplot2::aes_string(x = "CaptureDate", xend = "ExpirationDate", yend = "Fish"), alpha = 1/2) +
#     ggplot2::geom_point(ggplot2::aes_string(color = "Section"), alpha = 1/2) +
#     ggplot2::geom_point(data = fish, ggplot2::aes_string(x = "CaptureDate"), color = "red") +
#     ggplot2::geom_point(data = recapture, ggplot2::aes_string(x = "RecaptureDate", shape = "Recapture"), color = "black", size = 3) +
#     ggplot2::scale_x_date(name = "Date", expand = c(0,0)) +
#     ggplot2::scale_y_continuous(expand = c(0,1)) +
#     ggplot2::scale_colour_continuous(low = "grey25", high = "grey75", guide = ggplot2::guide_colourbar(reverse = TRUE)) +
#     ggplot2::scale_shape_manual(values = c(17,15)) +
#     ggplot2::expand_limits(x = as.Date(paste0(c(first_year(), last_year() + 1), "-01-01")))
  NULL
}

#' @export
plot.detect_data <- function(x, all = FALSE, ...) {
  print(plot_detect_coverage(x$coverage, x$interval))
  print(plot_detect_distance(x$distance, x$section))
    print(plot_detect_overview(x$capture, x$recapture, x$detection, x$interval))
  if (all) {
  }
  invisible(NULL)
}
