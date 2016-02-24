section_polygon <- function(section) {
  suppressMessages(polygon <- broom::tidy(section))
  polygon %<>% dplyr::rename_(.dots = list(Section = "id", EastingSection = "long",
                                           NorthingSection = "lat"))
  polygon$Section %<>% factor(levels = levels(section@data$Section))
  polygon %<>% dplyr::inner_join(dplyr::select_(section@data, ~-EastingSection, ~-NorthingSection), by = "Section")
  polygon
}

plot_detect_section <- function(section) {

  polygon <- section_polygon(section)

  ggplot2::ggplot(data = section@data, ggplot2::aes_(x = ~EastingSection / 1000,
                                                     y = ~NorthingSection / 1000,
                                                     group = ~Section)) +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~!hole),
                          ggplot2::aes_(fill = ~ColorCode, color = ~ColorCode)) +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~hole),
                          ggplot2::aes_(color = ~ColorCode, group = ~group), fill = "white") +
    ggplot2::geom_point(ggplot2::aes_(color = ~ColorCode), size = 4) +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~!hole),
                          color = "grey25", fill = "transparent") +
    ggrepel::geom_text_repel(ggplot2::aes_(label = ~Section),
                             size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity()
}

plot_detect_coverage <- function(coverage, section, interval) {
  tz <- lubridate::tz(interval$DateTime)
  first_year <- lubridate::year(interval$DateTime[1])
  last_year <- lubridate::year(interval$DateTime[nrow(interval)])

  all <- expand.grid(Section = unique(coverage$Section), Interval = interval$Interval)
  coverage %<>% dplyr::right_join(all, by = c("Section", "Interval"))
  coverage$Coverage[is.na(coverage$Coverage)] <- 0
  coverage %<>% dplyr::inner_join(interval, by = "Interval")
  coverage %<>% dplyr::inner_join(section@data, by = "Section")
  coverage %<>% dplyr::select_(~DateTime, ~Coverage, ~Section, ~ColorCode)

  ggplot2::ggplot(data = coverage, ggplot2::aes_(x = ~DateTime,
                                                 y = ~Coverage)) +
    ggplot2::facet_grid(Section~.) +
    ggplot2::geom_area(ggplot2::aes_(fill = ~ColorCode)) +
    ggplot2::scale_x_datetime(name = "Date", expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(name = "Coverage (%)", breaks = c(0.5,1), labels = scales::percent) +
    ggplot2::scale_fill_identity() +
    ggplot2::expand_limits(x = as.POSIXct(paste0(c(first_year, last_year + 1), "-01-01"), tz = tz),
                           y = c(0,1))
}

plot_detect_distance <- function(distance, section) {
  distance %<>% dplyr::filter_(~Distance == 1L)
  from <- dplyr::inner_join(distance, section@data, by = c(SectionFrom = "Section"))
  to <- dplyr::inner_join(distance, section@data, by = c(SectionTo = "Section"))
  to %<>% dplyr::select_(.dots = list(SectionTo = "SectionTo", EastingTo = "EastingSection",
                                      NorthingTo = "NorthingSection"))
  from %<>% dplyr::select_(.dots = list(SectionFrom = "SectionFrom", EastingFrom = "EastingSection",
                                        NorthingFrom = "NorthingSection"))

  distance <- dplyr::bind_cols(from, to)

  ggplot2::ggplot(data = section@data, ggplot2::aes_(
    x = ~EastingSection / 1000, y = ~NorthingSection / 1000)) +
    ggplot2::geom_point(ggplot2::aes_(color = ~ColorCode), size = 4) +
    ggplot2::geom_segment(data = distance, ggplot2::aes_(
      x = ~EastingFrom / 1000, y = ~NorthingFrom / 1000,
      xend = ~EastingTo / 1000, yend = ~NorthingTo / 1000),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"), type = "closed"), alpha = 1/2) +
    ggrepel::geom_text_repel(data = section@data, ggplot2::aes_(label = ~Section),
                             size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma) +
    ggplot2::scale_color_identity()

}

plot_detect_overview <- function(capture, recapture, detection, section, interval) {

  tz <- lubridate::tz(interval$DateTime)
  first_year <- lubridate::year(interval$DateTime[1])
  last_year <- lubridate::year(interval$DateTime[nrow(interval)])

  capture %<>% dplyr::inner_join(section@data, by = c(SectionCapture = "Section"))
  recapture %<>% dplyr::inner_join(section@data, by = c(SectionRecapture = "Section"))
  detection %<>% dplyr::inner_join(section@data, by = c(Section = "Section"))

  capture %<>% dplyr::inner_join(interval, by = c(IntervalCapture = "Interval"))
  capture %<>% dplyr::inner_join(dplyr::select_(interval, .dots = list(Interval = "Interval", DateTimeTagExpire = "DateTime")),
                                 by = c(IntervalTagExpire = "Interval"))
  recapture %<>% dplyr::inner_join(interval, by = c(IntervalRecapture = "Interval"))
  detection %<>% dplyr::inner_join(interval, by = c(IntervalDetection = "Interval"))

  recapture %<>% dplyr::inner_join(dplyr::select_(capture, ~Capture, ~Species), by = "Capture")
  detection %<>% dplyr::inner_join(dplyr::select_(capture, ~Capture, ~Species), by = "Capture")

  recapture$Released %<>% factor()
  levels(recapture$Released) %<>% list(Released = "TRUE", Retained = "FALSE")

  ggplot2::ggplot(data = detection, ggplot2::aes_string(x = "DateTime", y = "Capture")) +
    ggplot2::facet_grid(Species~. , scales = "free_y", space = "free_y") +
    ggplot2::geom_segment(data = capture, ggplot2::aes_string(xend = "DateTimeTagExpire", yend = "Capture"), alpha = 1/2) +
    ggplot2::geom_point(ggplot2::aes_string(color = "ColorCode"), alpha = 1/3, size = 1) +
    ggplot2::geom_point(data = capture, color = "red") +
    ggplot2::geom_point(data = recapture, ggplot2::aes_string(shape = "Released"), color = "black") +
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

plot_fish_year <- function(detection, section, capture, recapture) {
  tz <- lubridate::tz(detection$DateTime)
  year <- detection$Year[1]
  message(paste("plotting fish", detection$Capture[1], year, "..."))

  capture %<>% dplyr::filter_(~Year == year, ~Capture == detection$Capture[1])
  recapture %<>% dplyr::filter_(~Year == year, ~Capture == detection$Capture[1])
  section <- section@data

  detection %<>% tidyr::gather_("XY", "UTM", c("EastingSection", "NorthingSection"))
  section %<>% tidyr::gather_("XY", "UTM", c("EastingSection", "NorthingSection"))
  capture %<>% tidyr::gather_("XY", "UTM", c("EastingSection", "NorthingSection"))
  recapture %<>% tidyr::gather_("XY", "UTM", c("EastingSection", "NorthingSection"))

  detection$XY %<>% factor()
  section$XY %<>% factor()
  capture$XY %<>% factor()
  recapture$XY %<>% factor()

  levels(detection$XY) <- list(Northing = "NorthingSection", Easting = "EastingSection")
  levels(section$XY) <- list(Northing = "NorthingSection", Easting = "EastingSection")
  levels(capture$XY) <- list(Northing = "NorthingSection", Easting = "EastingSection")
  levels(recapture$XY) <- list(Northing = "NorthingSection", Easting = "EastingSection")

  recapture$Released %<>% factor()
  levels(recapture$Released) %<>% list(Released = "TRUE", Retained = "FALSE")

  section$DayteTime <- detection$DayteTime[1]
  detection$Jump <- detection$Jump > 0

  gp <- ggplot2::ggplot(data = detection, ggplot2::aes_string(x = "DayteTime", y = "UTM / 1000")) +
    ggplot2::facet_grid(XY~., space = "free_y", scales = "free_y") +
    ggplot2::geom_line() +
    ggplot2::geom_blank(data = section) +
    ggplot2::geom_point(data = dplyr::filter_(detection, ~!Jump), ggplot2::aes_string(color = "ColorCode")) +
    ggplot2::geom_point(data = dplyr::filter_(detection, ~Jump), ggplot2::aes_string(color = "ColorCode"), shape = 17) +
    ggplot2::geom_point(data = capture, color = "red") +
    ggplot2::geom_point(data = recapture, ggplot2::aes_string(shape = "Released"), color = "black") +
    ggplot2::scale_x_datetime(name = "Date",
                              breaks = scales::date_breaks("3 months"),
                              labels = scales::date_format("%b"), expand = c(0,0)) +
    ggplot2::scale_y_continuous(name = "UTM (km)", expand = c(0, 1), label = scales::comma) +
    ggplot2::scale_shape_manual(values = c(17,15)) +
    ggplot2::scale_color_identity() +
    ggplot2::ggtitle(paste(detection$Capture[1], year)) +
    ggplot2::expand_limits(x = as.POSIXct(paste0(c("2000", "2001"), "-01-01"), tz = tz)) +
    ggplot2::theme(legend.position = "none")

  print(gp)
  NULL
}

plot_detect_fish_year <- function(capture, recapture, detection, section, interval) {

  tz <- lubridate::tz(interval$DateTime)
  first_year <- lubridate::year(interval$DateTime[1])
  last_year <- lubridate::year(interval$DateTime[nrow(interval)])

  capture %<>% dplyr::inner_join(section@data, by = c(SectionCapture = "Section"))
  recapture %<>% dplyr::inner_join(section@data, by = c(SectionRecapture = "Section"))
  detection %<>% dplyr::inner_join(section@data, by = c(Section = "Section"))

  capture %<>% dplyr::inner_join(interval, by = c(IntervalCapture = "Interval"))
  capture %<>% dplyr::inner_join(dplyr::select_(interval, .dots = list(Interval = "Interval", DateTimeTagExpire = "DateTime")),
                                 by = c(IntervalTagExpire = "Interval"))
  recapture %<>% dplyr::inner_join(interval, by = c(IntervalRecapture = "Interval"))
  detection %<>% dplyr::inner_join(interval, by = c(IntervalDetection = "Interval"))

  recapture %<>% dplyr::inner_join(dplyr::select_(capture, ~Capture, ~Species), by = "Capture")
  detection %<>% dplyr::inner_join(dplyr::select_(capture, ~Capture, ~Species), by = "Capture")

  recapture$Released %<>% factor()
  levels(recapture$Released) %<>% list(Released = "TRUE", Retained = "FALSE")

  detection$Year <- lubridate::year(detection$DateTime)

  plyr::ddply(detection, c("Capture", "Year"), plot_fish_year, section, capture, recapture)
}


#' @export
plot.detect_data <- function(x, all = FALSE, ...) {
  print(plot_detect_section(x$section))
  print(plot_detect_coverage(x$coverage, x$section, x$interval))
  print(plot_detect_distance(x$distance, x$section))
  print(plot_detect_overview(x$capture, x$recapture, x$detection, x$section, x$interval))
  if (all) {
    plot_detect_fish_year(x$capture, x$recapture, x$detection, x$section, x$interval)
  }
  invisible(NULL)
}
