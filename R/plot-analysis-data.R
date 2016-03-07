plot_analysis_coverage <- function(coverage, section, period) {
  coverage %<>% reshape2::melt(as.is = TRUE, value.name = "Coverage")
  coverage$Section %<>% factor(levels = levels(section$Section))
  coverage$Period %<>% factor(levels = levels(period$Period))

  coverage %<>% dplyr::inner_join(section, by = "Section")
  coverage %<>% dplyr::inner_join(period, by = "Period")

  coverage %<>% plyr::ddply("Section", function(x) {if (max(x$Coverage) == 0) return(NULL); x})

  ggplot2::ggplot(data = coverage, ggplot2::aes_(x = ~DateTime, y = ~Coverage)) +
    ggplot2::facet_grid(Section~.) +
    ggplot2::geom_area(ggplot2::aes_(fill = ~ColorCode)) +
    ggplot2::scale_x_datetime(name = "Date", date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(name = "Coverage (%)", breaks = c(0.5,1), labels = scales::percent) +
    ggplot2::scale_fill_identity()
}

plot_analysis_length <- function(length, capture, period) {
  length %<>% reshape2::melt(as.is = TRUE, value.name = "Length")
  length$Capture %<>% factor(levels = levels(capture$Capture))
  length$Period %<>% factor(levels = levels(period$Period))

  length %<>% dplyr::inner_join(capture, by = "Capture")
  length %<>% dplyr::inner_join(period, by = "Period")
  capture <- dplyr::filter_(length, ~Period == PeriodCapture) %>%
    dplyr::select_(~Capture, ~Length, ~DateTime, ~Species) %>%
    unique()

  ggplot2::ggplot(data = length, ggplot2::aes_(x = ~DateTime, y = ~Length)) +
    ggplot2::facet_grid(Species~.) +
    ggplot2::geom_line(ggplot2::aes_(group = ~Capture)) +
    ggplot2::geom_point(data = capture) +
    ggplot2::scale_x_datetime(name = "Year", date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(name = "Fork Length (mm)")
}

plot_analysis_logical_matrix <- function(x, value, capture, period) {
  x %<>% reshape2::melt(as.is = TRUE, value.name = value)
  x$Capture %<>% factor(levels = levels(capture$Capture))
  x$Period %<>% factor(levels = levels(period$Period))

  x %<>% dplyr::inner_join(capture, by = "Capture")
  x %<>% dplyr::inner_join(period, by = "Period")

  ggplot2::ggplot(data = x, ggplot2::aes_(x = ~DateTime, y = ~Capture)) +
    ggplot2::facet_grid(Species~. , scales = "free_y", space = "free_y") +
    ggplot2::geom_point(ggplot2::aes_string(shape = value, color = value)) +
    ggplot2::scale_x_datetime(name = "Year", date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_color_manual(values = c("red", "black")) +
    ggplot2::scale_shape_manual(values = c(17, 16)) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
}

plot_analysis_spawning <- function(spawning, capture, period) {
  plot_analysis_logical_matrix(spawning, "Spawning", capture, period)
}

plot_analysis_detected <- function(detected, capture, period) {
  plot_analysis_logical_matrix(detected, "Detected", capture, period)
}

plot_analysis_moved <- function(moved, capture, period) {
  plot_analysis_logical_matrix(moved, "Moved", capture, period)
}

plot_analysis_reported <- function(reported, capture, period) {
  plot_analysis_logical_matrix(reported, "Reported", capture, period)
}

plot_analysis_removed <- function(removed, capture, period) {
  plot_analysis_logical_matrix(removed, "Removed", capture, period)
}

plot_analysis_released <- function(released, capture, period) {
  plot_analysis_logical_matrix(released, "Released", capture, period)
}

plot_analysis_public <- function(public, capture, period) {
  plot_analysis_logical_matrix(public, "Public", capture, period)
}

plot_analysis_tags <- function(tags, capture, period) {
  tags %<>% reshape2::melt(as.is = TRUE, value.name = "Tagged")
  tags$Capture %<>% factor(levels = levels(capture$Capture))
  tags$Period %<>% factor(levels = levels(period$Period))
  tags$Tag %<>% factor(levels = c("TBarTag1", "TBarTag2"))

  tags %<>% dplyr::inner_join(capture, by = "Capture")
  tags %<>% dplyr::inner_join(period, by = "Period")

  ggplot2::ggplot(data = tags, ggplot2::aes_(x = ~DateTime, y = ~Capture)) +
    ggplot2::facet_grid(Species~Tag , scales = "free_y", space = "free_y") +
    ggplot2::geom_point(ggplot2::aes_string(shape = "Tagged", color = "Tagged")) +
    ggplot2::scale_x_datetime(name = "Year", date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_color_manual(values = c("red", "black")) +
    ggplot2::scale_shape_manual(values = c(17, 16)) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
}

plot_fish <- function(detection, section, capture, recapture, period) {
  tz <- lubridate::tz(detection$DateTime)
  message(paste("plotting fish", detection$Capture[1], "..."))

  capture %<>% dplyr::filter_(~Capture == detection$Capture[1])
  recapture %<>% dplyr::filter_(~Capture == detection$Capture[1])

  detection %<>% dplyr::filter_(~ProportionPeriod > 0)

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

  period %<>% dplyr::slice(c(1,nrow(.)))
  section %<>% dplyr::slice(c(1,nrow(.)))
  blank <- dplyr::bind_cols(section, period)

  gp <- ggplot2::ggplot(data = detection, ggplot2::aes_string(x = "DateTime", y = "UTM / 1000")) +
    ggplot2::facet_grid(XY~., space = "free_y", scales = "free_y") +
    ggplot2::geom_blank(data = blank) +
    ggplot2::geom_point(data = detection, ggplot2::aes_string(color = "ColorCode")) +
    ggplot2::geom_point(data = capture, color = "red") +
    ggplot2::geom_point(data = recapture, ggplot2::aes_string(shape = "Released"), color = "black") +
    ggplot2::scale_x_datetime(name = "Year",
                              breaks = scales::date_breaks("1 year"),
                              labels = scales::date_format("%Y")) +
    ggplot2::scale_y_continuous(name = "UTM (km)", expand = c(0, 1), label = scales::comma) +
    ggplot2::scale_color_identity() +
    ggplot2::ggtitle(capture$Capture[1]) +
    ggplot2::theme(legend.position = "none")

  print(gp)
  NULL
}

plot_analysis_fish <- function(capture, recapture, detection, section, period) {

  detection %<>% reshape2::melt(varnames = c("Capture", "Period", "Section"),
                                as.is = TRUE, value.name = "ProportionPeriod")
  detection$Capture %<>% factor(levels = levels(capture$Capture))
  detection$Period %<>% factor(levels = levels(period$Period))
  detection$Section %<>% factor(levels = levels(section$Section))

  recapture$Released %<>% factor()
  levels(recapture$Released) %<>% list(Released = "TRUE", Retained = "FALSE")

  capture %<>% dplyr::inner_join(section, by = c(SectionCapture = "Section"))
  recapture %<>% dplyr::inner_join(section, by = c(SectionRecapture = "Section"))
  detection %<>% dplyr::inner_join(section, by = c(Section = "Section"))

  capture %<>% dplyr::inner_join(period, by = c(PeriodCapture = "Period"))
  capture %<>% dplyr::inner_join(dplyr::select_(period, .dots = list(Period = "Period", DateTimeTagExpire = "DateTime")),
                                 by = c(PeriodTagExpire = "Period"))
  recapture %<>% dplyr::inner_join(period, by = c(PeriodRecapture = "Period"))
  detection %<>% dplyr::inner_join(period, by = c(Period = "Period"))

  recapture %<>% dplyr::inner_join(dplyr::select_(capture, ~Capture, ~Species), by = "Capture")
  detection %<>% dplyr::inner_join(dplyr::select_(capture, ~Capture, ~Species), by = "Capture")

  plyr::ddply(detection, c("Capture"), plot_fish, section, capture, recapture, period)
}


#' @export
plot.analysis_data <- function(x, all = FALSE, ...) {
  print(plot_analysis_spawning(x$released, x$capture, x$period))
  stop()
  print(plot_analysis_coverage(x$coverage, x$section, x$period))
  print(plot_analysis_reported(x$reported, x$capture, x$period))
  print(plot_analysis_removed(x$removed, x$capture, x$period))
  print(plot_analysis_released(x$released, x$capture, x$period))
  print(plot_analysis_public(x$public, x$capture, x$period))
  print(plot_analysis_tags(x$tags, x$capture, x$period))
  print(plot_analysis_detected(x$detected, x$capture, x$period))
  print(plot_analysis_moved(x$moved, x$capture, x$period))
  print(plot_analysis_length(x$length, x$capture, x$period))
  if (all) {
    plot_analysis_fish(x$capture, x$recapture, x$detection, x$section, x$period)
  }
  invisible(NULL)
}
