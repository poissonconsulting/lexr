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
  if (all) {
    plot_analysis_fish(x$capture, x$recapture, x$detection, x$section, x$period)
  }
  invisible(NULL)
}
