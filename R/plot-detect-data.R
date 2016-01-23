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

plot_detect_overview <- function (capture, recapture, detection, interval) {
  capture %<>% inner_join(interval, by = c(IntervalCapture = "Interval"))
  recapture %<>% inner_join(interval, by = c(IntervalRecapture = "Interval"))
  detection %<>% inner_join(interval, by = c(IntervalDetection = "Interval"))


}

#' @export
plot.detect_data <- function(x, all = FALSE...) {
  print(plot_detect_coverage(x$coverage, x$interval))
  if (all) {
    print(plot_detect_overview(x$capture, x$recapture, x$detection, x$interval))
  }
  invisible(NULL)
}
