plot_detect_coverage <- function(coverage) {

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

#' @export
plot.detect_data <- function(x, ...) {
  coverage <- dplyr::inner_join(x$coverage, x$interval, by = "Interval")
  print(plot_detect_coverage(coverage))
  invisible(NULL)
}
