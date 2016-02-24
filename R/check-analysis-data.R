check_detect_dims <- function (data) {
  sections <- nrow(data$section)
  intervals <- nrow(data$interval)
  captures <- nrow(data$capture)

  stopifnot(identical(dim(data$step), c(sections, sections)))
  stopifnot(identical(dim(data$distance), c(sections, sections)))
  stopifnot(identical(dim(data$coverage), c(sections, intervals)))
  stopifnot(identical(dim(data$monitored), c(captures, intervals)))
  stopifnot(identical(dim(data$detection), c(captures, intervals)))
  stopifnot(identical(dim(data$alive), c(captures, intervals)))
  invisible(data)
}

#' Check Lake Exploitation Analysis Data
#'
#' Checks loaded lake exploitation data and returns an invisible copy of the data.
#' Otherwise stops with an informative error.
#'
#' @param data The detect_data object to check.
#' @export
check_analysis_data <- function(data) {
  if (!inherits(data, "analysis_data")) error("data must be a analysis_data object")
  if (!identical(names(data), analysis_data_names())) error("data must have correct names")
  check_detect_dims(data)
  invisible(data)
}
