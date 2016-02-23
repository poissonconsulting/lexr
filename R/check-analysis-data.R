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
  invisible(data)
}
