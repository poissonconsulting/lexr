section <- NULL
section_polygons <- NULL
section_distance <- NULL
station <- NULL
receiver <- NULL
station_deployment <- NULL
recapture <- NULL
detection <- NULL
depth <- NULL
capture <- NULL

#' Load Lake Exploitation Data
#'
#' @param package A string of the data package.
load_lex_data <- function(package) {
  if (!assertthat::is.string(package)) check_stop("package must be a string")

  data("section", "section_polygons", "section_distance",
       "station", "receiver", "station_deployment",
       "recapture", "detection", "depth", "capture",
       package = package, verbose = FALSE, envir = parent.frame())
}
