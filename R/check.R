check_stop <- function(...) stop(..., call. = FALSE)

#' Check Lake Exploitation Data
#'
#' Checks lake exploitation data and returns a TRUE if passes all the tests.
#' Otherwise stops with an informative error.
#'
#' @inheritParams load_lex_data
#' @return A flag indicating whether the package data passes the checks.
#' @export
check_lex_data <- function(package) {
  load_lex_data(package)

  if (!dplyr::is.tbl(section)) check_stop("section is not a tbl data frame")
  if (class(section_polygons) != "SpatialPolygonsDataFrame")
    check_stop("section_distance is not a spatial polygons data frame")
  if (!is.matrix(section_distance)) check_stop("section_distance is not a matrix")
  if (!dplyr::is.tbl(station)) check_stop("station is not a tbl data frame")
  if (!dplyr::is.tbl(receiver)) check_stop("receiver is not a tbl data frame")
  if (!dplyr::is.tbl(station_deployment))
    check_stop("station_deployment is not a tbl data frame")
  if (!dplyr::is.tbl(recapture)) check_stop("recapture is not a tbl data frame")
  if (!dplyr::is.tbl(detection)) check_stop("detection is not a tbl data frame")
  if (!dplyr::is.tbl(depth)) check_stop("depth is not a tbl data frame")
  if (!dplyr::is.tbl(capture)) check_stop("capture is not a tbl data frame")


  TRUE
}
