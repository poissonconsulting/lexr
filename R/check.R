check_stop <- function(...) stop(..., call. = FALSE)

check_section <- function(section) {
  values <- list(Section = c(1L, .Machine$integer.max),
                 SectionArea = c(0, Inf),
                 SectionX = 1,
                 SectionY = 1)

  datacheckr::check_data(section, values)
  TRUE
}

check_section_polygons <- function(section_polygons) {

  if (!inherits(section_polygons, "SpatialPolygonsDataFrame"))
    check_stop("section_polygons must be a spatial polygons data frame")

  values <- list(Section = c(1L, .Machine$integer.max))
  datacheckr::check_data(section_polygons@data, values)
  TRUE
}

check_section_distance <- function(section_distance) {
  if (!inherits(section_distance, "matrix"))
    check_stop("section_distance must be a matrix")

  if (!is.integer(section_distance))
    check_stop("section_distance must be an integer matrix")
  TRUE
}

check_station <- function(station) {
  values <- list(Station = c(1L, .Machine$integer.max),
                 Section = c(1L, .Machine$integer.max),
                 StationX = 1,
                 StationY = 1)

  datacheckr::check_data(station, values)
  TRUE
}

check_station_deployment <- function(station_deployment) {
  values <- list(Station = c(1L, .Machine$integer.max),
                 Receiver = c(1L, .Machine$integer.max),
                 ReceiverDateTimeIn = Sys.time(),
                 ReceiverDateTimeOut = Sys.time())

  datacheckr::check_data(station_deployment, values)
  TRUE
}

check_recapture <- function(recapture) {
  values <- list(RecaptureDateTime = Sys.time(),
                 Capture = c(1L, .Machine$integer.max),
                 Section = c(1L, .Machine$integer.max),
                 TBarTag1 = TRUE,
                 TBarTag2 = TRUE,
                 TagsRemoved = TRUE,
                 Released = TRUE)

  datacheckr::check_data(recapture, values)
  TRUE
}

check_detection <- function(detection) {

  values <- list(DetectionDateTime = Sys.time(),
                 Capture = c(1L, .Machine$integer.max),
                 Receiver = c(1L, .Machine$integer.max),
                 Detections = c(1L, .Machine$integer.max))

  datacheckr::check_data(detection, values)
  TRUE
}

check_depth <- function(depth) {

  values <- list(
    DepthDateTime = Sys.time(),
    Capture = c(1L, .Machine$integer.max),
    Receiver = c(1L, .Machine$integer.max),
    Depth = c(0, Inf))
  datacheckr::check_data(depth, values)
  TRUE
}

check_capture <- function(capture) {
  values <- list(
    Capture = c(1L, .Machine$integer.max),
    CaptureDateTime = Sys.time(),
    Section = c(1L, .Machine$integer.max),
    Species = factor(""),
    Length = c(1L, 1000L),
    Weight = c(0, Inf, NA),
    Reward1 = c(1, 10, 100),
    Reward2 = c(1, 10, 100),
    TagExpireDateTime = Sys.time())

  TRUE
}

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

  check_section(section)
  check_section_polygons(section_polygons)
  check_section_distance(section_distance)
  check_station(station)
  check_station_deployment(station_deployment)
  check_recapture(recapture)
  check_detection(detection)
  check_depth(depth)
  check_capture(capture)

  TRUE
}
