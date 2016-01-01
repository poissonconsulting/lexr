check_stop <- function(...) stop(..., call. = FALSE)

check_section <- function(section) {
  values <- list(Section = c(1L, nrow(section)),
                 SectionArea = c(0, Inf),
                 SectionX = 1,
                 SectionY = 1)

  datacheckr::check_data(section, values)
  stopifnot(!anyDuplicated(section$Section))
  invisible(section)
}

check_section_polygons <- function(section_polygons) {

  if (!inherits(section_polygons, "SpatialPolygonsDataFrame"))
    check_stop("section_polygons must be a spatial polygons data frame")

  values <- list(Section = c(1L, nrow(section)))

  datacheckr::check_data(section_polygons@data, values)
  stopifnot(!anyDuplicated(section_polygons$Section))

  invisible(section_polygons)
}

check_section_distance <- function(section_distance) {
  if (!inherits(section_distance, "matrix"))
    check_stop("section_distance must be a matrix")

  if (!is.integer(section_distance))
    check_stop("section_distance must be an integer matrix")
  stopifnot(nrow(section_distance) == ncol(section_distance))

  invisible(section_distance)
}

check_station <- function(station) {
  values <- list(Station = c(1L, nrow(section)),
                 Section = c(1L, datacheckr::max_integer()),
                 StationX = 1,
                 StationY = 1)

  datacheckr::check_data(station, values)
  stopifnot(!anyDuplicated(station$Station))
  invisible(station)
}

check_receiver <- function(receiver) {
  values <- list(Receiver = c(1L, nrow(receiver)))

  datacheckr::check_data(receiver, values)
  stopifnot(!anyDuplicated(receiver$Receiver))
  invisible(receiver)
}

check_station_deployment <- function(station_deployment) {
  values <- list(Station = c(1L, datacheckr::max_integer()),
                 Receiver = c(1L, datacheckr::max_integer()),
                 ReceiverDateTimeIn = Sys.time(),
                 ReceiverDateTimeOut = Sys.time())

  datacheckr::check_data(station_deployment, values)
}

check_recapture <- function(recapture) {
  values <- list(RecaptureDateTime = Sys.time(),
                 Capture = c(1L, datacheckr::max_integer()),
                 Section = c(1L, datacheckr::max_integer()),
                 TBarTag1 = TRUE,
                 TBarTag2 = TRUE,
                 TagsRemoved = TRUE,
                 Released = TRUE)

  datacheckr::check_data(recapture, values)
}

check_detection <- function(detection) {

  values <- list(DetectionDateTime = Sys.time(),
                 Capture = c(1L, datacheckr::max_integer()),
                 Receiver = c(1L, datacheckr::max_integer()),
                 Detections = c(1L, datacheckr::max_integer()))

  datacheckr::check_data(detection, values)
}

check_depth <- function(depth) {

  values <- list(
    DepthDateTime = Sys.time(),
    Capture = c(1L, datacheckr::max_integer()),
    Receiver = c(1L, datacheckr::max_integer()),
    Depth = c(0, Inf))

  datacheckr::check_data(depth, values)
}

check_capture <- function(capture) {
  values <- list(
    Capture = c(1L, nrow(capture)),
    CaptureDateTime = Sys.time(),
    Section = c(1L, datacheckr::max_integer()),
    Species = factor(""),
    Length = c(1L, 1000L),
    Weight = c(0, Inf, NA),
    Reward1 = c(1L, 10L, 100L),
    Reward2 = c(1L, 10L, 100L),
    TagExpireDateTime = Sys.time())

  datacheckr::check_data(capture, values)
  stopifnot(!anyDuplicated(capture$Capture))
  invisible(capture)
}

check_all <- function(section, section_polygons, section_distance,
                      station, receiver, station_deployment, recapture, detection, depth,
                      capture) {

   stopifnot(nrow(!section_polygons@data) == nrow(section))
   stopifnot(nrow(section_distance) == nrow(section))
   stopifnot(ncol(section_distance) == nrow(section))

   stopifnot(all(station$Section %in% section$Section))
   stopifnot(all(recapture$Section %in% section$Section))
   stopifnot(all(capture$Section %in% section$Section))

   stopifnot(all(station_deployment$Station %in% station$Station))

   stopifnot(all(station_deployment$Receiver %in% receiver$Receiver))
   stopifnot(all(detection$Receiver %in% receiver$Receiver))
   stopifnot(all(depth$Receiver %in% receiver$Receiver))

   stopifnot(all(detection$Capture %in% capture$Capture))
   stopifnot(all(depth$Capture %in% capture$Capture))
   stopifnot(all(recapture$Capture %in% capture$Capture))

    invisible(section)
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
  check_receiver(receiver)
  check_station_deployment(station_deployment)
  check_recapture(recapture)
  check_detection(detection)
  check_depth(depth)
  check_capture(capture)
  check_all(section, section_polygons, section_distance,
                      station, receiver, station_deployment, recapture, detection, depth,
                      capture)

  TRUE
}
