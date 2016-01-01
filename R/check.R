check_stop <- function(...) stop(..., call. = FALSE)

check_section <- function(section) {
  if (!inherits(section, "SpatialPolygonsDataFrame"))
    check_stop("section must be a spatial polygons data frame")

  values <- list(Section = c(1L, nrow(section)),
                 SectionX = 1,
                 SectionY = 1)

  datacheckr::check_data(section@data, values)
  stopifnot(!anyDuplicated(section@data$Section))
  section@data %<>% subset(select = names(values))
  invisible(section)
}

check_station <- function(station) {
  values <- list(Station = c(1L, nrow(section)),
                 Section = c(1L, datacheckr::max_integer()),
                 StationX = 1,
                 StationY = 1)

  datacheckr::check_data(station, values)
  stopifnot(!anyDuplicated(station$Station))
  station %<>% subset(select = names(values))
  invisible(station)
}

check_receiver <- function(receiver) {
  values <- list(Receiver = c(1L, nrow(receiver)))

  datacheckr::check_data(receiver, values)
  stopifnot(!anyDuplicated(receiver$Receiver))
  receiver %<>% subset(select = names(values))
  invisible(receiver)
}

check_deployment <- function(deployment) {
  values <- list(Station = c(1L, datacheckr::max_integer()),
                 Receiver = c(1L, datacheckr::max_integer()),
                 ReceiverDateTimeIn = Sys.time(),
                 ReceiverDateTimeOut = Sys.time())

  datacheckr::check_data(deployment, values)
  deployment %<>% subset(select = names(values))
  invisible(deployment)
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
  recapture %<>% subset(select = names(values))
  invisible(recapture)
}

check_detection <- function(detection) {

  values <- list(DetectionDateTime = Sys.time(),
                 Capture = c(1L, datacheckr::max_integer()),
                 Receiver = c(1L, datacheckr::max_integer()),
                 Detections = c(1L, datacheckr::max_integer()))

  datacheckr::check_data(detection, values)
  detection %<>% subset(select = names(values))
  invisible(detection)
}

check_depth <- function(depth) {

  values <- list(
    DepthDateTime = Sys.time(),
    Capture = c(1L, datacheckr::max_integer()),
    Receiver = c(1L, datacheckr::max_integer()),
    Depth = c(0, Inf))

  datacheckr::check_data(depth, values)
  depth %<>% subset(select = names(values))
  invisible(depth)
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
  capture %<>% subset(select = names(values))
  invisible(capture)
}

check_data <- function(data) {
  expr <- paste0("check_", data$name, "(", data$name,")")
}

check_all <- function(data) {

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
  data <- list_lex_data(package)

  data %<>% lapply(check_data)
  check_all(data)
  invisible(data)
}
