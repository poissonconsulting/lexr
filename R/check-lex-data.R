check_lex_section <- function(section) {
  if (!inherits(section, "SpatialPolygonsDataFrame"))
    error("section must be a spatial polygons data frame")

  values <- list(Section = c(1L, nrow(section@data)),
         SectionX = 1,
         SectionY = 1)

  datacheckr::check_data3(section@data, values, key = "Section", select = TRUE)
  invisible(section)
}

check_lex_station <- function(station) {
  values <- list(Station = c(1L, nrow(station)),
         Section = 1L,
         StationX = 1,
         StationY = 1)

  datacheckr::check_data3(station, values, key = "Station", select = TRUE)
}

check_lex_receiver <- function(receiver) {
  values <- list(Receiver = c(1L, nrow(receiver)))

  datacheckr::check_data3(receiver, values, key = "Receiver", select = TRUE)
}

check_lex_deployment <- function(deployment) {

  values <-  list(Station = 1L,
         Receiver = 1L,
         ReceiverDateTimeIn = Sys.time(),
         ReceiverDateTimeOut = Sys.time())

  datacheckr::check_data3(deployment, values,
                          key = c("Station", "Receiver", "ReceiverDateTimeIn"), select = TRUE)
}

check_lex_capture <- function(capture) {
  values <- list(Capture = c(1L, nrow(capture)),
         CaptureDateTime = Sys.time(),
         Section = 1L,
         Species = factor(""),
         Length = c(200L, 1000L),
         Weight = c(0.5, 10, NA),
         Reward1 = c(1L, 10L, 100L),
         Reward2 = c(1L, 10L, 100L),
         TagExpireDateTime = Sys.time(),
         TagDepthRange = c(1, NA))

  datacheckr::check_data3(capture, values, key = "Capture", select = TRUE)
}

check_lex_recapture <- function(recapture) {
  values <- list(RecaptureDateTime = Sys.time(),
         Capture = 1L,
         Section = c(1L,NA),
         TBarTag1 = TRUE,
         TBarTag2 = TRUE,
         TagsRemoved = TRUE,
         Released = TRUE)

  datacheckr::check_data3(recapture, values, select = TRUE)
}

check_lex_detection <- function(detection) {

  values <- list(DetectionDateTime = Sys.time(),
         Capture = 1L,
         Receiver = 1L,
         Detections = 1L)

  datacheckr::check_data3(detection, values, key = c("DetectionDateTime", "Capture", "Receiver"),
                          select = TRUE)
}

check_lex_depth <- function(depth) {

  values <- list(
    DepthDateTime = Sys.time(),
         Capture = 1L,
         Receiver = 1L,
         Depth = c(0, 340))

  datacheckr::check_data3(depth, values, key = c("DepthDateTime", "Capture", "Receiver"),
                          select = TRUE)
}

check_lex_joins <- function(data) {

  datacheckr::check_join(data$station, data$section@data, "Section")
  datacheckr::check_join(data$deployment,  data$station, "Station")
  datacheckr::check_join(data$deployment,  data$receiver, "Receiver")
  datacheckr::check_join(data$capture,  data$section@data, "Section")
  datacheckr::check_join(data$recapture,  data$capture, "Capture", extra = TRUE)
  datacheckr::check_join(data$recapture,  data$section@data, "Section")
  datacheckr::check_join(data$detection,  data$capture, "Capture")
  datacheckr::check_join(data$detection,  data$receiver, "Receiver")
  datacheckr::check_join(data$depth,  data$capture, "Capture")
  datacheckr::check_join(data$depth,  data$receiver, "Receiver")
  invisible(data)
}

#' Check Lake Exploitation Data
#'
#' Checks loaded lake exploitation data and returns a TRUE if passes all the tests.
#' Otherwise stops with an informative error.
#'
#' @param data The lex_data object to check.
#' @return A flag indicating whether the package data passes the checks.
#' @export
check_lex_data <- function(data) {
  if (!inherits(data, "lex_data")) error("data must be a lex_data object")
  if (!identical(names(data), lex_data_names())) error("data must have correct names")
  data %<>% purrr::lmap(fun_data_name, fun = "check_lex")
  check_lex_joins(data)
  class(data) <- "lex_data"
  invisible(data)
}
