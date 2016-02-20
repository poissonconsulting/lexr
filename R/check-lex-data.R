check_lex_section <- function(section) {
  if (!inherits(section, "SpatialPolygonsDataFrame"))
    error("section must be a spatial polygons data frame")

  if (!identical(row.names(section), as.character(section@data$Section)))
    error("section row names must be identical to section@data$Section")

  values <- list(Section = factor(1),
                 Habitat = factor(1),
                 Bounded = TRUE,
                 EastingSection = 1,
                 NorthingSection = 1)

  datacheckr::check_data3(section@data, values, key = "Section", select = TRUE)
  invisible(section)
}

check_lex_station <- function(station) {
  values <- list(Station = factor(1),
                 Section = factor(1),
                 EastingStation = 1,
                 NorthingStation = 1)

  datacheckr::check_data3(station, values, key = "Station", select = TRUE)
}

check_lex_deployment <- function(deployment) {

  values <-  list(Station = factor(1),
                  Receiver = factor(1),
                  DateTimeReceiverIn = Sys.time(),
                  DateTimeReceiverOut = Sys.time())

  datacheckr::check_data3(deployment, values,
                          key = c("Station", "Receiver", "DateTimeReceiverIn"), select = TRUE)
  deployment %<>% dplyr::arrange_(~DateTimeReceiverIn, ~DateTimeReceiverOut, ~Station)
  invisible(deployment)
}

check_lex_capture <- function(capture) {
  values <- list(Capture = factor(1),
                 DateTimeCapture = Sys.time(),
                 SectionCapture = factor(1),
                 Species = factor(1),
                 Length = c(200L, 1000L),
                 Weight = c(0.5, 10, NA),
                 Reward1 = c(0L, 10L, 100L),
                 Reward2 = c(0L, 10L, 100L, NA),
                 DateTimeTagExpire = Sys.time(),
                 DepthRangeTag = c(1L, NA))

  datacheckr::check_data3(capture, values, key = "Capture", select = TRUE)
  capture %<>% dplyr::arrange_(~DateTimeCapture, ~Capture)
  invisible(capture)
}

check_lex_recapture <- function(recapture) {
  values <- list(DateTimeRecapture = Sys.time(),
                 Capture = factor(1),
                 SectionRecapture = factor(c(1, NA)),
                 TBarTag1 = TRUE,
                 TBarTag2 = TRUE,
                 TagsRemoved = TRUE,
                 Released = TRUE,
                 Public = TRUE)

  datacheckr::check_data3(recapture, values, select = TRUE)
  recapture %<>% dplyr::arrange_(~DateTimeRecapture, ~Capture)
  invisible(recapture)
}

check_lex_detection <- function(detection) {

  values <- list(DateTimeDetection = Sys.time(),
                 Capture = factor(1),
                 Receiver = factor(1),
                 Detections = c(3L, datacheckr::max_integer()))

  datacheckr::check_data3(detection, values, key = c("DateTimeDetection", "Capture", "Receiver"),
                          select = TRUE)
  detection %<>% dplyr::arrange_(~DateTimeDetection, ~Capture, ~Receiver)
  invisible(detection)
}

check_lex_depth <- function(depth) {

  values <- list(
    DateTimeDepth = Sys.time(),
    Capture = factor(1),
    Receiver = factor(1),
    Depth = c(0, 340))

  datacheckr::check_data3(depth, values, key = c("DateTimeDepth", "Capture", "Receiver"),
                          select = TRUE)
  depth %<>% dplyr::arrange_(~DateTimeDepth, ~Capture, ~Receiver)
  invisible(depth)
}

check_lex_joins <- function(data) {

  datacheckr::check_join(data$station, data$section@data, "Section")
  datacheckr::check_join(data$deployment,  data$station, "Station")
  datacheckr::check_join(data$capture,  data$section@data, c(SectionCapture = "Section"))
  datacheckr::check_join(data$recapture,  data$capture, "Capture")
  datacheckr::check_join(data$recapture,  data$section@data,
                         c(SectionRecapture = "Section"), ignore_nas = TRUE)
  datacheckr::check_join(data$detection,  data$capture, "Capture")
  datacheckr::check_join(data$depth,  data$capture, "Capture")

  stopifnot(all(data$detection$Receiver %in% data$deployment$Receiver))
  stopifnot(all(data$depth$Receiver %in% data$deployment$Receiver))
  invisible(data)
}

check_lex_deployment_detection <- function(deployment, detection) {
  deployment$DeploymentID <- 1:nrow(deployment)
  detection %<>% dplyr::inner_join(deployment, by = "Receiver")
  detection %<>% dplyr::filter_(~DateTimeDetection >= DateTimeReceiverIn)
  detection %<>% dplyr::filter_(~DateTimeDetection <= DateTimeReceiverOut)

  deployment %<>% dplyr::filter_(~!DeploymentID %in% unique(detection$DeploymentID))
  deployment$DeploymentID <- NULL
  if (nrow(deployment)) {
    warning(nrow(deployment), " deployments with no detections")
    print(as.data.frame(deployment))
  }
  invisible(deployment)
}

#' Check Lake Exploitation Data
#'
#' Checks loaded lake exploitation data and returns a TRUE if passes all the tests.
#' Otherwise stops with an informative error.
#'
#' @param data The lex_data object to check.
#' @param all A flag indicating whether to run all checks.
#' @return A flag indicating whether the package data passes the checks.
#' @export
check_lex_data <- function(data, all = FALSE) {
  if (!inherits(data, "lex_data")) error("data must be a lex_data object")
  datacheckr::check_flag(all)
  if (!identical(names(data), lex_data_names())) error("data must have correct names")
  data %<>% purrr::lmap(fun_data_name, fun = "check_lex")
  check_lex_joins(data)
  if (all) {
    check_lex_deployment_detection(data$deployment, data$detection)
  }
  class(data) <- "lex_data"
  invisible(data)
}
