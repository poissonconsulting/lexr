check_detect_section <- function(section) {
  datacheckr::check_data3(
    section, list(Section = factor(1),
                  Habitat = factor(1),
                  Area = c(0, 100),
                  Bounded = TRUE,
                  EastingSection = 1,
                  NorthingSection = 1),
    key = "Section", select = TRUE)
}

check_detect_distance <- function(distance) {
  datacheckr::check_data3(
    distance, list(SectionFrom = factor(1),
                   SectionTo = factor(1),
                   Distance = c(0L, 50L)),
    key = c("SectionFrom", "SectionTo"), select = TRUE)
}

check_detect_interval <- function(interval) {
  datacheckr::check_data3(
    interval, list(Interval = c(1L, nrow(interval)),
                   Date = as.Date("2000-01-01"),
                   Year = c(2000L, 2016L),
                   Month = c(1L, 12L),
                   Hour = c(0L, 23L),
                   DayteTime = Sys.time(),
                   DateTime = Sys.time()),
    key = c("Interval"), select = TRUE)
}

check_detect_coverage <- function(coverage) {
  datacheckr::check_data3(
    coverage, list(Interval = c(1L, max_integer()),
                   Section = factor(1),
                   Stations = c(1L, 9L),
                   Coverage = c(0, 1)),
  key = c("Interval", "Section"), select = TRUE)
}

check_detect_capture <- function(capture) {
  datacheckr::check_data3(
    capture, list(Capture = factor(1),
                  IntervalCapture = 1L,
                  SectionCapture = factor(1),
                  Length = c(200L, 1000L),
                  Reward1 = factor(rep("Low", 3), levels = c("Low", "High")),
                  Reward2 = factor(rep("Low", 3), levels = c("Low", "High")),
                  IntervalTagExpire = 1L),
    key = "Capture", select = TRUE)
}

check_detect_recapture <- function(recapture) {
  datacheckr::check_data3(
    recapture, list(IntervalRecapture = 1L,
                  Capture = factor(1),
                  SectionRecapture = factor(1),
                  TBarTag1 = TRUE,
                  TBarTag2 = TRUE,
                  TagsRemoved = TRUE,
                  Released = TRUE),
    key = c("IntervalRecapture", "Capture"), select = TRUE)
}

check_detect_detection <- function(detection) {
  datacheckr::check_data3(
    detection, list(IntervalDetection = 1L,
                 Section = factor(1),
                 Capture = factor(1),
                 Receivers = c(1L, 9L),
                 Detections = c(2L, max_integer())),
  key = c("IntervalDetection", "Section", "Capture"), select = TRUE)
}

check_detect_joins <- function(data) {
  datacheckr::check_join(data$coverage, data$section, "Section")
  datacheckr::check_join(data$distance, data$section, c(SectionFrom = "Section"))
  datacheckr::check_join(data$distance, data$section, c(SectionTo = "Section"))
  datacheckr::check_join(data$coverage, data$section, "Section")
  datacheckr::check_join(data$capture,  data$section, c(SectionCapture = "Section"))
  datacheckr::check_join(data$recapture,  data$section, c(SectionRecapture = "Section"))
  datacheckr::check_join(data$detection,  data$section, "Section")

  datacheckr::check_join(data$coverage,  data$interval, "Interval")
  datacheckr::check_join(data$capture,  data$interval, c(IntervalCapture = "Interval"))
  datacheckr::check_join(data$recapture,  data$interval, c(IntervalRecapture = "Interval"))
  datacheckr::check_join(data$detection,  data$interval, c(IntervalDetection = "Interval"))

  datacheckr::check_join(data$recapture,  data$capture, "Capture")
  datacheckr::check_join(data$detection,  data$capture, "Capture")
  invisible(data)
}

#' Check Lake Exploitation Detection Data
#'
#' Checks loaded lake exploitation data and returns a TRUE if passes all the tests.
#' Otherwise stops with an informative error.
#'
#' @param data The detect_data object to check.
#' @return A flag indicating whether the package data passes the checks.
#' @export
check_detect_data <- function(data) {
  if (!inherits(data, "detect_data")) error("data must be a detect_data object")
  if (!identical(names(data), detect_data_names())) error("data must have correct names")
  data %<>% purrr::lmap(fun_data_name, fun = "check_detect")
  check_detect_joins(data)
  class(data) <- "detect_data"
  invisible(data)
}
