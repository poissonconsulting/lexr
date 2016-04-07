check_detect_section <- function(section) {

  if (!identical(row.names(section), as.character(section@data$Section)))
    error("section row names must be identical to section@data$Section")

  section@data %<>% check_data3(
    list(Section = factor(1),
                  Habitat = factor(c(1, NA)),
                  Area = c(0, 100),
                  Bounded = TRUE,
                  EastingSection = 1,
                  NorthingSection = 1,
                  ColorCode = rep("[#].{6,6}", 2)),
    key = "Section", select = TRUE)
  check_key(section@data, "Area") # unique key for tiebreaks
  check_key(section@data, c("EastingSection", "NorthingSection"))
  invisible(section)
}

check_detect_distance <- function(distance) {
  check_data3(
    distance, list(SectionFrom = factor(1),
                   SectionTo = factor(1),
                   Distance = c(0L, as.integer(ceiling(sqrt(nrow(distance)))))),
    key = c("SectionFrom", "SectionTo"), select = TRUE)
}

check_detect_interval <- function(interval) {
  check_data3(
    interval, list(Interval = c(1L, nrow(interval)),
                   Date = as.Date("2000-01-01"),
                   Year = c(2000L, 2030L),
                   Month = c(1L, 12L),
                   Hour = c(0L, 23L),
                   DayteTime = Sys.time(),
                   DateTime = Sys.time()),
    key = c("Interval"), select = TRUE)
}

check_detect_coverage <- function(coverage) {
  check_data3(
    coverage, list(Interval = c(1L, max_integer()),
                   Section = factor(1),
                   Stations = c(1L, 9L),
                   Coverage = c(0, 1)),
  key = c("Interval", "Section"), select = TRUE)
}

check_detect_capture <- function(capture) {
  check_data3(
    capture, list(Capture = factor(1),
                  Species = factor(1),
                  IntervalCapture = 1L,
                  SectionCapture = factor(1),
                  Length = c(200L, 1000L),
                  Weight = c(0.5, 10, NA),
                  Reward1 = c(0L, 200L),
                  Reward2 = c(0L, 200L, NA),
                  IntervalTagExpire = 1L),
    key = "Capture", select = TRUE)
}

check_detect_recapture <- function(recapture) {
  check_data3(
    recapture, list(IntervalRecapture = 1L,
                  Capture = factor(1),
                  SectionRecapture = factor(c(1, NA)),
                  TBarTag1 = TRUE,
                  TBarTag2 = TRUE,
                  TagsRemoved = TRUE,
                  Released = TRUE,
                  Public = TRUE),
    min_row = 0,
    key = c("IntervalRecapture", "Capture"), select = TRUE)
}

check_detect_detection <- function(detection) {
  check_data3(
    detection, list(IntervalDetection = 1L,
                 Section = factor(1),
                 Capture = factor(1),
                 Receivers = c(1L, 9L),
                 Detections = c(3L, max_integer()),
                 Sections = c(1L, max_integer()),
                 Jump = c(0L, max_integer())),
  key = c("IntervalDetection", "Capture"), select = TRUE)
}

check_detect_joins <- function(data) {
  check_join(data$coverage, data$section@data, "Section")
  check_join(data$distance, data$section@data, c(SectionFrom = "Section"))
  check_join(data$distance, data$section@data, c(SectionTo = "Section"))
  check_join(data$coverage, data$section@data, "Section")
  check_join(data$capture,  data$section@data, c(SectionCapture = "Section"))
  check_join(data$recapture,  data$section@data,
                         c(SectionRecapture = "Section"), ignore_nas = TRUE)
  check_join(data$detection,  data$section@data, "Section")

  check_join(data$coverage,  data$interval, "Interval")
  check_join(data$capture,  data$interval, c(IntervalCapture = "Interval"))
  check_join(data$recapture,  data$interval, c(IntervalRecapture = "Interval"))
  check_join(data$detection,  data$interval, c(IntervalDetection = "Interval"))

  check_join(data$recapture,  data$capture, "Capture")
  check_join(data$detection,  data$capture, "Capture")
  invisible(data)
}

#' Check Lake Exploitation Detection Data
#'
#' Checks lake exploitation data and returns an invisible copy of the data.
#' Otherwise stops with an informative error.
#'
#' @param data The \code{detect_data} object to check.
#' @export
check_detect_data <- function(data) {
  if (!inherits(data, "detect_data")) error("data must be a detect_data object")
  if (!identical(names(data), detect_data_names())) error("data must have correct names")
  data %<>% purrr::lmap(fun_data_name, fun = "check_detect")
  check_detect_joins(data)
  class(data) <- "detect_data"
  invisible(data)
}
