check_detect_section <- function(section) {
  datacheckr::check_data2(
    section, list(Section = factor(1),
                  Habitat = factor(1),
                  Area = c(0, 100),
                  Bounded = TRUE,
                  EastingSection = 1,
                  NorthingSection = 1),
    key = "Section", select = TRUE)
}

check_detect_distance <- function(distance) {
  datacheckr::check_data2(
    distance, list(SectionFrom = factor(1),
                  SectionTo = factor(1),
                  Distance = c(0L, 50L)),
    key = c("SectionFrom", "SectionTo"), select = TRUE)
}

check_detect_interval <- function(interval) {
  datacheckr::check_data2(
    interval, list(Interval = c(1L, nrow(interval)),
                 Date = as.Date("2000-01-01"),
                 Hour = c(1L, 23L),
                 DateTime = Sys.time()),
    key = c("Interval"), select = TRUE)
}

check_detect_coverage <- function(coverage) {
  datacheckr::check_data2(
    coverage, list(IntervalDeployment = c(1L, max_integer()),
                 Section = factor(1),
                 Stations = c(1L, 9L),
                 Coverage = c(0, 1)),
    key = c("IntervalDeployment", "Section"), select = TRUE)
}

check_detect_capture <- function(capture) {
  values <- list(Capture = c(1L, 1000L),
                 Interval = 1L,
                 Section = 1L,
                 Length = c(200L, 1000L),
                 Reward1 = c(1L, 10L, 100L),
                 Reward2 = c(1L, 10L, 100L),
                 TagExpireInterval = 1L)

  datacheckr::check_data2(capture, values, key = "Capture")
  capture %<>% subset(select = names(values))
  invisible(capture)
}

check_detect_recapture <- function(recapture) {
  values <- list(Interval = 1L,
                 Section = 1L,
                 TBarTag1 = TRUE,
                 TBarTag2 = TRUE,
                 TagsRemoved = TRUE,
                 Released = TRUE)

  datacheckr::check_data2(recapture, values)
  recapture %<>% subset(select = names(values))
  invisible(recapture)
}

check_detect_detection <- function(detection) {

  values <- list(Interval = 1L,
                 Section = 1L,
                 Capture = 1L)

  datacheckr::check_data2(detection, values, key = c("Interval", "Section", "Capture"))
  detection %<>% subset(select = names(values))
  invisible(detection)
}

check_detect_joins <- function(data) {

  datacheckr::check_join(data$coverage, data$section, "Section")
  datacheckr::check_join(data$distance, data$section, "Section")
  datacheckr::check_join(data$capture,  data$section, "Section")
  datacheckr::check_join(data$recapture,  data$section, "Section")
  datacheckr::check_join(data$detection,  data$section, "Section")

  datacheckr::check_join(data$coverage,  data$interval, "Interval")
  datacheckr::check_join(data$capture,  data$interval, "Interval")
  datacheckr::check_join(data$recapture,  data$interval, "Interval")
  datacheckr::check_join(data$detection,  data$interval, "Interval")

  datacheckr::check_join(data$recapture,  data$capture, "Capture", extra = TRUE)
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
