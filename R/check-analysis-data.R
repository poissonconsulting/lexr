check_analysis_section <- function(section) {
  check_data3(
    section, list(Section = factor(1),
                  Habitat = factor(1),
                  Area = c(0, 100),
                  Bounded = TRUE,
                  EastingSection = 1,
                  NorthingSection = 1,
                  ColorCode = factor(1)),
    key = "Section", select = TRUE)
}

check_analysis_distance <- function(distance) {
  if (!is.matrix(distance)) error("distance must be a matrix")
  if (!is.integer(distance)) error("distance must be an integer matrix")
  if (any(is.na(distance))) error("distance must not have missing values")
  if (any(distance < 0)) error("distance must be 0 or greater")
  if (!isSymmetric(unname(distance))) error("distance must be symmetric")

  invisible(distance)
}

check_analysis_period <- function(period) {
  check_data3(
    period, list(Period = factor(1),
                   Date = as.Date("2000-01-01"),
                   Year = c(2000L, 2030L),
                   Month = c(1L, 12L),
                   Hour = c(0L, 23L),
                   DayteTime = Sys.time(),
                   DateTime = Sys.time(),
                   Days = c(1/24, 366)),
    key = c("Period"), select = TRUE)
}

check_analysis_interval <- function(interval) {
  check_data3(
    interval, list(Interval = 1L,
                   Period = factor(1)),
    key = c("Interval"), select = TRUE)
}

check_analysis_coverage <- function(coverage) {
  if (!is.matrix(coverage)) error("coverage must be a matrix")
  if (!is.numeric(coverage)) error("coverage must be an numeric matrix")
  if (any(is.na(coverage))) error("coverage must not have missing values")
  if (any(coverage < 0)) error("coverage must be 0 or greater")
  if (any(coverage > 1)) error("coverage must be 1 or less")

  coverage
}

check_analysis_capture <- function(capture) {
  check_data3(
    capture, list(Capture = factor(1),
                  Species = factor(1),
                  PeriodCapture = factor(1),
                  SectionCapture = factor(1),
                  PeriodTagExpire = factor(1)),
    key = "Capture", select = TRUE)
}

check_analysis_length <- function(length) {
  if (!is.matrix(length)) error("length must be a matrix")
  if (!is.integer(length)) error("length must be an integer matrix")
  if (any(is.na(length))) error("length must not have missing values")

  length
}

check_analysis_recapture <- function(recapture) {
  check_data3(
    recapture, list(PeriodRecapture = factor(1),
                  Capture = factor(1),
                  SectionRecapture = factor(c(1, NA)),
                  Released = TRUE,
                  Recaptures = c(1L, 10L)),
    min_row = 0,
    key = c("PeriodRecapture", "Capture"), select = TRUE)
}

check_analysis_detection <- function(detection) {
  if (!is.array(detection)) error("detection must be an array")
  if (!is.numeric(detection)) error("detection must be an numeric array")
  if (any(is.na(detection))) error("detection must not have missing values")
  if (any(detection < 0)) error("detection must be 0 or greater")
  if (any(detection > 1)) error("detection must be 1 or less")

  detection
}

check_analysis_detected <- function(detected) {
  if (!is.matrix(detected)) error("detected must be a matrix")
  if (!is.logical(detected)) error("detected must be a logical matrix")
  if (any(is.na(detected))) error("detected must not have missing values")

  detected
}

check_analysis_moved <- function(moved) {
  if (!is.matrix(moved)) error("moved must be a matrix")
  if (!is.logical(moved)) error("moved must be a logical matrix")
  if (any(is.na(moved))) error("moved must not have missing values")

  moved
}

check_analysis_reported <- function(reported) {
  if (!is.matrix(reported)) error("reported must be a matrix")
  if (!is.logical(reported)) error("reported must be a logical matrix")
  if (any(is.na(reported))) error("reported must not have missing values")

  reported
}

check_analysis_reward <- function(reward) {
  if (!is.matrix(reward)) error("reward must be a matrix")
  if (!is.integer(reward)) error("reward must be a integer matrix")

  reward
}

check_analysis_tags <- function(tags) {
  if (!is.array(tags)) error("tags must be a matrix")
  if (!is.logical(tags)) error("tags must be a logical array")

  tags
}

check_analysis_released <- function(released) {
  if (!is.matrix(released)) error("released must be a matrix")
  if (!is.logical(released)) error("released must be a logical matrix")

  released
}

check_analysis_removed <- function(removed) {
  if (!is.matrix(removed)) error("removed must be a matrix")
  if (!is.logical(removed)) error("removed must be a logical matrix")
  if (any(is.na(removed))) error("removed must not have missing values")

  removed
}

check_detect_dims <- function(data) {
  nsection <- nrow(data$section)
  nperiod <- nrow(data$period)
  ncapture <- nrow(data$capture)

  stopifnot(identical(dim(data$distance), c(nsection, nsection)))
  stopifnot(identical(dim(data$coverage), c(nsection, nperiod)))
  stopifnot(identical(dim(data$detection), c(ncapture, nperiod, nsection)))
  stopifnot(identical(dim(data$detected), c(ncapture, nperiod)))
  stopifnot(identical(dim(data$moved), c(ncapture, nperiod)))
  stopifnot(identical(dim(data$reported), c(ncapture, nperiod)))
  stopifnot(identical(dim(data$released), c(ncapture, nperiod)))
  stopifnot(identical(dim(data$removed), c(ncapture, nperiod)))
  stopifnot(identical(dim(data$length), c(ncapture, nperiod)))
  stopifnot(identical(dim(data$reward), c(ncapture, 2L)))
  stopifnot(identical(dim(data$tags), c(ncapture, nperiod, 2L)))

  stopifnot(all(!is.null(dimnames(data$distance))))
  stopifnot(all(!is.null(dimnames(data$coverage))))
  stopifnot(all(!is.null(dimnames(data$detection))))
  stopifnot(all(!is.null(dimnames(data$detected))))
  stopifnot(all(!is.null(dimnames(data$moved))))
  stopifnot(all(!is.null(dimnames(data$reported))))
  stopifnot(all(!is.null(dimnames(data$released))))
  stopifnot(all(!is.null(dimnames(data$removed))))
  stopifnot(all(!is.null(dimnames(data$length))))
  stopifnot(all(!is.null(dimnames(data$reward))))
  stopifnot(all(!is.null(dimnames(data$tags))))

  invisible(data)
}

check_analysis_data <- function(data) {
  if (!inherits(data, "analysis_data")) error("data must be a analysis_data object")
  if (!identical(names(data), analysis_data_names())) error("data must have correct names")
  data %<>% purrr::lmap(fun_data_name, fun = "check_analysis")
  check_detect_dims(data)
  class(data) <- "analysis_data"
  invisible(data)
}
