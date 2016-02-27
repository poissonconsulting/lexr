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
  if (!isSymmetric(distance)) error("distance must be symmetric")

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
                   DateTime = Sys.time()),
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
                  Length = c(200L, 1000L),
                  Reward1 = factor(rep("Low", 3), levels = c("Low", "High")),
                  Reward2 = factor(c(rep("Low", 3), NA), levels = c("Low", "High")),
                  PeriodTagExpire = factor(1)),
    key = "Capture", select = TRUE)
}

check_analysis_recapture <- function(recapture) {
  check_data3(
    recapture, list(PeriodRecapture = factor(1),
                  Capture = factor(1),
                  SectionRecapture = factor(c(1, NA)),
                  TBarTag1 = TRUE,
                  TBarTag2 = TRUE,
                  TagsRemoved = TRUE,
                  Released = TRUE,
                  Public = TRUE,
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


check_detect_dims <- function(data) {
  nsection <- nrow(data$section)
  nperiod <- nrow(data$period)
  ncapture <- nrow(data$capture)

  stopifnot(identical(dim(data$distance), c(nsection, nsection)))
  stopifnot(identical(dim(data$coverage), c(nsection, nperiod)))
  stopifnot(identical(dim(data$detection), c(ncapture, nperiod, nsection)))

  stopifnot(all(!is.null(dimnames(data$distance))))
  stopifnot(all(!is.null(dimnames(data$coverage))))
  stopifnot(all(!is.null(dimnames(data$detection))))

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