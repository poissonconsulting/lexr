expand_deployment_row <- function(x) {
  stopifnot(nrow(x) == 1)
  data <- dplyr::data_frame(
    IntervalDeployment = x$IntervalReceiverIn:x$IntervalReceiverOut)
  x %<>% merge(data)
  x$IntervalReceiverIn <- NULL
  x$IntervalReceiverOut <- NULL
  x
}

expand_deployment <- function(deployment) {
  if (any(deployment$IntervalDateTimeIn < deployment$IntervalDateTimeOut))
    error("receiver deployed before retrieved")
  deployment %<>% plyr::adply(1, expand_deployment_row)
  stopifnot(!anyDuplicated(deployment))
  deployment
}

add_coverage_code <- function(x) {
  x %<>% dplyr::arrange_(~Station)
  x$CoverageCode <- paste0(x$Station, collapse = "")
  x
}

circles_intersection <- function (x, section) {
  radius <- 500
  circles <- sampSurf::spCircle(
    radius = radius, spUnits = sp::CRS(sp::proj4string(section)),
    centerPoint = c(x = x$EastingStation[1], y = x$NorthingStation[1]),
    spID = x$Station[1])$spCircle

  if (nrow(x) > 1) {
    for (i in 2:nrow(x)) {
      circle <- sampSurf::spCircle(
        radius = radius, spUnits = sp::CRS(sp::proj4string(section)),
        centerPoint = c(x = x$EastingStation[i], y = x$NorthingStation[i]),
        spID = x$Station[i])$spCircle
      circles <- rgeos::gUnion(circles, circle)
    }
  }
  sp::proj4string(circles) <- sp::proj4string(section)
  circles
}

calc_coverage_code_interval <- function(y, section) {
  stopifnot(nrow(section@data) == 1)

  circles <- circles_intersection(y, section)
  circles <- rgeos::gIntersection(circles, section)

  coverage <- (rgeos::gArea(circles) / 10 ^ 6) / section@data$Area
  coverage
}

calc_coverage_code <- function(x, section) {
  x %<>% dplyr::arrange_(~IntervalDeployment)
  y <- dplyr::filter_(x, ~IntervalDeployment == x$IntervalDeployment[1]) %>% dplyr::as.tbl()
  x$Coverage <- calc_coverage_code_interval(y, section)
  x %<>% dplyr::group_by_(~IntervalDeployment, ~Section) %>%
    dplyr::summarise_(.dots = list(Stations = "n()", Coverage = "first(Coverage)"))
  x
}

check_consecutive_dups <- function(duplicates) {
  intervals <- duplicates$IntervalDeployment
  intervals %<>% sort() %>% diff()
  if (any(intervals == 1))
    error("multiple receivers at the same station")
  NULL
}

calc_coverage <- function(data, section) {
  section <- section[section@data$Section == data$Section[1],]
  data$Station %<>% droplevels() %>% as.integer()

  if (anyDuplicated(data[c("IntervalDeployment", "Station")])) {
    duplicates <- data[c("IntervalDeployment", "Station")]
    data <- data[!duplicated(duplicates),,drop = FALSE]
    duplicates <- duplicates[duplicated(duplicates),,drop = FALSE]
    plyr::ddply(duplicates, "Station", check_consecutive_dups)
  }

  data %<>% plyr::ddply("IntervalDeployment", add_coverage_code)
  data %<>% plyr::ddply("CoverageCode", calc_coverage_code, section)
  data$CoverageCode <- NULL
  data
}

make_coverage <- function(data) {
  message("making coverage...")
  deployment <- data$deployment
  deployment %<>% expand_deployment()

  data$section@data$Area <- rgeos::gArea(data$section, byid = TRUE) / 10 ^ 6
  coverage <- dplyr::inner_join(data$station, deployment, by = "Station")
  coverage %<>% dplyr::select_(~IntervalDeployment, ~Section, ~Station,
                               ~EastingStation, ~NorthingStation)

  coverage %<>% plyr::ddply(c("Section"), calc_coverage, section = data$section)
  coverage %<>% dplyr::rename_(.dots = list(Interval = "IntervalDeployment"))
  data$coverage <- dplyr::as.tbl(coverage)
  data
}

set_interval <- function(col, interval) {
  if (!length(col))
    return(integer(0))
  lubridate::minute(col) <- 0
  lubridate::second(col) <- 0

  hourly_interval <- difftime(interval$DateTime[2], interval$DateTime[1], units = "hours") %>%
    as.integer()
  lubridate::hour(col) <- lubridate::hour(col) %/% hourly_interval * hourly_interval
  data <- dplyr::data_frame(DateTime = col)
  data %<>% dplyr::left_join(interval, by = "DateTime")
  data$Interval
}

adjust_datetime_section <- function(section, start_date, end_date) {
  section
}

adjust_datetime_station <- function(station, start_date, end_date) {
  station
}

adjust_datetime_deployment <- function(deployment, start_date, end_date) {
  deployment %<>% dplyr::filter_(~DateTimeReceiverIn <= end_date)
  deployment %<>% dplyr::filter_(~DateTimeReceiverOut >= start_date)
  deployment$DateTimeReceiverIn[deployment$DateTimeReceiverIn < start_date] <- start_date
  deployment$DateTimeReceiverOut[deployment$DateTimeReceiverOut > end_date] <- end_date
  deployment
}

adjust_datetime_capture <- function (capture, start_date, end_date) {
  capture %<>% dplyr::filter_(~DateTimeCapture >= start_date)
  capture %<>% dplyr::filter_(~DateTimeCapture <= end_date)
  capture$DateTimeTagExpire[capture$DateTimeTagExpire > end_date] <- end_date
  capture
}

adjust_datetime_recapture <- function (recapture, start_date, end_date) {
  recapture %<>% dplyr::filter_(~DateTimeRecapture >= start_date)
  recapture %<>% dplyr::filter_(~DateTimeRecapture <= end_date)
  recapture
}

adjust_datetime_detection <- function(detection, start_date, end_date) {
  detection %<>% dplyr::filter_(~DateTimeDetection >= start_date)
  detection %<>% dplyr::filter_(~DateTimeDetection <= end_date)
  detection
}

set_intervals <- function(data, interval) {
  . <- NULL
  colnames <- colnames(data)
  colnames <- colnames[grepl("^DateTime.+", colnames)]
  if (length(colnames)) {
    data[colnames] %<>% lapply(set_interval, interval = interval)
    data[colnames] %<>% na.omit() # delete rows with missing intervals
    colnames(data) %<>% sub("^(DateTime)(.+)$", "Interval\\2" , .)
  }
  data
}

make_interval <- function(data, start_date, end_date, hourly_interval) {
  message("making interval...")
  . <- NULL

  tz <- lubridate::tz(data$capture$DateTimeCapture)

  start_date %<>% paste("00:00:00") %>% as.POSIXct(tz = tz)
  end_date %<>% paste("23:59:59") %>% as.POSIXct(tz = tz)

  data %<>% purrr::lmap(fun_data_name, fun = "adjust_datetime", start_date, end_date)

  interval <- dplyr::data_frame(DateTime = seq(start_date, end_date, by = paste(hourly_interval,"hours")))
  interval %<>% dplyr::mutate_(.dots = list(Interval = ~1:nrow(.),
                                            Date = ~lubridate::date(DateTime),
                                            DayteTime = ~DateTime,
                                            Year = ~as.integer(lubridate::year(DateTime)),
                                            Month = ~as.integer(lubridate::month(DateTime)),
                                            Hour = ~lubridate::hour(DateTime)))
  lubridate::year(interval$DayteTime) <- 2000
  interval %<>% dplyr::select_(~Interval, ~Date, ~Year, ~Month, ~Hour, ~DayteTime, ~DateTime)
  data$interval <- interval
  data %<>% lapply(set_intervals, interval = .$interval)
  data
}

make_capture <- function(data) {
  message("making capture...")
  capture <- data$capture
  capture %<>% dplyr::select_(~Capture, ~IntervalCapture, ~SectionCapture,
                              ~IntervalTagExpire, ~Length, ~Weight,
                              ~Reward1, ~Reward2, ~Species)
  data$capture <- capture
  data
}

make_distance <- function(data) {
  message("making distance...")
  neighbours <- spdep::poly2nb(data$section, row.names = data$section@data$Section)
  adjacency <- matrix(FALSE, nrow = nrow(data$section@data), ncol = nrow(data$section@data))
  for (i in 1:nrow(data$section@data))
    adjacency[i,neighbours[[i]]] <- TRUE

  diag(adjacency) <- TRUE
  stopifnot(isSymmetric(adjacency))
  distance <- spa::uDist(adjacency, nrow(adjacency))
  dim <- dim(distance)
  distance <- as.integer(distance)
  dim(distance) <- dim
  section_names <- as.character(data$section@data$Section)
  colnames(distance) <- section_names
  distance %<>% as.data.frame()
  distance$SectionFrom <- factor(section_names, levels = levels(data$section@data$Section))
  distance %<>% tidyr::gather_("SectionTo", "Distance", section_names)
  distance$Distance %<>% as.integer()
  distance$SectionTo %<>% factor(levels = levels(data$section@data$Section))
  data$distance <- dplyr::as.tbl(distance)
  data
}

sum_detections <- function(data) {
  Detections <- sum(data$Detections)
  Receivers <- length(unique(data$Receiver))
  dplyr::data_frame(Receivers = Receivers, Detections = Detections)
}

filter_detections <- function(data, section) {
  data$Sections <- nrow(data)
  if (nrow(data) == 1)
    return(data)

  data %<>% dplyr::arrange_(~-Detections, ~Receivers, ~Area) # area for tiebreaker
  dplyr::slice(data, 1)
}

non_zero <- function(x) {
  x[x < 0] <- 0
  x
}

set_jumps <- function (data, distance) {
  data %<>% dplyr::arrange_(~IntervalDetection)
  data$SectionFrom <- c(data$Section[1], data$Section[-nrow(data)])
  data$SectionFrom <- factor(x = levels(data$Section)[data$SectionFrom],
                             levels = levels(data$Section))
  data$Intervals <- c(0, diff(data$IntervalDetection))
  data %<>% dplyr::inner_join(distance, by = c(SectionFrom = "SectionFrom", Section = "SectionTo"))
  data %<>% dplyr::mutate_(.dots = list(Jump = ~as.integer(non_zero(Distance - Intervals))))
}

make_detection <- function(data) {
  message("making detection...")
  detection <- data$detection
  detection %<>% dplyr::inner_join(data$deployment, by = "Receiver")
  detection %<>% dplyr::filter_(~IntervalDetection >= IntervalReceiverIn,
                                ~IntervalDetection <= IntervalReceiverOut)

  detection %<>% dplyr::inner_join(data$station, by = "Station")
  detection %<>% plyr::ddply(c("IntervalDetection", "Section", "Capture"), sum_detections)

  capture <- dplyr::select_(data$capture, .dots = list(
    IntervalDetection = "IntervalCapture", Section = "SectionCapture", Capture = "Capture"))

  capture %<>% dplyr::mutate_(.dots = list(Receivers = max_integer(),
                                           Detections = max_integer()))

  detection %<>% dplyr::bind_rows(capture)

  detection %<>% dplyr::inner_join(dplyr::select_(data$section@data, ~Section, ~Area), by = "Section")
  detection %<>% plyr::ddply(c("IntervalDetection", "Capture"), filter_detections)
  detection %<>% plyr::ddply("Capture", set_jumps, data$distance)
  detection %<>% dplyr::select_(~IntervalDetection, ~Section, ~Capture, ~Receivers,
                                ~Detections, ~Sections, ~Jump)
  detection %<>% dplyr::filter_(~Receivers < max_integer())
  data$detection <- dplyr::as.tbl(detection)
  data
}

proportion_range <- function (x, na.rm = FALSE) {
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}

make_section <- function(data) {
  message("making section...")
  red <- proportion_range(data$section@data$Easting)
  blue <- proportion_range(data$section@data$Northing)
  data$section@data$ColorCode <- grDevices::rgb(red = red, blue = blue, green = 0.5)
  data
}

keep_morts <- function (x) {
  x %<>% dplyr::arrange_(~DateDetection)
  x %<>% dplyr::filter_(~DateDetection > DateRecovery)
  sections <- length(unique(x$Section))
  if (sections > 1) # two or more sections after date recovery
    return(NULL)
  return(dplyr::data_frame(Mortality = TRUE)) # so data row
}

filter_lex_captures <- function(data, capture) {
  detection <- data$detection
  recapture <- data$recapture

  capture$Capture %<>% droplevels()
  capture$Species %<>% droplevels()

  recapture %<>% dplyr::filter_(~Capture %in% capture$Capture)
  detection %<>% dplyr::filter_(~Capture %in% capture$Capture)

  recapture$Capture %<>% factor(levels = levels(capture$Capture))
  detection$Capture %<>% factor(levels = levels(capture$Capture))

  data$capture <- capture
  data$recapture <- recapture
  data$detection <- detection
  data
}

filter_recovery_days <- function (data, recovery_days) {
  if (recovery_days == 0) {
    return(data)
  }
  morts <- data$capture
  morts %<>% dplyr::inner_join(dplyr::select_(data$interval, .dots = list(
    IntervalCapture = ~Interval, DateCapture = ~Date)), by = c("IntervalCapture"))
  morts %<>% dplyr::inner_join(dplyr::select_(data$interval, .dots = list(
    IntervalTagExpire = ~Interval, DateTagExpire = ~Date)), by = c("IntervalTagExpire"))
  morts %<>% dplyr::mutate_(.dots = list(TagDays = ~as.integer(DateTagExpire) - as.integer(DateCapture)))
  morts %<>% dplyr::mutate_(.dots = list(DateRecovery = ~DateCapture + recovery_days))
  # assume no morts among fish without acoustic tags
  morts %<>% dplyr::filter_(~TagDays > 0)
  recaps <- dplyr::group_by_(data$recapture, ~Capture) %>% dplyr::summarise_(.dots = list(IntervalRecapture = ~max(IntervalRecapture))) %>% dplyr::ungroup()
  recaps %<>% dplyr::inner_join(dplyr::select_(data$interval, .dots = list(
    IntervalRecapture = ~Interval, DateRecapture = ~Date)), by = c("IntervalRecapture"))
  morts %<>% dplyr::left_join(recaps, by = "Capture")
  # recaptured after recovery are not morts
  morts %<>% dplyr::filter_(~is.na(DateRecapture) | DateRecapture <= DateRecovery)
  detection <- data$detection
  detection %<>% dplyr::inner_join(dplyr::select_(data$interval, .dots = list(
    IntervalDetection = ~Interval, DateDetection = ~Date)), by = c("IntervalDetection"))
  morts %<>% dplyr::left_join(detection, by = "Capture")
  morts %<>% plyr::ddply("Capture", keep_morts)
  capture <- dplyr::filter_(data$capture, ~!Capture %in% morts$Capture)

  filter_lex_captures(data, capture)
}


#' Make Detect Data
#'
#' Makes detect_data object from a lex_data object. The hourly interval can be
#' 1, 2, 3, 4, 6, 12 or 24.
#'
#' If the individual has an active
#' transmitter and no movement is detected after the recovery_days and it is not recaptured
#' then it is considered a handling mortality and removed from the data set.
#'
#' @param data The lex_data object.
#' @param capture A data frame of the capture data to use.
#' @param recapture A data frame of the recapture data to use.
#' @param start_date A date of the start.
#' @param end_date A date of the end.
#' @param hourly_interval A count indicating the hourly interval.
#' @param recovery_days A count indicating the number of days during which
#' fish are recovering from acoustic tagging.
#' @return A detect_data object.
#' @export
make_detect_data <-  function(
  data, capture = data$capture, recapture = data$recapture,
  start_date = min(lubridate::date(capture$DateTimeCapture)),
  end_date = max(lubridate::date(capture$DateTimeTagExpire)),
  hourly_interval = 24L, recovery_days = 0L) {

  check_scalar(hourly_interval, c(1L,2L,3L,4L,6L,12L,24L))
  check_scalar(recovery_days, c(0L, 365L))

  check_date(start_date)
  check_date(end_date)

  if (end_date <= start_date) error("start_date must be before end_date")

  data %<>% filter_lex_data(capture = capture, recapture = recapture,
                            start_date = start_date,
                            end_date = end_date)

  data %<>% make_interval(start_date = start_date, end_date = end_date,
                          hourly_interval = hourly_interval)
  data %<>% make_coverage()
  data %<>% make_capture()
  data %<>% make_distance()
  data %<>% make_detection()
  data %<>% make_section()
  data %<>% filter_recovery_days(recovery_days)
  data <- data[detect_data_names()]
  class(data) <- "detect_data"
  check_detect_data(data)
}
