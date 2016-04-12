drop_recaptures_after_harvest <- function(recapture) {
  if (all(recapture$Released))
    return(recapture)
  recapture %<>% dplyr::arrange_(~DateTimeRecapture)
  recapture %<>% dplyr::slice(1:min(which(!recapture$Released)))
  recapture$TagsRemoved[nrow(recapture)] <- TRUE
  recapture
}

filter_lex_captures_recaptures <- function(data, capture, recapture) {
  detection <- data$detection

  capture$Capture %<>% droplevels()
  capture$Species %<>% droplevels()

  recapture %<>% dplyr::filter_(~Capture %in% capture$Capture)
  detection %<>% dplyr::filter_(~Capture %in% capture$Capture)

  recapture$Capture %<>% factor(levels = levels(capture$Capture))
  detection$Capture %<>% factor(levels = levels(capture$Capture))

  recapture %<>% plyr::ddply("Capture", drop_recaptures_after_harvest)

  harvest <- dplyr::filter_(recapture, ~!Released)
  if (nrow(harvest)) { # drop detections after harvest
    detection %<>% dplyr::left_join(dplyr::select_(harvest, ~Capture, ~DateTimeRecapture), by = "Capture")
    detection %<>% dplyr::filter_(~is.na(DateTimeRecapture) | DateTimeDetection < DateTimeRecapture)
    detection$DateTimeRecapture <- NULL
  }

  data$capture <- capture
  data$recapture <- recapture
  data$detection <- detection
  data
}

agg_hab <- function (x) {
  if (length(unique(x)) > 1) {
    is.na(x[1]) <- TRUE
  }
  x[1]
}

combine_sections <- function(section) {
  section <- section[!is.na(section@data$Section),]
  if (!nrow(section)) stop("section must have at least one remaining section")
  if (!all(levels(section@data$Section) %in% section@data$Section))
    stop("section must have all levels")

  # because raster::aggregate drops columns if no section to aggregate
  if (!anyDuplicated(section@data$Section))
    return(section)

#  require("raster") # otherwise aggregate throws error

 section <- raster::aggregate(section, by = "Section", sums = list(
    list(agg_hab, "Habitat"), list(all, "Bounded")))

  section@data <- as.data.frame(dplyr::bind_cols(
  section@data, dplyr::select_(as.data.frame(rgeos::gCentroid(section, byid = TRUE)),
                       EastingSection = ~x, NorthingSection = ~y)))
  row.names(section) <- as.character(section@data$Section)
  row.names(section@data) <- as.character(section@data$Section)

  section <- section[order(section@data$Section),]

  section
}

filter_lex_sections <- function(data, sections) {
  assert_that(is.list(sections) && !is.null(names(sections)))

  station <- data$station
  deployment <- data$deployment
  detection <- data$detection
  capture <- data$capture
  recapture <- data$recapture
  section <- data$section

  levels(section@data$Section) <- sections
  levels(capture$SectionCapture) <- sections
  levels(recapture$SectionRecapture) <- sections
  levels(station$Section) <- sections

  section <- combine_sections(section)

  capture %<>% dplyr::filter_(~!is.na(SectionCapture))
  station %<>% dplyr::filter_(~!is.na(Section))

  deployment %<>% dplyr::filter_(~Station %in% station$Station)
  detection %<>% dplyr::inner_join(deployment, by = "Receiver")
  detection %<>% dplyr::filter_(~DateTimeDetection > DateTimeReceiverIn, ~DateTimeDetection < DateTimeReceiverOut)
  detection %<>% dplyr::select_(~DateTimeDetection, ~Capture, ~Receiver, ~Detections)

  station$Station %<>% droplevels()
  deployment$Station %<>% factor(levels = levels(station$Station))
  deployment$Receiver %<>% droplevels()
  detection$Receiver %<>% factor(levels = levels(deployment$Receiver))

  data$section <- section
  data$recapture <- recapture
  data$capture <- capture
  data$detection <- detection
  data$deployment <- deployment
  data$station <- station
  data$section <- section
  data
}

filter_lex_station <- function (data, station) {
  deployment <- data$deployment
  detection <- data$detection

  station$Station %<>% droplevels()
  deployment %<>% dplyr::filter_(~Station %in% levels(station$Station))
  deployment$Station %<>% factor(levels = levels(station$Station))

  detection %<>% dplyr::inner_join(deployment, by = "Receiver")
  detection %<>% dplyr::filter_(~DateTimeDetection > DateTimeReceiverIn, ~DateTimeDetection < DateTimeReceiverOut)
  detection %<>% dplyr::select_(~DateTimeDetection, ~Capture,  ~Receiver, ~Detections)

  deployment$Receiver %<>% droplevels()
 detection$Receiver %<>% factor(levels = levels(deployment$Receiver))

  data$deployment <- deployment
  data$detection <- detection

  data$station <- station
  data
}

#' Filter Lex Data
#'
#' Filters lex data by capture and recapture.
# #'  and combines and drops section.
#' Drops all recaptures and detections after first coded
#' harvest and ensures coded as tags removed.
#'
#' @inheritParams make_detect_data
#' @param sections A named list of sections to combine (or drop if excluded).
#' @param station A data frame of stations.
#' @return A lex_data object.
#' @export
filter_lex_data <-  function(
  data, capture = data$capture, recapture = data$recapture,
  sections = stats::setNames(as.list(levels(data$section@data$Section)), levels(data$section@data$Section)),
  station = data$station,
    start_date = min(lubridate::date(capture$DateTimeCapture)),
  end_date = max(lubridate::date(capture$DateTimeTagExpire))) {

  force(sections)
  check_date(start_date)
  check_date(end_date)

  if (end_date <= start_date) error("start_date must be before end_date")

  capture %<>% check_lex_capture()
  station %<>% check_lex_station()
  data %<>% check_lex_data()

  capture %<>% dplyr::filter_(~lubridate::date(DateTimeCapture) >= start_date,
                              ~lubridate::date(DateTimeCapture) <= end_date)

  if (!nrow(capture)) error("no captures fall within the specified dates")

  recapture %<>% dplyr::filter_(~lubridate::date(DateTimeRecapture) >= start_date,
                              ~lubridate::date(DateTimeRecapture) <= end_date)

  data$station <- station
  data %<>% filter_lex_captures_recaptures(capture, recapture)
  data %<>% filter_lex_sections(sections)
  data %<>% filter_lex_captures_recaptures(data$capture, data$recapture)
  data %<>% filter_lex_station(data$station)

  data$recapture %<>% check_lex_recapture()

  data <- data[lex_data_names()]
  class(data) <- "lex_data"
  check_lex_data(data)
}
