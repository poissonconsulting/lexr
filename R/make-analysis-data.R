filter_detect_captures_section <- function(data, capture, section) {

  section@data$Section %<>% droplevels()

  data$distance %<>% dplyr::filter_(~SectionFrom %in% section@data$Section)
  data$distance %<>% dplyr::filter_(~SectionTo %in% section@data$Section)
  data$coverage <- data$coverage[data$coverage$Section  %in% section@data$Section,]
  data$capture %<>% dplyr::filter_(~SectionCapture %in% section@data$Section)
  data$detection <- data$detection[data$detection$Section  %in% section@data$Section,]

  data$distance$SectionFrom %<>% factor(levels = levels(section@data$Section))
  data$distance$SectionTo %<>% factor(levels = levels(section@data$Section))
  data$coverage$Section %<>% factor(levels = levels(section@data$Section))
  data$capture$SectionCapture %<>% factor(levels = levels(section@data$Section))
  # sets missing recapture sections to NA
  data$recapture$SectionRecapture %<>% factor(levels = levels(section@data$Section))
  data$detection$Section %<>% factor(levels = levels(section@data$Section))

  capture$Capture %<>% droplevels()

  data$recapture %<>% dplyr::filter_(~Capture %in% capture$Capture)
  data$detection %<>% dplyr::filter_(~Capture %in% capture$Capture)

  data$recapture$Capture %<>% factor(levels = levels(capture$Capture))
  data$detection$Capture %<>% factor(levels = levels(capture$Capture))

  data$section <- section
  data$capture <- capture
  data
}

factors_to_integers <- function (data) {
  data[sapply(data, is.factor)] %<>% lapply(as.integer)
  data
}

logicals_to_integers <- function (data) {
  data[sapply(data, is.logical)] %<>% lapply(as.integer)
  data
}

make_analysis_section <- function(data) {
  message("making analysis section...")

  data$section <- data$section@data
  data$section %<>% dplyr::select_(~Section, ~Habitat, ~Area, ~Bounded)
  data$section %<>% factors_to_integers()
  data$section %<>% logicals_to_integers()
  data
}

make_analysis_coverage <- function(data) {
  message("making analysis coverage...")

  data$coverage %<>% dplyr::select_(~Interval, ~Section, ~Coverage)
  data$coverage %<>% factors_to_integers()

  all <- expand.grid(Interval = data$interval$Interval,
                     Section = data$section$Section)

  coverage <- dplyr::left_join(all, data$coverage,
                               by = c("Interval", "Section"))

  coverage$Coverage[is.na(coverage$Coverage)] <- 0

  coverage %<>% tidyr::spread_("Interval", "Coverage")
  coverage$Section <- NULL
  data$coverage <- as.matrix(coverage)
  data
}

make_analysis_interval <- function(data) {
  message("making analysis interval...")

  data$interval %<>% dplyr::select_(~Interval, ~Year, ~Month, ~Hour)
  data
}

make_analysis_capture <- function(data) {
  message("making analysis capture...")

  data$capture %<>% factors_to_integers()
  data$capture %<>% logicals_to_integers()
  data
}

make_analysis_recapture <- function(data) {
  message("making analysis recapture...")

  data$recapture %<>% factors_to_integers()
  data$recapture %<>% logicals_to_integers()
  data
}

last_movement <- function(data) {
  if (nrow(data) == 1)
    return(NULL)
  data %<>% dplyr::arrange_(~IntervalDetection)
  # can only assume alive at the section it moved from
  data$Move <- c(diff(as.integer(data$Section)) != 0, FALSE)
  whch <- which(data$Move)
  data.frame(Interval = data$IntervalDetection[whch[length(whch)]])
}

make_analysis_alive <- function(data) {
  message("making analysis alive...")

  intervals <- nrow(data$interval)
  captures <- nrow(data$capture)
  alive <- matrix(NA, nrow = captures, ncol = intervals)

  data$capture %<>% dplyr::arrange_(~Capture)

  for (i in 1:captures) {
    alive[i, 1:data$capture$IntervalCapture[i]] <- TRUE
  }
  if (nrow(data$recapture)) {
    for (i in 1:nrow(data$recapture)) {
      alive[data$recapture$Capture[i], 1:data$recapture$IntervalRecapture[i]] <- TRUE
    }
    retained <- dplyr::filter_(data$recapture, ~!Released, ~IntervalRecapture < intervals)
    if (nrow(retained)) {
      for (i in 1:nrow(retained)) {
        alive[retained$Capture[i], (retained$IntervalRecapture[i] + 1):intervals] <- FALSE
      }
    }
  }
  move <- plyr::ddply(data$detection, "Capture", last_movement)
  move$Capture %<>% as.integer()
  if (nrow(move)) {
    for (i in 1:nrow(move)) {
      alive[move$Capture[i], 1:move$Interval[i]] <- TRUE
    }
  }
  data$alive <- alive
  data
}

make_analysis_detection <- function(data) {
  message("making analysis detection...")

  data$detection %<>% dplyr::select_(~IntervalDetection, ~Capture, ~Section)
  data$detection %<>% factors_to_integers()

  all <- expand.grid(IntervalDetection = data$interval$Interval,
                     Capture = data$capture$Capture)

  detection <- dplyr::left_join(all, data$detection,
                                by = c("IntervalDetection", "Capture"))
  detection %<>% tidyr::spread_("IntervalDetection", "Section")
  detection$Capture <- NULL
  data$detection <- as.matrix(detection)
  data
}

make_analysis_distance <- function(data) {
  message("making analysis distance...")

  sections <- levels(data$distance$SectionFrom)
  data$distance %<>% dplyr::arrange_(~SectionFrom, ~SectionTo)
  distance <- matrix(data$distance$Distance, nrow = length(sections), ncol = length(sections), dimnames = list(sections, sections))

  data$distance <- distance
  data$step <- data$distance == 1
  data
}

convert_analysis_data <- function (data) {
  list <- list()
  list$nSection <- nrow(data$section)
  list$Step <- data$step
  list$Distance <- data$distance
  list$nInterval <- nrow(data$interval)
  list$Coverage <- data$coverage
  list$nCapture <- nrow(data$capture)
  list$IntervalCapture <- data$capture$IntervalCapture
  list$SectionCapture <- data$capture$SectionCapture
  list$IntervalTagExpire <- data$capture$IntervalTagExpire
  list$Monitored <- data$monitored
  list$Detection <- data$detection
  list$Alive <- data$alive
  list
}

#' Make Analysis Data
#'
#' Makes analysis_data object from a detect_data object.
#' @param data A detect_data object to use.
#' @param capture A data frame of the capture data to use.
#' @param section A data frame of the section data to use.
#'
#' @return A detect_data object.
#' @export
make_analysis_data <-  function(
  data, capture = data$capture, section = data$section) {

  data %<>% check_detect_data()
  capture %<>% check_detect_capture()
  section %<>% check_detect_section()

  data %<>% filter_detect_captures_section(capture, section)
  data %<>% check_detect_data()
  data %<>% make_analysis_section()
  data %<>% make_analysis_distance()
  data %<>% make_analysis_capture()
  data %<>% make_analysis_recapture()
  data %<>% make_analysis_interval()
  data %<>% make_analysis_coverage()
  data %<>% make_analysis_alive()
  data %<>% make_analysis_detection()
  data <- data[analysis_data_names()]
  class(data) <- "analysis_data"
  data %<>% check_analysis_data()
  data %<>% convert_analysis_data()
  data
}
