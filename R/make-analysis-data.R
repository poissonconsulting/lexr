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

# factors_to_integers <- function (data) {
#   data[sapply(data, is.factor)] %<>% lapply(as.integer)
#   data
# }
#
# logicals_to_integers <- function (data) {
#   data[sapply(data, is.logical)] %<>% lapply(as.integer)
#   data
# }

make_analysis_section <- function(data) {
  message("making analysis section...")

  data$section <- dplyr::as.tbl(data$section@data)
  data$section$ColorCode %<>% factor() # hack to deal with fact jaggernaut rejects character
  data
}

make_analysis_distance <- function(data) {
  message("making analysis distance...")

  sections <- levels(data$distance$SectionFrom)
  data$distance %<>% dplyr::arrange_(~SectionFrom, ~SectionTo)
  distance <- matrix(data$distance$Distance, nrow = length(sections), ncol = length(sections), dimnames = list(SectionFrom = sections, SectionTo = sections))

  data$distance <- distance
  data
}

rounded_hours <- function (x) {
  stopifnot(lubridate::is.difftime(x))
  x %<>% lubridate::as.duration() %>% as.integer() %>%
    magrittr::divide_by(60 * 60) %>% round()
  x
}

make_analysis_interval <- function(data, interval_period) {
  message("making analysis interval...")

  if (lubridate::is.difftime(interval_period)) {
    if (length(interval_period) != 1) error("interval_period as a difftime must be length 1")
    if (interval_period < get_difftime(data)) error("interval_period as a difftime must not be less than data's")
    if (interval_period > lubridate::make_difftime(60 * 60 * 24 * 28)) error("interval_period as a difftime must not be greater than 28 days")
    if (interval_period == get_difftime(data)) {
      interval_period <- data$interval$Interval
    } else {
      interval_period %<>% rounded_hours()
      difftime <- get_difftime(data) %>% rounded_hours()
      if (interval_period %% difftime != 0)
        error("interval_period as a difftime must be a multiple of data's")
      interval_period <- interval_period / difftime
      interval_period <- rep(1:ceiling(nrow(data$interval) / interval_period), each = interval_period)
      interval_period <- interval_period[1:nrow(data$interval)]
    }
  } else {
    if (length(interval_period) != nrow(data$interval))
      error("interval_period as a vector must be the same length as the number of intervals")
  }

  data$interval$Period <- interval_period
  data$interval %<>% dplyr::arrange_(~Interval)
  data$interval$Period %<>% factor(levels = unique(.))
  data$period <- plyr::ddply(data$interval, "Period", dplyr::slice, 1)
  data$period %<>% dplyr::select_(~-Interval)
  data$period %<>% dplyr::select_(~Period, ~everything())
  data$interval %<>% dplyr::select_(~Period, ~Interval)

  data$period %<>% dplyr::as.tbl()
  data$interval %<>% dplyr::as.tbl()
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

replace_interval_with_period <- function(x, data, suffix = "") {
  interval <- data$interval
  if (!identical(suffix, "")) {
    interval[paste0("Period", suffix)] <- interval["Period"]
    interval["Period"] <- NULL
  }
  by = "Interval"
  names(by) <- paste0("Interval", suffix)
  x %<>% dplyr::left_join(interval, by = by)
  x[paste0("Interval", suffix)] <- NULL
  x
}

make_analysis_alive <- function(data) {
  message("making analysis alive...")

  intervals <- nrow(data$interval)
  captures <- nrow(data$capture)
  alive <- matrix(NA, nrow = captures, ncol = intervals)
  dimnames(alive) <- list(Capture = data$capture$Capture, Interval = data$interval$Interval)

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
  alive %<>% reshape2::melt(as.is = TRUE, value.name = "Alive")
  alive$Capture %<>% factor(levels = levels(data$capture$Capture))
  alive$Interval %<>% as.integer()
  alive %<>% replace_interval_with_period(data)
  alive %<>% dplyr::group_by_(~Capture, ~Period) %>%
    dplyr::summarise_(.dots = list(Alive = ~any(Alive))) %>% dplyr::ungroup()
  alive %<>% reshape2::acast(list(plyr::as.quoted(~Capture),
                                  plyr::as.quoted(~Period)),
                             drop = FALSE, value.var = "Alive")
  dimnames(alive) <- list(Capture = data$capture$Capture, Period = data$period$Period)
  data$alive <- alive
  data
}

make_analysis_capture <- function(data) {
  message("making analysis capture...")

  data$capture %<>% replace_interval_with_period(data, "Capture")
  data$capture %<>% replace_interval_with_period(data, "TagExpire")
  data$capture %<>% dplyr::as.tbl()
  data
}

group_recaptures <- function (recapture) {
  if (nrow(recapture) == 1)
    return(recapture)
  is.na(recapture$SectionRecapture) <- TRUE
  recapture$TBarTag1 <- any(recapture$TBarTag1)
  recapture$TBarTag2 <- any(recapture$TBarTag2)
  recapture$TagsRemoved <- any(recapture$TagsRemoved)
  recapture$Released <- all(recapture$TagsRemoved)
  recapture$Public <- any(recapture$Public)

  dplyr::slice(recapture, 1)
}

make_analysis_recapture <- function(data) {
  message("making analysis recapture...")

  data$recapture %<>% replace_interval_with_period(data, "Recapture")
  data$recapture %<>% dplyr::mutate_(.dots = list(Recaptures = ~1L))
  data$recapture %<>% plyr::ddply(c("Capture", "PeriodRecapture"), group_recaptures)

  data$recapture %<>% dplyr::as.tbl()
  data
}

make_analysis_reported <- function(data) {
  message("making analysis reported...")

  reported <- data$recapture
  reported$Reported <- TRUE
  reported %<>% reshape2::acast(list(plyr::as.quoted(~Capture),
                                  plyr::as.quoted(~PeriodRecapture)),
                                fill = FALSE, drop = FALSE, value.var = "Reported")
  dimnames(reported) <- list(Capture = data$capture$Capture, Period = data$period$Period)
  data$reported <- reported
  data
}

make_analysis_coverage <- function(data) {
  message("making analysis coverage...")

  data$coverage %<>% dplyr::select_(~Interval, ~Section, ~Coverage)

  all <- expand.grid(Interval = data$interval$Interval,
                     Section = data$section$Section)

  coverage <- dplyr::left_join(all, data$coverage,
                               by = c("Interval", "Section"))

  coverage$Coverage[is.na(coverage$Coverage)] <- 0

  coverage %<>% replace_interval_with_period(data)
  coverage %<>% dplyr::group_by_(~Section, ~Period) %>%
    dplyr::summarise_(.dots = list(Coverage = ~mean(Coverage))) %>% dplyr::ungroup()

  coverage %<>% tidyr::spread_("Period", "Coverage")
  coverage$Section <- NULL
  coverage %<>% as.matrix()
  dimnames(coverage) <- list(Section = data$section$Section, Period = data$period$Period)
  data$coverage <- coverage
  data
}

make_analysis_detection <- function(data) {
  message("making analysis detection...")

  detection <- dplyr::select_(data$detection, ~IntervalDetection, ~Capture, ~Section)
  detection %<>% replace_interval_with_period(data, "Detection")

  all <- expand.grid(PeriodDetection = data$period$Period,
                     Capture = data$capture$Capture, Section = data$section$Section)

  detection$Periods <- 1
  detection %<>% dplyr::left_join(all, .,
                                  by = c("PeriodDetection", "Capture", "Section"))

  detection$Periods[is.na(detection$Periods)] <- 0

  detection %<>% dplyr::group_by_(~PeriodDetection, ~Capture, ~Section) %>%
    dplyr::summarise_(.dots = list(Periods = ~mean(Periods))) %>% dplyr::ungroup()

  detection %<>% reshape2::acast(list(plyr::as.quoted(~Capture),
                                      plyr::as.quoted(~PeriodDetection),
                                      plyr::as.quoted(~Section)),
                                 drop = FALSE, value.var = "Periods")
  dimnames(detection) <- list(Capture = data$capture$Capture,
                              Period = data$period$Period,
                              Section = data$section$Section)
  data$detection <- detection
  data
}

cleanup_analysis_data <- function (data) {
  data <- data[analysis_data_names()]
  class(data) <- "analysis_data"
  data
}

#' Make Analysis Data
#'
#' If a difftime element, interval_period cannot be greater than 28 days
#' i.e. \code{lubridate::make_difftime(60 * 60 * 24 * 28)}.
#'
#' Makes analysis_data object from a detect_data object.
#' @param data A detect_data object to use.
#' @param capture A data frame of the capture data to use.
#' @param section A data frame of the section data to use.
#' @param interval_period A difftime element that will be used to group the interval or
#' a vector indicating the actual interval groupings.
#'
#' @return A detect_data object.
#' @export
make_analysis_data <-  function(
  data, capture = data$capture, section = data$section, interval_period = get_difftime(data)) {

  data %<>% check_detect_data()
  capture %<>% check_detect_capture()
  section %<>% check_detect_section()

  data %<>% filter_detect_captures_section(capture, section)
  data %<>% check_detect_data()

  data %<>% make_analysis_section()
  data %<>% make_analysis_distance()

  data %<>% make_analysis_interval(interval_period)
  data %<>% make_analysis_alive()

  data %<>% make_analysis_capture()
  data %<>% make_analysis_recapture()
  data %<>% make_analysis_reported()
  data %<>% make_analysis_coverage()
  data %<>% make_analysis_detection()
  data %<>% cleanup_analysis_data()

  data %<>% check_analysis_data()
  data
}
