filter_detect_captures_section <- function(data, capture, section) {

  section@data$Section %<>% droplevels()

  data$distance %<>% dplyr::filter_(~SectionFrom %in% section@data$Section)
  data$distance %<>% dplyr::filter_(~SectionTo %in% section@data$Section)
  data$coverage <- data$coverag[data$coverage$Section  %in% section@data$Section,]
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
  distance <- matrix(data$distance$Distance, nrow = length(sections), ncol = length(sections))
  dimnames(distance) <- list(SectionFrom = sections, SectionTo = sections)

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

  diff_hours <- get_difftime(data) %>% rounded_hours()

  if (lubridate::is.difftime(interval_period)) {
    if (length(interval_period) != 1) error("interval_period as a difftime must be length 1")
    if (interval_period < get_difftime(data)) error("interval_period as a difftime must not be less than data's")
    if (interval_period > lubridate::make_difftime(60 * 60 * 24 * 28)) error("interval_period as a difftime must not be greater than 28 days")
    if (interval_period == get_difftime(data)) {
      interval_period <- data$interval$Interval
    } else {
      interval_period %<>% rounded_hours()
      if (interval_period %% diff_hours != 0)
        error("interval_period as a difftime must be a multiple of data's")
      interval_period <- interval_period / diff_hours
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
  data$period <- plyr::ddply(data$interval, "Period", function(x) {x$Days = nrow(x) * diff_hours /24 ; x})
  data$period %<>% plyr::ddply("Period", dplyr::slice, 1)
  data$period %<>% dplyr::select_(~-Interval)
  data$period %<>% dplyr::select_(~Period, ~everything())
  data$interval %<>% dplyr::select_(~Period, ~Interval, ~DateTime)
  data$interval %<>% dplyr::rename_(.dots = list(DateTimeInterval = ~DateTime))

  data$period %<>% dplyr::as.tbl()
  data$interval %<>% dplyr::as.tbl()
  data
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

make_analysis_spawning <- function(data, spawning) {
  message("making analysis spawning...")

  captures <- nrow(data$capture)
  periods <- nrow(data$period)

  spawn <- matrix(NA, nrow = captures, ncol = periods)
  dimnames(spawn) <- list(Capture = levels(data$capture$Capture), Period = levels(data$period$Period))

  detections <- dplyr::inner_join(data$capture, data$detection, by = "Capture")
  detections %<>% dplyr::inner_join(data$interval, by = c(IntervalDetection = "Interval"))
  detections %<>% dplyr::select_(~Capture, ~Species, ~DateTimeInterval, ~Section, ~Period)
  detections %<>% dplyr::rename_(.dots = list(DateTimeDetection = ~DateTimeInterval))

  period <- dplyr::select_(data$period, ~Period, ~DateTime)

  for (i in 1:nrow(data$capture)) {
    capture_id <- as.character(data$capture$Capture[i])
    detection <- dplyr::filter_(detections, ~Capture == capture_id)
      spawn[capture_id,] <- spawning(detection, period)
  }
  data$spawning <- spawn
  data
}

make_analysis_capture <- function(data) {
  message("making analysis capture...")

  data$capture$Tagged <- data$capture$IntervalCapture < data$capture$IntervalTagExpire

  data$capture %<>% replace_interval_with_period(data, "Capture")
  data$capture %<>% replace_interval_with_period(data, "TagExpire")
  data$capture %<>% dplyr::as.tbl()
  data
}

make_analysis_monitored <- function(data) {
  message("making analysis monitored...")

  captures <- nrow(data$capture)
  periods <- nrow(data$period)

  monitored <- matrix(FALSE, nrow = captures, ncol = periods)
  dimnames(monitored) <- list(Capture = levels(data$capture$Capture), Period = levels(data$period$Period))

  for (i in 1:nrow(data$capture)) {
    if (data$capture$Tagged[i]) {
      monitored[data$capture$Capture[i],as.integer(data$capture$PeriodCapture[i]):as.integer(data$capture$PeriodTagExpire[i])] <- TRUE
    }
  }
  data$capture$Tagged <- NULL
  data$capture$PeriodTagExpire <- NULL
  data$monitored <- monitored
  data
}

make_analysis_length <- function(data, growth, ...) {
  message("making analysis length...")

  if (!is.function(growth)) error("growth must be a function")

  captures <- nrow(data$capture)
  periods <- nrow(data$period)

  length <- matrix(NA, nrow = captures, ncol = periods)
  dimnames(length) <- list(Capture = levels(data$capture$Capture), Period = levels(data$period$Period))

  for (i in 1:captures) {
    for (j in 1:periods) {
      years <- as.numeric(difftime(data$period$DateTime[j], data$period$DateTime[data$capture$PeriodCapture[i]], units = "days")) / 365
      length[data$capture$Capture[i],j] <- as.integer(round(growth(data$capture$Length[i], years, ...)))
    }
  }
  data$length <- length
  data
}

group_recaptures <- function (recapture) {
  if (nrow(recapture) == 1)
    return(recapture)
  is.na(recapture$SectionRecapture) <- TRUE
  recapture$TBarTag1 <- any(recapture$TBarTag1)
  recapture$TBarTag2 <- any(recapture$TBarTag2)
  recapture$TagsRemoved <- any(recapture$TagsRemoved)
  recapture$Released <- all(recapture$Released)
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

  if (nrow(data$recapture)) {
    reported <- data$recapture
    reported$Reported <- TRUE
    reported %<>% reshape2::acast(list(plyr::as.quoted(~Capture),
                                       plyr::as.quoted(~PeriodRecapture)),
                                  fill = FALSE, drop = FALSE, value.var = "Reported")
  } else
    reported <- matrix(FALSE, nrow = nrow(data$capture), ncol = nrow(data$period))

  dimnames(reported) <- list(Capture = levels(data$capture$Capture), Period = levels(data$period$Period))
  data$reported <- reported
  data
}

make_analysis_public <- function(data) {
  message("making analysis public...")

  if (nrow(data$recapture)) {
    public <- data$recapture
    public %<>% reshape2::acast(list(plyr::as.quoted(~Capture),
                                     plyr::as.quoted(~PeriodRecapture)),
                                fill = NA, drop = FALSE, value.var = "Public")
  } else
    public <- matrix(NA, nrow = nrow(data$capture), ncol = nrow(data$period))

  dimnames(public) <- list(Capture = levels(data$capture$Capture), Period = levels(data$period$Period))
  data$public <- public
  data
}

make_analysis_released <- function(data) {
  message("making analysis released...")

  if(nrow(data$recapture)) {
    released <- data$recapture
    released %<>% reshape2::acast(list(plyr::as.quoted(~Capture),
                                       plyr::as.quoted(~PeriodRecapture)),
                                  fill = NA, drop = FALSE, value.var = "Released")
  } else
    released <- matrix(NA, nrow = nrow(data$capture), ncol = nrow(data$period))
  dimnames(released) <- list(Capture = levels(data$capture$Capture), Period = levels(data$period$Period))
  data$released <- released
  data
}

make_analysis_removed <- function(data) {
  message("making analysis removed...")

  if (nrow(data$recapture)) {
    removed <- data$recapture
    removed %<>% reshape2::acast(list(plyr::as.quoted(~Capture),
                                      plyr::as.quoted(~PeriodRecapture)),
                                 fill = NA, drop = FALSE, value.var = "TagsRemoved")
  } else
    removed <- matrix(NA, nrow = nrow(data$capture), ncol = nrow(data$period))

  recaps <- dplyr::filter_(data$recapture, ~as.integer(PeriodRecapture) > 1)
  if (nrow(recaps)) {
    for (i in 1:nrow(recaps)) { # tags can only be removed once
      removed[recaps$Capture[i],1:(as.integer(recaps$PeriodRecapture[i]) - 1)] <- FALSE
    }
  }

  dimnames(removed) <- list(Capture = levels(data$capture$Capture), Period = levels(data$period$Period))
  data$removed <- removed
  data
}

make_analysis_tags <- function(data) {
  message("making analysis tags...")

  captures <- nrow(data$capture)
  periods <- nrow(data$period)

  tags <- array(NA, dim = c(captures, periods, 2))
  dimnames(tags) <- list(Capture = levels(data$capture$Capture),
                         Period = levels(data$period$Period),
                         Tag = c("TBarTag1","TBarTag2"))

  for (i in 1:nrow(data$capture)) {
    tags[data$capture$Capture[i],1:as.integer(data$capture$PeriodCapture[i]),1] <- FALSE
    tags[data$capture$Capture[i],as.integer(data$capture$PeriodCapture[i]),1] <- TRUE
    if (!is.na(data$capture$Reward2[i])) {
      tags[data$capture$Capture[i],1:as.integer(data$capture$PeriodCapture[i]),2] <- FALSE
      tags[data$capture$Capture[i],as.integer(data$capture$PeriodCapture[i]),2] <- TRUE
    } else
      tags[data$capture$Capture[i],,2] <- FALSE
  }

  if (nrow(data$recapture)) {
    for (i in 1:nrow(data$recapture)) {
      period_capture <- data$capture$PeriodCapture[data$capture$Capture == data$recapture$Capture[i]]
      if (data$recapture$TBarTag1[i]) {
        tags[data$recapture$Capture[i],as.integer(period_capture):as.integer(data$recapture$PeriodRecapture[i]),1] <- TRUE
      } else {
        tags[data$recapture$Capture[i],as.integer(data$recapture$PeriodRecapture[i]):periods,1] <- FALSE
      }
      if (data$recapture$TBarTag2[i]) {
        tags[data$recapture$Capture[i],as.integer(period_capture):as.integer(data$recapture$PeriodRecapture[i]),2] <- TRUE
      } else {
        tags[data$recapture$Capture[i],as.integer(data$recapture$PeriodRecapture[i]),2] <- FALSE
      }
      if (data$recapture$TagsRemoved[i] && as.integer(data$recapture$PeriodRecapture[i]) < periods) {
        tags[data$recapture$Capture[i],(as.integer(data$recapture$PeriodRecapture[i]) + 1):periods,] <- FALSE
      }
    }
  }
  data$tags <- tags
  data
}

make_analysis_reward <- function(data) {
  message("making analysis reward...")

  captures <- nrow(data$capture)

  reward <- matrix(nrow = captures, ncol = 2)
  dimnames(reward) <- list(Capture = levels(data$capture$Capture), Tag = c("TBarTag1","TBarTag2"))

  for (i in 1:nrow(data$capture)) {
    reward[data$capture$Capture[i],1] <- data$capture$Reward1[i]
    reward[data$capture$Capture[i],2] <- data$capture$Reward2[i]
  }
  data$reward <- reward
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

  coverage %<>% reshape2::acast(list(plyr::as.quoted(~Section),
                                     plyr::as.quoted(~Period)),
                                value.var = "Coverage")

  dimnames(coverage) <- list(Section = levels(data$section$Section), Period = levels(data$period$Period))
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
  dimnames(detection) <- list(Capture = levels(data$capture$Capture),
                              Period = levels(data$period$Period),
                              Section = levels(data$section$Section))
  data$detection <- detection
  data
}

make_analysis_detected <- function(data) {
  message("making analysis detected...")

  detected <- apply(data$detection, MARGIN = c(1,2), max)
  detected <- detected > 0
  data$detected <- detected
  data
}

make_analysis_moved <- function(data) {
  message("making analysis moved...")

  moved <- apply(data$detection, MARGIN = c(1,2), function(x) sum(x > 0))
  moved <- moved > 1
  data$moved <- moved
  data
}

cleanup_analysis_data <- function (data) {
  data <- data[analysis_data_names()]
  class(data) <- "analysis_data"
  data
}

#' Make Analysis Data
#'
#' Makes analysis_data object from a detect_data object.
#' capture$PeriodCapture indicates the period during which the fish was caught.
#' capture$PeriodTagExpire indicates the period during which the tag expired.
#' detected is a logical matrix indicating for each individual-period whether it
#' was detected during the period.
#' moved is a logical matrix indicating for each individual-period whether it
#' was detected to have moved during the period
#' (based on being detected at multiple sections).
#' reported is a logical matrix indicating for each individual-period
#' whether it was reported to have been recaught during the period.
#' released is a logical matrix indicating for each individual-period whether it
#' was released during the period.
#' tags is a logical array indicating for each individual-period-tbartag whether
#' it was attached at the start of the period (note considered attached at the
#' start of the period during which first caught).
#'
#' @details If a difftime element, interval_period cannot be greater than 28 days
#' i.e. \code{lubridate::make_difftime(60 * 60 * 24 * 28)}.
#'
#' @param data A detect_data object to use.
#' @param capture A data frame of the capture data to use.
#' @param section A data frame of the section data to use.
#' @param interval_period A difftime element that will be used to group the interval or
#' a vector indicating the actual interval groupings.
#' @param growth A function that takes the length of a fish at capture and predicts
#' its length after a number of years.
#' @param additional arguments passed to growth.
#'
#' @return A detect_data object.
#' @export
make_analysis_data <-  function(
  data, capture = data$capture, section = data$section, interval_period = get_difftime(data),
  growth = growth_no, spawning = spawning_no, ...) {

  data %<>% check_detect_data()
  capture %<>% check_detect_capture()
  section %<>% check_detect_section()

  data %<>% filter_detect_captures_section(capture, section)
  data %<>% check_detect_data()

  data %<>% make_analysis_section()
  data %<>% make_analysis_distance()

  data %<>% make_analysis_interval(interval_period)
  data %<>% make_analysis_spawning(spawning)
  data %<>% make_analysis_capture()
  data %<>% make_analysis_monitored()
  data %<>% make_analysis_length(growth, ...)
  data %<>% make_analysis_recapture()
  data %<>% make_analysis_public()
  data %<>% make_analysis_reported()
  data %<>% make_analysis_released()
  data %<>% make_analysis_removed()
  data %<>% make_analysis_reward()
  data %<>% make_analysis_tags()
  data %<>% make_analysis_coverage()
  data %<>% make_analysis_detection()
  data %<>% make_analysis_detected()
  data %<>% make_analysis_moved()
  data %<>% cleanup_analysis_data()

  data %<>% check_analysis_data()
  data
}
