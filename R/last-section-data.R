#' Last Section Data
#'
#' Gives captures and date of final movement by last section moved to excluding fish that subsequently recaptured.
#'
#' @param data The detect_data object to merge.
#' @param delay_days A count of the minimum number of days between final movement and tag expiration.
#' @return A tbl data frame.
#' @export
last_section_data <-  function(data, delay_days = 30L) {
  data %<>% check_detect_data()
  check_count(delay_days)

  lex <- data
  data <- merge(lex$section@data, lex$interval)
  data %<>% dplyr::inner_join(lex$detection, by = c(Interval = "IntervalDetection", Section = "Section"))

  data %<>% dplyr::group_by_(~Capture) %>%
    dplyr::summarise_(.dots = list(LastSection = ~last(Section, order_by = DateTime),
                                   LastDate = ~last(DateTime, order_by = DateTime))) %>%
    dplyr::ungroup()

  data %<>% dplyr::inner_join(dplyr::select_(lex$capture, ~Capture, ~IntervalTagExpire), by = "Capture")
  data %<>% dplyr::inner_join(dplyr::select_(lex$interval, ~Interval, ~Date), by = c(IntervalTagExpire = "Interval"))

  data %<>% dplyr::mutate_(.dots = list(LastDate = ~as.Date(LastDate),
                                 Days = ~Date - LastDate))

  if (delay_days > 0) {
    data %<>% dplyr::filter_(~Days >= delay_days)
  }

  data %<>% dplyr::left_join(dplyr::select_(lex$recapture, ~Capture, ~IntervalRecapture), by = "Capture")

  data %<>% dplyr::select_(~Capture, ~LastSection, ~LastDate, ~IntervalRecapture)
  data %<>% dplyr::left_join(dplyr::select_(lex$interval, ~Interval, ~Date), by = c(IntervalRecapture = "Interval"))

  # get most recent recapture
  recent_recap <- function (x) {
    x %<>% dplyr::arrange_(~Date)
    x %<>% dplyr::slice_(~n())
    x
  }
  data %<>% plyr::ddply("Capture", recent_recap)

  # discard those that most recent recap is after last movement
  data %<>% dplyr::filter_(~is.na(Date) | Date < LastDate)

  data %<>% dplyr::select_(.dots = list(Capture = ~Capture,
                                        Section = ~LastSection,
                                        Date = ~LastDate))

  data %<>% dplyr::arrange_(~Capture)

  dplyr::as.tbl(data)
}
