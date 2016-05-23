#' Last Section Data
#'
#' Calculates number of fish last detected at each section.
#'
#' @param data The detect_data object to merge.
#' @return A tbl data frame.
#' @export
last_section_data <-  function(data) {
  data %<>% check_detect_data()

  lex <- data
  data <- merge(lex$section@data, lex$interval)
  data %<>% dplyr::inner_join(lex$detection, by = c(Interval = "IntervalDetection", Section = "Section"))
  data %<>% dplyr::group_by_(~Capture) %>%
    dplyr::summarise_(.dots = list(Section = ~last(Section, order_by = DateTime))) %>%
    dplyr::ungroup()
  data %<>% dplyr::group_by_(~Section) %>% dplyr::summarise_(.dots = list(Fish = ~n())) %>%
    dplyr::ungroup()

  data
}
