#' Combine Sections Lex Data
#'
#' @param data The lex_data object to combine the sections for.
#' @param sections A named list of the sections to combine.
#' @return The modified lex
#' @export
combine_sections_lex_data <- function(data, sections) {
  stopifnot(is.list(sections), !is.null(names(sections)), !anyDuplicated(names(sections)))
  check_lex_data(data)

  secs <- stats::setNames(as.list(levels(data$section@data$Section)), levels(data$section@data$Section))

  stopifnot(all(unlist(sections) %in% unlist(secs)))

  secs <- secs[!unlist(secs) %in% unlist(sections)]

  secs <- c(sections, secs)
  filter_lex_data(data, sections = secs)
}

