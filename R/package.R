#' Lake Exploitation Study Data Plotting and Analysis
#'
#' @docType package
#' @name lexr
#' @import datacheckr
#' @importFrom magrittr %<>% %>%
#' @importFrom sp %over%
#' @examples
#' \dontrun{
#' library(dplyr) # so tbl data frames print nice
#' library(klexdatr) # data set for example
#' library(lexr)
#'
#' qlex <- input_lex_data("qlexdatr")
#' check_lex_data(qlex)
#' print(qlex)
#' plot(qlex, all = TRUE)
#'
#' qdetect <- make_detect_data(qlex)
#' check_detect_data(qdetect)
#' print(qdetect)
#' plot(qdetect)
#'
#' klex <- input_lex_data("klexdatr")
#' check_lex_data(klex)
#' print(klex)
#' plot(klex, all = TRUE)
#'
#' kdetect <- make_detect_data(klex)
#' check_detect_data(kdetect)
#' print(kdetect)
#' plot(kdetect)
#' }
NULL
