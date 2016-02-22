#' Lake Exploitation Study Data Plotting and Analysis
#'
#' @docType package
#' @name lexr
#' @import datacheckr
#' @importFrom magrittr %<>% %>%
#' @importFrom sp %over%
#' @examples
#' \dontrun{
#' library(qlexdatr) # data set for example
#' library(klexdatr) # data set for example
#' library(lexr)
#'
#' qlex <- input_lex_data("qlexdatr")
#' check_lex_data(qlex)
#' plot(qlex, all = TRUE)
#'
#' qdetect <- make_detect_data(qlex, hourly_interval = 24L, end_date = as.Date("2015-09-26"))
#' plot(qdetect)
#' summary(qdetect)
#'
#' klex <- input_lex_data("klexdatr")
#' plot(klex, all = TRUE)
#'
#' kdetect <- make_detect_data(klex, hourly_interval = 24L)
#' plot(kdetect)
#' summary(kdetect)
#' }
NULL
