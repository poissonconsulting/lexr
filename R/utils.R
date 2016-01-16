error <- function(...) stop(..., call. = FALSE)

is_named <- function(x) !is.null(names(x))

equal <- function(x, y) isTRUE(all.equal(x, y, check.names = FALSE))

is_POSIXct <- function(x) inherits(x, "POSIXct")

plural <- function(x, n = 1, end = "") paste0(x, ifelse(n != 1, "s", ""), end)
isare <- function(n) ifelse(n > 1, "are", "is")

punctuate <- function(x, qualifier = "or") {
  if (!(is.logical(x) || is.integer(x) || is.numeric(x)))
    x <- paste0("'", as.character(x), "'")
  if (length(x) == 1)
    return(x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

capture_output <- function(x) {
  x %<>% capture.output() %>% paste(collapse = "\n")
  x
}

lex_data_names <- function() c("section", "station", "deployment", "capture",
       "recapture", "detection", "depth")

detect_data_names <- function() c("section", "distance", "interval", "coverage", "capture",
       "recapture", "detection")

fun_data_name <- function(data, fun) {
  name <- names(data)
  expr <- paste0("data$", name, " <- ", fun, "_", name, "(data$", name, ")")
  eval(parse(text = expr))
  invisible(data)
}
