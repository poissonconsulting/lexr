error <- function(...) stop(..., call. = FALSE)

is_flag <- function(x)  is.logical(x) && length(x) == 1 && !is.na(x)
is_string <- function(x)  (is.character(x) || is.factor(x)) && length(x) == 1 && !is.na(x)
is_count <- function(x)  (is.integer(x) || is.numeric(x)) && length(x) == 1 &&
  !is.na(x) && x >= 0 && identical(as.numeric(x), floor(x))
is_named <- function(x) !is.null(names(x))

equal <- function(x, y) isTRUE(all.equal(x, y, check.names = FALSE))

is_POSIXct <- function(x) inherits(x, "POSIXct")

check_string <- function(x)
  if (!is_string(x)) error(substitute(x), " must be a string")

check_flag <- function(x)
  if (!is_flag(x)) error(substitute(x), " must be a flag")

check_count <- function(x)
  if (!is_count(x)) error(substitute(x), " must be a count")

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

data_names <- function() c("section", "station", "receiver", "deployment", "capture",
       "recapture", "detection", "depth")

fun_data_name <- function(data, fun) {
  name <- names(data)
  expr <- paste0("data$", name, " <- ", fun, "_", name, "(data$", name, ")")
  eval(parse(text = expr))
  invisible(data)
}
