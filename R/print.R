#' @export
print.SpatialPolygonsDataFrame <- function(x, ...) {
  x <- dplyr::as.tbl(x@data)
  print(x)
}
