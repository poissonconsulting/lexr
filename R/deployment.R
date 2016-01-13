#
#   deployment %<>% dplyr::arrange_(~ReceiverDateTimeIn)
#   deployment %<>% dplyr::mutate_(.dots = list(
#     "Duration" = ~as.integer(difftime(ReceiverDateTimeOut, ReceiverDateTimeIn, units = "secs"))))
#   if (any(deployment$Duration <= 0)) {
#     deployment %<>% dplyr::filter_(~Duration <= 0)
#     error("receiver retrieved before deployed\n", capture_output(deployment))
#   }
#   deployment_diff <- function (x) {
#     x %<>% dplyr::arrange_(~ReceiverDateTimeIn)
#     x$Difference <- c(diff(as.integer(x$ReceiverDateTimeIn)), NA)
#     x
#   }
#   deployment %<>% plyr::ddply("Station", deployment_diff)
#   overlap <- !is.na(deployment$Difference) & deployment$Difference < deployment$Duration
#   if (FALSE) { #(any(overlap)) {
#     overlap <- which(overlap)
#     overlap <- sort(unique(c(overlap, overlap + 1)))
#     deployment %<>% dplyr::slice_(~overlap)
#     error("multiple receivers at the same station\n", capture_output(deployment))
#   }
