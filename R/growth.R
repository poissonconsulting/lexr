#' No Growth
#'
#' Function that simply returns the input length, ie allows no growth.
#'
#' @param length A count indicating the length at capture.
#' @param years A number indicating the number of years since capture.
#' @export
#' @examples
#' growth_no(100L, 0)
#' growth_no(100L, 1)
#' growth_no(100L, -1)
growth_no <- function(length, years) {
    return(length)
}

#' Von Bertalanffy Growth
#'
#' Function that calculates now length based on Von Bertalanffy growth.
#'
#' @inheritParams growth_no
#' @param linf Length at infinity for Von Bertalanffy Growth Curve.
#' @param k Growth coefficient for Von Bertalanffy Growth Curve.
#' @export
#' @examples
#' growth_vb(100L, 0)
#' growth_vb(100L, 1)
#' growth_vb(100L, -1)
#' growth_vb(2000L, 1)
growth_vb <- function(length, years, linf = 1000, k = 0.19) {
  check_scalar(linf, c(100, 1000))
  check_scalar(k, c(0, 1))

  increment <- (linf - length) * (1 - exp(-k * years))
  new_length <- length + increment
  if (years < 0) return(max(new_length, 0)) # can't be less than 0
  if(length > linf) return(length) # can't grow if longer than linf
  new_length
}
