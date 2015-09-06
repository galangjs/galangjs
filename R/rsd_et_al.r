#' Relative Standard Deviation
#'
#' This function calculates the relative standard deviation of a numeric vector.
#' @param x a numeric vector
#' @param as_pct logical stating whether the result should be expressed as a
#' percentage
#' @export
#' @examples
#' rsd(c(19, 17, 18))
#' rsd(17:19, as_pct = TRUE)
rsd <- function(x, as_pct = FALSE) {
  out <- sd(x) / mean(x)
  ifelse(as_pct, 100*out, out)
}

#' Geometric Mean
#'
#' This function calculates the geometric mean of a numeric vector.
#' @param x a numeric vector
#' @export
#' @examples
#' geomean(c(19, 17, 18))
geomean <- function(x) {
  exp(mean(log(x)))
}

#' Geometric Coefficient of Variation
#'
#' This function calculates the geometric coefficient of variation (GCV) of
#' a numeric vector.
#' @param x a numeric vector
#' @param as_pct logical stating whether the result should be expressed as a
#' percentage
#' @export
#' @examples
#' gcv(c(19, 17, 18))
#' gcv(17:19, as_pct = TRUE)
gcv <- function(x, as_pct = FALSE) {
  out <- exp(sd(log(x))) - 1
  ifelse (as_pct, 100*out, out)
}
