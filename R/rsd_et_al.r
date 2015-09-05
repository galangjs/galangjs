#' Relative Standard Deviation
#' 
#' This function calculates the relative standard deviation of a numeric vector.
#' @param x a numeric vector
#' @export
#' @examples
#' rsd(c(19, 17, 18))
rsd <- function(x) {
  sd(x) / mean(x)
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
#' @export
#' @examples
#' gcv(c(19, 17, 18))
gcv <- function(x) {
  exp(sd(log(x))) - 1
}
