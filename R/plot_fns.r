#' Plot Function
#'
#' This function plots a numeric function that takes a numeric input.
#' @param f a function
#' @param start value at which to begin plotting
#' @param end value at which to stop plotting
#' @param res resolution of the plot
#' @param ... additional arguments to be passed to \code{f}
#' @export
#' @examples
#' plot_fn(sin, -pi, pi)
#' plot_fn(dgamma, 0, 5, shape = 1.1, rate = 2)
plot_fn <- function(f, start, end, res = 250, ...) {
#  require(ggplot2)
  x <- seq(start, end, length = res)
  y <- f(x, ...)
  ggplot(data.frame(x, y), aes(x, y)) + geom_line()
}

#' Joel's ggplot Theme
#'
#' A ggplot plot theme adds the the Candara font to \code{theme_solarize} from
#' the \code{ggthemes} package.
#' @export
#' @examples
#' plot_fn(sin, -pi, pi) + theme_galangjs()
theme_galangjs <- function() {
  if (!grep('Candara', extrafont::fonts())) {
    extrafont::ttr_import(pattern = 'Candara')
  }
  ggthemes::theme_solarized(base_family = "Candara")
}
