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
  require(ggplot2)
  x <- seq(start, end, length = res)
  y <- f(x, ...)
  ggplot(data.frame(x, y), aes(x, y)) + geom_line()
}

#' Joel's ggplot Theme
#'
#' A ggplot plot theme based on \code{theme_bw} that uses the Candara font and
#' a solarized light background.
#' @export
#' @examples
#' plot_fn(sin, -pi, pi) + theme_galangjs()
theme_galangjs <- function() {
  require(extrafont)
  if (!grep('Candara', fonts())) ttr_import(pattern = 'Candara')
  # solarized light background
  # (http://www.zovirl.com/2011/07/22/solarized_cheat_sheet/)
  solar_bg <- rgb(253,  246,  227, maxColorValue = 255)
  theme_bw() +
    theme(text = element_text(family = 'Candara'),
          rect = element_rect(fill = solar_bg),
          panel.background = element_rect(fill = solar_bg),
          panel.grid.minor = element_line(color = 'gray90', size = 0.2))
}
