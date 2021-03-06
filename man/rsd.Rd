% Generated by roxygen2 (4.1.1.9000): do not edit by hand
% Please edit documentation in R/rsd_et_al.r
\name{rsd}
\alias{rsd}
\title{Relative Standard Deviation}
\usage{
rsd(x, as_pct = FALSE)
}
\arguments{
\item{x}{a numeric vector}

\item{as_pct}{logical stating whether the result should be expressed as a
percentage}
}
\description{
This function calculates the relative standard deviation of a numeric vector.
}
\examples{
rsd(c(19, 17, 18))
rsd(17:19, as_pct = TRUE)
}

