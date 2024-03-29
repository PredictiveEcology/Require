% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkgDep.R
\name{paddedFloatToChar}
\alias{paddedFloatToChar}
\title{Convert numeric to character with padding}
\usage{
paddedFloatToChar(x, padL = ceiling(log10(x + 1)), padR = 3, pad = "0")
}
\arguments{
\item{x}{numeric. Number to be converted to character with padding}

\item{padL}{numeric. Desired number of digits on left side of decimal.
If not enough, \code{pad} will be used to pad.}

\item{padR}{numeric. Desired number of digits on right side of decimal.
If not enough, \code{pad} will be used to pad.}

\item{pad}{character to use as padding (\code{nchar(pad) == 1} must be \code{TRUE}).
Currently, can be only \code{"0"} or \code{" "} (i.e., space).}
}
\value{
Character string representing the filename.
}
\description{
This will pad floating point numbers, right or left. For integers, either class
integer or functionally integer (e.g., 1.0), it will not pad right of the decimal.
For more specific control or to get exact padding right and left of decimal,
try the \code{stringi} package. It will also not do any rounding. See examples.
}
\examples{
paddedFloatToChar(1.25)
paddedFloatToChar(1.25, padL = 3, padR = 5)
paddedFloatToChar(1.25, padL = 3, padR = 1) # no rounding, so keeps 2 right of decimal
}
\author{
Eliot McIntire and Alex Chubaty
}
