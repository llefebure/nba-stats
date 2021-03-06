% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotData.R
\name{courtOutlinePlot}
\alias{courtOutlinePlot}
\title{Plot the outline of the court}
\usage{
courtOutlinePlot(full = FALSE)
}
\arguments{
\item{full, }{flag indicating whether to plot the full court as opposed to
half court coordinates. FALSE by default.}
}
\value{
A ggplot object with the outline of an NBA court.
}
\description{
This functions creates a ggplot object with the outline of an
NBA court for building custom shot charts.
}
\examples{
courtOutlinePlot()
courtOutlinePlot(full = TRUE)
}

