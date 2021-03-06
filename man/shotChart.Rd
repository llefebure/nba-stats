% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotData.R
\name{shotChart}
\alias{shotChart}
\title{Build a shot chart for a player}
\usage{
shotChart(d = NULL, params = NULL, color = "EVENT_TYPE")
}
\arguments{
\item{d, }{a data frame resulting from a call to getGenericData("shotchartdetail", ...)}

\item{params, }{a list of parameters for a call to "shotchartdetail" (see
getEndpointParams("shotchartdetail") for valid options). Not all need to be
specified as defaults will be used for unspecified parameters.}

\item{color, }{the attribute on which to color the data points in the shot chart
passed as a character vector}
}
\value{
A list containing a ggplot object with the shot chart and a data frame containing
the raw data from which the shot chart was built.
}
\description{
This function builds a shot chart from either the specified data frame
or the specified parameters.
}
\examples{
default <- shotChart()
default$plot
####
andrew.bogut <- shotChart(params = list(PlayerID = 101106))
}

