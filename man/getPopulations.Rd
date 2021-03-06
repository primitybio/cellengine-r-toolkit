% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPopulations.R
\name{getPopulations}
\alias{getPopulations}
\title{Get populations}
\usage{
getPopulations(experimentId, params = list())
}
\arguments{
\item{experimentId}{ID of experiment.}

\item{params}{Optional query parameters.}
}
\description{
Retrieves the list of populations in an experiment.
}
\examples{
\dontrun{
# List all populations in the experiment
getPopulations(experimentId)

# List the names of the first five populations
getPopulations(experimentId, params = list("limit" = "5", "fields" = "+name"))
}
}
