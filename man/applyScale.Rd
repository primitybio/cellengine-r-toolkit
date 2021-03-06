% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applyScale.R
\name{applyScale}
\alias{applyScale}
\title{Apply a scale}
\usage{
applyScale(scale, data, clamp_q = FALSE)
}
\arguments{
\item{scale}{Scale (named list with keys ['type', 'maximum', 'minimum', 'cofactor']).}

\item{data}{List of values for a channel.}
}
\description{
Applies a scale to a list of channel values
}
\examples{
\dontrun{
applyScale(list(type='LinearScale', minimum=1, maximum=10), list(1, 2, 3, 4, 5))
}
}
