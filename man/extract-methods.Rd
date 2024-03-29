% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{Entity extraction methods}
\alias{Entity extraction methods}
\alias{[,hiveFeatureSpaceEntity-method}
\alias{[,hiveFeatureSetEntity-method}
\alias{[,hiveWeightedFeatureSetEntity-method}
\title{Extract part of a hiveEntity}
\usage{
\S4method{[}{hiveFeatureSpaceEntity}(x, i)

\S4method{[}{hiveFeatureSetEntity}(x, i)

\S4method{[}{hiveWeightedFeatureSetEntity}(x, i)
}
\arguments{
\item{x}{A \code{\linkS4class{hiveEntity}} object from which to extract element(s)}

\item{i}{A numeric, character or logical vector specifying elements to extract}
}
\value{
A \code{hiveEntity} object of the same length as \code{i}.
}
\description{
These methods extract part of a \code{\linkS4class{hiveEntity}}
corresponding to a vector of indices.
}
\author{
Adam C. Gower \email{agower@bu.edu}
}
