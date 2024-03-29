% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hiveProcessing.R
\name{GeneHive processing}
\alias{GeneHive processing}
\alias{hivePreprocess}
\alias{hivePostprocess}
\title{Pre- or post-process lists during GeneHive operations}
\usage{
hivePreprocess(x)

hivePostprocess(
  x,
  type = c("Entity", "EntityClass", "Group", "User", "WorkFileProperties")
)
}
\arguments{
\item{x}{A list object (for \code{hivePreprocess}, must be named)}

\item{type}{A character string specifying the type of the record;
defaults to \code{'Entity'}}
}
\value{
\describe{
  \item{\code{hivePreprocess}}{
    The list \code{x}, modified as needed for upload.
  }
  \item{\code{hivePostprocess}}{
    If the list \code{x} has no names, it represents a list of records, and
    a \code{\linkS4class{SimpleList}} object of the same length as \code{x}
    will be returned; otherwise, a single S4 object will be returned.
  }
}
}
\description{
These functions pre-process lists that will be converted to JSON during PUT
or POST API calls, or post-process list objects converted from JSON in the
HTTP response body of API calls.  These are utility functions that are
called by other functions, and should not be called directly by the
user.
}
\author{
Adam C. Gower \email{agower@bu.edu}
}
