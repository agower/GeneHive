% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/httpRequest.R
\name{httpRequest}
\alias{httpRequest}
\title{Submit an HTTP request}
\usage{
httpRequest(
  url,
  method = c("GET", "POST", "PUT", "DELETE"),
  content,
  httpheader = c(Accept = "application/json", `Content-Type` = "application/json"),
  curl
)
}
\arguments{
\item{url}{A character string specifying the URL to which an HTTP request will be
submitted}

\item{method}{A character string specifying the method to be used to submit the HTTP
request}

\item{content}{An optional variable specifying the content to be uploaded during a POST or
PUT request}

\item{httpheader}{A character vector specifying the \code{httpheader} CURLOption to be used;
defaults to \code{Accept='application/json'} and
\code{Content-Type='application/json'}}

\item{curl}{A \code{\linkS4class{CURLHandle}} object}
}
\value{
If an HTTP error was encountered, a \code{\link{simpleError}} object is
returned containing a character vector that consists of an HTTP status
line, followed by the HTTP response body (if it exists).
Otherwise:
\itemize{
  \item{
    If the \code{Content-Type} attribute of the HTTP response body is set to
    \code{'application/json'}, the response body is converted from JSON to an
    R object, and that object is returned.
  }
  \item{
    If that attribute is set to anything else (or does not exist), the HTTP
    response body itself is returned.
  }
}
}
\description{
This function uses \code{\link{curlPerform}} to carry out all low-level HTTP
requests (i.e., API calls), converting all input and output to/from JSON as
specified.
}
\details{
\code{\link{curlPerform}} is used in place of \code{\link{getURL}}
(which does not collect the HTTP header) and \code{\link{getURLContent}}
(which does not return the HTTP response body in the event of an HTTP error)
}
\author{
Adam C. Gower \email{agower@bu.edu}
}
