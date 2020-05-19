#' @import rjson

#' @title Terminate if an HTTP response is hive error message
#' @description
#' This is a convenience function to terminate if the result of an HTTP request
#' contains a hive error message.
#' @param x
#' A variable returned by \code{\link{httpRequest}}
#' @details
#' The hive may return a JSON-encoded list containing a single element,
#' named \code{error}, containing an error message.  If so, it is extracted to
#' an error message during termination (see below).
#' @return
#' If the argument \code{x} is of class \code{\link{simpleError}}, the function
#' processes the error message as needed and terminates with the error;
#' otherwise, the argument \code{x} is returned.
#' @author Adam C. Gower \email{agower@@bu.edu}

stopIfHiveError <- function (x)
{
  if (is(x, "simpleError")) {
    if (length(x$message) > 1) {
      # Split the message into the HTTP status line and HTTP response body
      http.status <- x$message[1]
      http.response <- x$message[2]
      # Process the response body if it is JSON code
      http.response <- tryCatch(
        fromJSON(http.response), error = function (x) http.response
      )
      # The hive may return a list of one element, named 'error',
      # if there is an HTTP error
      if (is.list(http.response)) {
        if (identical(names(http.response), "error")) {
          http.response <- http.response$error
        } else {
          http.response <- unlist(http.response)
        }
      }
      # In case a vector of length > 1 is obtained, collapse it with newlines
      http.response <- paste(http.response, collapse="\n")
      # Put the error message back together into a single character string
      x$message <- paste(http.status, http.response, sep="\n")
    }
    stop(x)
  } else {
    x
  }
}
