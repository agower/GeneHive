#' @import RCurl rjson
# Note: '@import utils' causes a namespace conflict with S4Vectors
#' @importFrom utils URLencode

#' @title Submit an HTTP request
#' @description
#' This function uses \code{\link{curlPerform}} to carry out all low-level HTTP
#' requests (i.e., API calls), converting all input and output to/from JSON as
#' specified.
#' @param url
#' A character string specifying the URL to which an HTTP request will be
#' submitted
#' @param method
#' A character string specifying the method to be used to submit the HTTP
#' request
#' @param content
#' An optional variable specifying the content to be uploaded during a POST or
#' PUT request
#' @param httpheader
#' A character vector specifying the \code{httpheader} CURLOption to be used;
#' defaults to \code{Accept='application/json'} and
#' \code{Content-Type='application/json'}
#' @param curl
#' A \code{\linkS4class{CURLHandle}} object
#' @details
#' \code{\link{curlPerform}} is used in place of \code{\link{getURL}}
#' (which does not collect the HTTP header) and \code{\link{getURLContent}}
#' (which does not return the HTTP response body in the event of an HTTP error)
#' @return
#' If an HTTP error was encountered, a \code{\link{simpleError}} object is
#' returned containing a character vector that consists of an HTTP status
#' line, followed by the HTTP response body (if it exists).
#' Otherwise:
#' \itemize{
#'   \item{
#'     If the \code{Content-Type} attribute of the HTTP response body is set to
#'     \code{'application/json'}, the response body is converted from JSON to an
#'     R object, and that object is returned.
#'   }
#'   \item{
#'     If that attribute is set to anything else (or does not exist), the HTTP
#'     response body itself is returned.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

httpRequest <- function (
  url, method=c("GET", "POST", "PUT", "DELETE"), content,
  httpheader=c("Accept"="application/json", "Content-Type"="application/json"),
  curl
)
{
  # Check arguments for errors
  if (missing(url) || missing(curl)) {
    stop("Arguments 'url' and 'curl' are required")
  }
  if (!(is.character(url) && length(url) == 1)) {
    stop("Argument 'url' must be a character vector of length 1")
  }
  method <- match.arg(method)
  if (!is.character(httpheader) || is.null(names(httpheader))) {
    stop("Argument 'httpheader' must be a named character vector")
  }
  if (!is(curl, "CURLHandle")) {
    stop("Argument 'curl' must be a CURLHandle object")
  }

  # Ensure that URL is properly encoded
  url <- URLencode(url)

  # Convert 'content' argument if needed
  if (!missing(content)) {
    if (httpheader["Content-Type"] == "application/json") {
      if (!isJSON(content)) content <- toJSON(content)
    } else if (httpheader["Content-Type"] == "application/octet-stream") {
      if (is.character(content)) {
        content <- charToRaw(content) 
      } else {
        content <- as.raw(content)
      }
    }
    # According to the code for httpPUT(), it appears that the content must be
    # converted to a raw vector before uploading
    if (method == "PUT") {
      if (is.character(content)) {
        content <- charToRaw(content) 
      } else {
        content <- as.raw(content)
      }
    }
  } else {
    if (method %in% c("POST", "PUT")) {
      stop(
        paste(
          "The 'content' argument must not be missing when method =",
          sQuote(method)
        )
      )
    } else {
      # The server will not accept GET or DELETE requests
      # with a Content-Type entry in the HTTP header
      httpheader <- httpheader[!(names(httpheader) == "Content-Type")]
    }
  }

  # Duplicate CURL handle to prevent resetting of CURLOptions across calls
  curl <- dupCurlHandle(curl)

  # Construct the list of CURLOptions to be used with curlPerform()
  curl_reader <- dynCurlReader(curl, baseURL=url)
  .opts <- list(
    url=url, customrequest=method, httpheader=httpheader,
    headerfunction=curl_reader$update
  )
  if (method == "POST") {
    .opts <- c(.opts, list(post=TRUE, postfields=content))
  } else if (method == "PUT") {
    .opts <- c(
      .opts,
      list(infilesize=length(content), readfunction=content, upload=TRUE)
    )
  }
  # Set the CURLOptions and protect them from garbage collection
  curlSetOpt(.opts=.opts, curl=curl, .isProtected=TRUE)

  # Submit an HTTP request to the specified URL, up to twice, if needed
  # Note: PUT requests may require two passes
  #       (stream can't be rewound during multi-pass authentication)
  curlPerform_result <- try(curlPerform(curl=curl), silent=TRUE)
  if (inherits(curlPerform_result, "try-error")) {
    curlPerform_result <- try(curlPerform(curl=curl), silent=TRUE)
  }

  # If request could not be made, return curl error code as SimpleError object;
  # otherwise, parse HTTP response body, header, and status code
  if (inherits(curlPerform_result, "try-error")) {
    simpleError(attr(curlPerform_result, "condition")$message)
  } else {
    # Extract HTTP response body and process according to content type
    response <- curl_reader$value()
    if (!is.null(attributes(response))) {
      # Sometimes the HTTP response has been observed to come back with
      # Content-Type "application/octet-stream" (i.e., raw) when it
      # should really be "text/plain" (i.e., character); this workaround is used
      # to coerce the raw response back to character
      if (attr(response, "Content-Type") == "application/octet-stream") {
        if (httpheader["Accept"] != "application/octet-stream") {
          response <- rawToChar(response)
        }
      } else if (attr(response, "Content-Type") == "application/json") {
        # When a bad password is provided or a Group is deleted,
        # a plaintext message is returned with Content-Type "application/json";
        # tryCatch() prevents fromJSON() from throwing an error
        response <- tryCatch(fromJSON(response), error = function (x) response)
      }
    }

    # Extract HTTP header and status code
    header <- parseHTTPHeader(curl_reader$header())
    http_status_code <- as.integer(header["status"])
    if (http_status_code >= 400) {
      # If there was an error, initialize an error message vector
      # with the HTTP status line
      error_message <- paste0(
        "HTTP ", http_status_code, ": ", header["statusMessage"]
      )
      # Remove any newline at the end of the status line
      error_message <- sub("\r?\n?$", "", error_message)
      # Add the HTTP response body (if it exists) to end of error message vector
      error_message <- c(
        error_message,
        if (is.raw(response)) rawToChar(response) else as.character(response)
      )
      # Return the message as a simpleError object
      simpleError(error_message)
    } else {
      # Otherwise, return the response
      response
    }
  }
}
