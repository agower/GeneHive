#' @title Construct a hive API URL
#' @description
#' This function constructs a URL from the base URL of the hive and any
#' arguments (e.g., function names).
#' @param \dots
#' Character strings, integers, and/or numeric values denoting items to be used
#' to construct the path
#' @param query
#' An optional named list of strings that specify the query parameters
#' @param hostname
#' A character string specifying the hostname (IP address) of the hive; defaults
#' to the contents of the environment variable \code{HIVE_HOSTNAME} (if set) or
#' \code{"localhost"} (if not set)
#' @param https
#' A logical value specifying whether a secure HTTP connection should be used;
#' defaults to the contents of the environment variable \code{HIVE_HTTPS}
#' (if set) or \code{TRUE} (if not set)
#' @param api.base.path
#' A character string specifying the base URL path to the hive
#' @return
#' A character string containing the specified API URL.
#' @seealso
#' The function \code{\link{buildQueryString}} is used to create a query string
#' from the \code{query} argument.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveURL <- function (
  ..., query, hostname=getOption("GeneHive.hostname"),
  https=getOption("GeneHive.https"),
  api.base.path=getOption("GeneHive.api.base.path")
)
{
  # Check arguments for errors
  if (!missing(query)) {
    if (!is.list(query)) stop("Argument 'query' must be a list")
  }
  if (!(is.character(hostname) && length(hostname) == 1)) {
    stop("Argument 'hostname' must be a character vector of length 1")
  }
  if (!(is.logical(https) && length(https) == 1)) {
    stop("Argument 'https' must be a logical vector of length 1")
  }
  if (!(is.character(api.base.path) && length(api.base.path) == 1)) {
    stop("Argument 'api.base.path' must be a character vector of length 1")
  }

  # Create a query string from the parameters, using standard HTTP/HTTPS ports
  # Note: ifelse() does not work here when package 'S4Vectors' is attached
  #       ifelse() becomes a generic function
  #       and cannot evaluate buildQueryString(query)
  query.string <- if (!missing(query)) buildQueryString(query) else ""

  paste0(
    ifelse(https, "https", "http"), "://",
    hostname, ":", ifelse(https, 8443, 8080),
    "/", paste(api.base.path, ..., sep="/"),
    query.string
  )
}
