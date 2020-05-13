#' @name buildQueryString
#' @title Create a URL query string from a list of parameter names and values
#' @description
#' This function creates a URL query string from a list of parameter names and
#' values.
#' @param parameters
#' A named list with one item (value) for each parameter;
#' the names of the list items correspond to the names of the parameters
#' @return
#' A character string of the form \code{"param1=value1&param2=value2&..."}
#' @author Adam C. Gower \email{agower@@bu.edu}

buildQueryString <- function (parameters=list())
{
  # Check arguments for errors
  if (!is.list(parameters)) {
    stop("Argument 'parameters' must be a list")
  }

  # If the parameter list is empty, return an empty string;
  # otherwise, check validity of entries in the parameter list
  if (length(parameters) == 0) {
    query.string <- ""
  } else if (!all(sapply(parameters, is.atomic))) {
    stop("All elements of the 'parameters' argument must be atomic values")
  } else if (is.null(names(parameters))) {
    stop("If the 'parameters' argument is not empty, it must be named")
  } else if (any(is.na(names(parameters)))) {
    stop("All elements of the 'parameters' argument must have valid names")
  } else {
    # Convert any NULL values in the parameter list to "null"
    parameters[sapply(parameters, is.null)] <- "null"
    # Convert any logical values to "true"/"false"
    parameters[sapply(parameters, is.logical)] <- tolower(
      as.character(parameters[sapply(parameters, is.logical)])
    )
    # Collapse values into a query string
    query.string <- paste0(
      "?",
      paste(paste(names(parameters), parameters, sep="="), collapse="&")
    )
  }

  # Return the result
  query.string
}
