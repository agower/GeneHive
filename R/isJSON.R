#' @import rjson

#' @name isJSON
#' @title Test whether a string encodes valid JSON
#' @description
#' This function tests whether a given string contains syntactically correct
#' JSON.
#' @param x
#' A character string
#' @return
#' A logical value specifying whether the stirng contains valid JSON.
#' @author Adam C. Gower \email{agower@@bu.edu}

isJSON <- function (x)
{
  if (is.character(x)) {
    # If fromJSON() throws an error, the input was not JSON
    !inherits(try(rjson::fromJSON(x), silent=TRUE), "try-error")
  } else {
    # If the input is not a character string, it is, by definition, not JSON
    FALSE
  }
}
