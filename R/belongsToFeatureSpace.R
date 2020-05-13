#' @export
#' @name belongsToFeatureSpace
#' @title Tests whether each element of a vector belongs to a FeatureSpace
#' @description
#' This convenience function tests whether each element of a character vector
#' belongs to a given \code{FeatureSpace}.
#' @param x
#' A character vector
#' @param featureSpace
#' A \code{\linkS4class{hiveFeatureSpaceEntity}} object
#' @return
#' A logical vector specifying whether each element of \code{x} belongs to the
#' given \code{FeatureSpace}
#' @author Adam C. Gower \email{agower@@bu.edu}

belongsToFeatureSpace <- function (x, featureSpace)
{
  # Check arguments for errors
  if (missing(x) || missing(featureSpace)) {
    stop("Arguments 'x' and 'featureSpace' are required")
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be a character vector")
  }
  if (!is(featureSpace, "hiveFeatureSpaceEntity")) {
    stop("Argument 'featureSpace' must be a hiveFeatureSpaceEntity object")
  }

  if (length(featureSpace) == 1) {
      result <- grepl(featureSpace@values, x, perl=TRUE)
  } else {
      result <- is.element(x, featureSpace@values)
  }
  result
}
