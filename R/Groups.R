#' @rdname Groups
#' @name Groups
#' @title
#' Add, delete, or list Group record(s)
#' @description
#' These functions attempt to add or delete a \code{Group} record
#  or to list \code{Group} records.
#' @param name
#' A character string specifying the name of a \code{Group}
#' record to be added or deleted
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' \describe{
#'   \item{\code{addGroup}}{
#'     If the operation is successful, a \code{\linkS4class{hiveGroup}}
#'     object is invisibly returned.
#'   }
#'   \item{\code{deleteGroup}}{
#'     A logical value is returned stating whether the operation was successful.
#'   }
#'   \item{\code{listGroups}}{
#'     A character vector containing all \code{Group} names is returned.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addGroup <- function (
  name, con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(name)) stop("Argument 'name' is required")
  if (!(is.character(name) && length(name) == 1)) {
    stop("Argument 'name' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  hiveAdd(con, type="Group", name=name, verbose=verbose)
}

#' @export
#' @rdname Groups
deleteGroup <- function (
  name, con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(name)) stop("Argument 'name' is required")
  if (!(is.character(name) && length(name) == 1)) {
    stop("Argument 'name' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  hiveDelete(con, type="Group", name=name, verbose=verbose)
}

#' @export
#' @rdname Groups
listGroups <- function (con=hiveConnection())
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  hiveList(con, type="Group")
}
