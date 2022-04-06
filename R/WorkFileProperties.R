#' @rdname WorkFileProperties
#' @name WorkFileProperties
#' @title
#' Retrieve, update, or list WorkFileProperties record(s)
#' @description
#' These functions attempt to retrieve a \code{WorkFileProperties} record
#' or to list \code{WorkFileProperties} records.
#' @param id
#' A \code{\linkS4class{hiveWorkFileID}} object (or coercible to one)
#' specifying the unique identifier of a \code{WorkFileProperties} record.
#' Automatically populated when a \code{WorkFile} is uploaded.
#' @param \dots
#' Additional arguments specifying fields of the \code{WorkFileProperties}
#' record to be updated, or fields on which to limit a listing
#' @param isTrashed
#' A logical value specifying how to limit the result with respect to trashed
#' records.  If \code{FALSE} (default), only untrashed records are returned;
#' if \code{TRUE}, only trashed records are returned; if \code{NA}, both trashed
#' and untrashed records are returned.
#' @param simplify
#' A logical value specifying whether to return the listing as a data frame
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' \describe{
#'   \item{\code{getWorkFileProperties}}{
#'     If the operation is successful,
#'     a \code{\linkS4class{hiveWorkFileProperties}} object.
#'   }
#'   \item{\code{updateWorkFileProperties}}{
#'     If the operation is successful,
#'     a \code{\linkS4class{hiveWorkFileProperties}} object (invisibly).
#'   }
#'   \item{\code{listWorkFileProperties}}{
#'     \describe{
#'       \item{If \code{simplify} = \code{TRUE}}{
#'         A data frame containing one row per record and one column per field.
#'       }
#'       \item{If \code{simplify} = \code{FALSE}}{
#'         A \code{\linkS4class{SimpleList}} object
#'         containing one \code{\linkS4class{hiveWorkFileProperties}} object
#'         per record.
#'       }
#'     }
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
getWorkFileProperties <- function (id, con=hiveConnection())
{
  # Check arguments for errors
  if (!is(id, "hiveWorkFileID")) {
    id <- try(as(id, "hiveWorkFileID"), silent=TRUE)
    if (inherits(id, "try-error")) {
      stop("Argument 'id' must be a hiveWorkFileID object or coercible to one")
    }
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  workFile <- try(
    hiveGet(con, type="WorkFileProperties", id=as.character(id)), silent=TRUE
  )
  if (!inherits(workFile, "try-error")) {
    return (workFile)
  } else {
    stop(paste("No WorkFile with id", id, "is available"))
  }
}

#' @export
#' @rdname WorkFileProperties
updateWorkFileProperties <- function (
  id, ..., con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  if (!is(id, "hiveWorkFileID")) {
    id <- try(as(id, "hiveWorkFileID"), silent=TRUE)
    if (inherits(id, "try-error")) {
      stop("Argument 'id' must be a hiveWorkFileID object or coercible to one")
    }
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  hiveUpdate(
    con, type="WorkFileProperties", list(id=as.character(id), ...),
    verbose=verbose
  )
}

#' @export
#' @rdname WorkFileProperties
listWorkFileProperties <- function (
  ..., isTrashed=FALSE, con=hiveConnection(), simplify=TRUE
)
{
  if (!(is.logical(isTrashed) && length(isTrashed) == 1)) {
    stop("Argument 'isTrashed' must be a logical vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  arglist <- list(
    con=con, type="WorkFileProperties", simplify=simplify, fields=list(...)
  )
  # If isTrashed is set to TRUE or FALSE, limit the result accordingly;
  # otherwise, return all matching results regardless of trashed status
  if (!is.na(isTrashed)) arglist$fields$isTrashed <- isTrashed
  do.call(hiveList, args=arglist)
}
#' @export
#' @rdname WorkFileProperties
listWorkFiles <- listWorkFileProperties
