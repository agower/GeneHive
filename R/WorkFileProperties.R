#' @rdname WorkFileProperties
#' @name WorkFileProperties
#' @title
#' Retrieve, update, or list WorkFileProperties record(s)
#' @description
#' These functions attempt to retrieve a \code{WorkFileProperties} record
#' or to list \code{WorkFileProperties} records.
#' @param id
#' A \code{\linkS4class{hiveWorkFileID}} object specifying the unique identifier
#' of a \code{WorkFileProperties} record.
#' Automatically created when a \code{WorkFile} is uploaded.
#' @param isTrashed
#' A logical value specifying how to limit the result with respect to trashed
#' records.  If \code{FALSE} (default), only untrashed records are returned;
#' if \code{TRUE}, only trashed records are returned; if \code{NA}, both trashed
#' and untrashed records are returned.
#' @param \dots
#' Additional arguments specifying fields of the \code{WorkFileProperties}
#' record to be updated, or fields on which to limit a listing
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' \describe{
#'   \item{\code{getWorkFileProperties}}{
#'     If the operation is successful,
#'     a \code{\linkS4class{hiveWorkFileProperties}} object
#'     is returned.
#'   }
#'   \item{\code{updateWorkFileProperties}}{
#'     If the operation is successful,
#'     a \code{\linkS4class{hiveWorkFileProperties}} object
#'     is invisibly returned.
#'   }
#'   \item{\code{listWorkFileProperties}}{
#'     A data frame containing one row per record and one column per field
#'     is returned.
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
  if (!((is.character(id) && length(id) == 1) || is(id, "WorkFileID"))) {
    stop(
      "Argument 'id' must be a character vector of length 1 ",
      "or a WorkFileID"
    )
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
  if (!((is.character(id) && length(id) == 1) || is(id, "WorkFileID"))) {
    stop(
      "Argument 'id' must be a character vector of length 1 ",
      "or a WorkFileID"
    )
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  hiveUpdate(
    con, type="WorkFileProperties", id=as.character(id), ..., verbose=verbose
  )
}

#' @export
#' @rdname WorkFileProperties
listWorkFileProperties <- function (..., isTrashed=FALSE, con=hiveConnection())
{
  if (!(is.logical(isTrashed) && length(isTrashed) == 1)) {
    stop("Argument 'isTrashed' must be a logical vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  if (!is.na(isTrashed)) {
    # If isTrashed is set to TRUE or FALSE, limit the result accordingly
    hiveList(con, type="WorkFileProperties", isTrashed=isTrashed, ...)
  } else {
    # Otherwise, return all results regardless of trashed status
    hiveList(con, type="WorkFileProperties", ...)
  }
}
#' @export
#' @rdname WorkFileProperties
listWorkFiles <- listWorkFileProperties