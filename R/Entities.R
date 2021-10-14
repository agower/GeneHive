#' @rdname Entities
#' @name Entities
#' @title
#' Add, delete, retrieve, update, or list Entity record(s)
#' @description
#' These functions attempt to add, delete, retrieve or update an \code{Entity}
#' record, or to list \code{Entity} records.
#' @param .class
#' A character string specifying the class of the \code{Entity} record
#' @param .entity_id
#' A character string or \code{UUID} specifying the unique identifier of an
#' \code{Entity} record to be deleted or retrieved
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @param .permissions
#' A \code{\linkS4class{hivePermissions}} object specifying the permissions to
#' be used when creating the record
#' @param \dots
#' Additional arguments specifying fields of the \code{Entity}
#' record to be added or updated, or fields on which to limit a listing
#' @details
#' In functions \code{deleteEntity}, \code{getEntity}, and \code{updateEntity},
#' the \code{.class} argument does not need to be specified, but if present,
#' these functions will check that it matches the class of the \code{Entity}
#' record corresponding to the identifier in argument \code{.entity_id}.
#' @return
#' \describe{
#'   \item{\code{addEntity}, \code{updateEntity}}{
#'     If the operation is successful, a \code{\linkS4class{hiveEntity}} object
#'     (invisibly).
#'   }
#'   \item{\code{getEntity}}{
#'     If the operation is successful, a \code{\linkS4class{hiveEntity}} object.
#'   }
#'   \item{\code{deleteEntity}}{
#'     A logical value stating whether the operation was successful.
#'   }
#'   \item{\code{listEntities}}{
#'     A \code{\linkS4class{hiveEntityList}} object.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addEntity <- function (
  .class, ...,
  .permissions=getOption("GeneHive.permissions"),
  con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(.class)) stop("Argument '.class' is required")
  if (!(is.character(.class) && length(.class) == 1)) {
    stop("Argument '.class' must be a character vector of length 1")
  }
  if (!is(.permissions, "hivePermissions")) {
    stop("Argument '.permissions' must be a hivePermissions object")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # This needs to include a check that any key fields are included
  # (or this check could be placed in hiveAdd(), but only Entities need it)
  hiveAdd(
    con, type="Entity", .class=.class, ...,
    .permissions=.permissions, verbose=verbose
  )
}

#' @export
#' @rdname Entities
deleteEntity <- function (
  .entity_id, con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(.entity_id)) stop("Argument '.entity_id' is required")
  valid.entity_id <- 
    (is.character(.entity_id) && length(.entity_id) == 1) ||
    is(.entity_id, "UUID")
  if (!valid.entity_id) {
    stop(
      "Argument '.entity_id' must be a character vector of length 1 or a UUID"
    )
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  hiveDelete(con, type="Entity", .entity_id=.entity_id, verbose=verbose)
}

#' @export
#' @rdname Entities
getEntity <- function (.entity_id, con=hiveConnection())
{
  if (missing(.entity_id)) stop("Argument '.entity_id' is required")
  valid.entity_id <- 
    (is.character(.entity_id) && length(.entity_id) == 1) ||
    is(.entity_id, "UUID")
  if (!valid.entity_id) {
    stop(
      "Argument '.entity_id' must be a character vector of length 1 or a UUID"
    )
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  hiveGet(con=con, .entity_id=.entity_id)
}

#' @export
#' @rdname Entities
updateEntity <- function (
  .entity_id, ..., con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  if (missing(.entity_id)) stop("Argument '.entity_id' is required")
  valid.entity_id <- 
    (is.character(.entity_id) && length(.entity_id) == 1) ||
    is(.entity_id, "UUID")
  if (!valid.entity_id) {
    stop(
      "Argument '.entity_id' must be a character vector of length 1 or a UUID"
    )
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  hiveUpdate(con, type="Entity", .entity_id=.entity_id, ..., verbose=verbose)
}

#' @export
#' @rdname Entities
listEntities <- function (.class, ..., con=hiveConnection())
{
  # Check arguments for errors
  if (missing(.class)) stop("Argument '.class' is required")
  if (!(is.character(.class) && length(.class) == 1)) {
    stop("Argument '.class' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  result <- hiveList(con, type="Entity", .class=.class, simplify=FALSE, ...)
  hiveEntityList(listData=result@listData)
}
