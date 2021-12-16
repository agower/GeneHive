#' @rdname Users
#' @name Users
#' @title
#' Add, retrieve, update, or list User record(s)
#' @description
#' These functions attempt to add or retrieve a \code{User} record,
#' or to list \code{User} records.
#' @param username
#' A character string specifying the name of a \code{User}
#' record to be added, retrieved, or updated.
#' @param \dots
#' Additional arguments specifying fields of the \code{User}
#' record to be added or updated, or fields on which to limit a listing
#' @param active
#' A logical value specifying whether to list only \code{User}s that are
#' active or inactive; a value of \code{NA} shows all \code{User}s
#' regardless of active status.
#' @param simplify
#' A logical value specifying whether to return the listing as a data frame
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' \describe{
#'   \item{\code{addUser}, \code{updateUser}}{
#'     If the operation is successful, a \code{\linkS4class{hiveUser}} object
#'     (invisibly).
#'   }
#'   \item{\code{getUser}}{
#'     If the operation is successful, a \code{\linkS4class{hiveUser}} object.
#'   }
#'   \item{\code{listUsers}}{
#'     \describe{
#'       \item{If \code{simplify} = \code{TRUE}}{
#'         A data frame containing one row per record and one column per field.
#'       }
#'       \item{If \code{simplify} = \code{FALSE}}{
#'         A \code{\linkS4class{SimpleList}} object
#'         containing one \code{\linkS4class{hiveUser}} object per record.
#'       }
#'     }
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addUser <- function (
  username, ..., con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(username)) stop("Argument 'username' is required")
  if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  fields <- list(...)
  # If the email is already in use, exit with an error
  if (!is.null(fields$email)) {
    if (isTRUE(fields$email %in% listUsers(con=con)$email)) {
      stop(paste("Email address", sQuote(fields$email), "is already in use"))
    }
  }
  # If any of the groups do not exist, exit with an error
  if (!is.null(fields$groups)) {
    valid.groups <- sapply(
      is.element(fields$groups, listGroups(con=con)), isTRUE
    )
    if (any(!valid.groups)) {
      stop(
        paste(
          "The following groups do not exist:",
          paste(sQuote(fields$groups[!valid.groups]), collapse=", "))
      )
    }
  }
  hiveAdd(
    con, type="User", fields=list(username=username, ...), verbose=verbose
  )
}

#' @export
#' @rdname Users
getUser <- function (username, con=hiveConnection())
{
  # Check arguments for errors
  if (missing(username)) stop("Argument 'username' is required")
  if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  hiveGet(con, type="User", id=username)
}

#' @export
#' @rdname Users
updateUser <- function (
  username, ..., con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(username)) stop("Argument 'username' is required")
  if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  if (username == "root") {
    dots <- list(...)
    if (!is.null(dots$active)) {
      stop("The 'active' status of User 'root' may not be changed.")
    }
    if (!is.null(dots$superuser)) {
      stop("The 'superuser' status of User 'root' may not be changed.")
    }
  }
  hiveUpdate(con, type="User", list(username=username, ...), verbose=verbose)
}

#' @export
#' @rdname Users
listUsers <- function (con=hiveConnection(), active=TRUE, simplify=TRUE)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(active) && length(active) == 1)) {
    stop("Argument 'active' must be a logical vector of length 1")
  }

  arglist <- list(con=con, type="User", simplify=simplify)
  if (is.na(active)) arglist$fields <- list(active = active)
  do.call(hiveList, args=arglist)
}
