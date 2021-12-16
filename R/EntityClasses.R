#' @import rjson

#' @rdname EntityClasses
#' @name EntityClasses
#' @title
#' Add, delete, retrieve, update, or list EntityClass record(s)
#' @description
#' These functions attempt to add, delete, retrieve or update an
#' \code{EntityClass} record, or to list \code{EntityClass} records.
#' @param filename
#' A character string specifying the name of a file containing an
#' \code{EntityClass} definition in JSON
#' @param name
#' A character string specifying the name of an \code{EntityClass}
#' record to be deleted or retrieved
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' \describe{
#'   \item{\code{addEntityClass}, \code{updateEntityClass}}{
#'     If the operation is successful, a \code{\linkS4class{hiveEntityClass}}
#'     object (invisibly).
#'   }
#'   \item{\code{getEntityClass}}{
#'     If the operation is successful, a \code{\linkS4class{hiveEntityClass}}
#'     object.
#'   }
#'   \item{\code{deleteEntityClass}}{
#'     A logical value stating whether the operation was successful.
#'   }
#'   \item{\code{listEntityClasses}}{
#'     A \code{\linkS4class{hiveEntityClassList}} object.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addEntityClass <- function (
  filename, con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(filename)) stop("Argument 'filename' is required")
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  if (file.exists(filename)) {
    class.definition <- paste(
      sub("^[ ]+", "", scan(filename, what="", sep="\n", quiet=TRUE)),
      collapse=""
    )
    entity.class <- tryCatch(
      fromJSON(class.definition),
      error = function (x) {
        stop("File ", sQuote(filename), " does not contain valid JSON code")
      }
    )
    if (!is.element("name", names(entity.class))) {
      stop(
        "File ", sQuote(filename),
        " does not contain a valid entity class definition"
      )
    }
  } else {
    stop("File ", sQuote(filename), " does not exist")
  }

  # If the EntityClass already exists, exit with an error
  if (is.element(entity.class$name, sapply(listEntityClasses(con), objectId))) {
    stop(
      "EntityClass ", sQuote(entity.class$name),
      " already exists; use updateEntityClass() instead"
    )
  }

  # Submit a POST request and stop if an error is returned
  response <- stopIfHiveError(
    httpRequest(
      url=hiveURL("EntityClasses"), method="POST", content=class.definition,
      curl=con
    )
  )
  # Convert the response to an EntityClass object and return it invisibly
  result <- hivePostprocess(response, "EntityClass")
  if (verbose) {
    cat("EntityClass", sQuote(objectId(result)), "was successfully added.\n")
  }
  invisible(result)
}

#' @export
#' @rdname EntityClasses
deleteEntityClass <- function (
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

  hiveDelete(con, type="EntityClass", id=name, verbose=verbose)
}

#' @export
#' @rdname EntityClasses
getEntityClass <- function (name, con=hiveConnection())
{
  # Check arguments for errors
  if (missing(name)) stop("Argument 'name' is required")
  if (!(is.character(name) && length(name) == 1)) {
    stop("Argument 'name' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  hiveGet(con, type="EntityClass", id=name)
}

#' @export
#' @rdname EntityClasses
updateEntityClass <- function (
  filename, con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(filename)) stop("Argument 'filename' is required")
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  if (file.exists(filename)) {
    class.definition <- paste(
      sub("^[ ]+", "", scan(filename, what="", sep="\n", quiet=TRUE)),
      collapse=""
    )
    entity.class <- tryCatch(
      fromJSON(class.definition),
      error = function (x) {
        stop("File ", sQuote(filename), " does not contain valid JSON code")
      }
    )
    if (!is.element("name", names(entity.class))) {
      stop(
        "File ", sQuote(filename),
        " does not contain a valid entity class definition"
      )
    }
  } else {
    stop("File ", sQuote(filename), " does not exist")
  }

  # If the EntityClass does not exist, exit with an error
  if (!is.element(entity.class$name, sapply(listEntityClasses(con), objectId)))
  {
    stop(
      "EntityClass ", sQuote(entity.class$name),
      " does not exist; use addEntityClass() instead"
    )
  }

  # Submit a PUT request and stop if an error is returned
  response <- stopIfHiveError(
    httpRequest(
      url=hiveURL("EntityClasses", entity.class$name), method="PUT",
      content=class.definition, curl=con
    )
  )
  # Convert the response to an EntityClass object and return it invisibly
  result <- hivePostprocess(response, "EntityClass")
  if (verbose) {
    cat("EntityClass", sQuote(objectId(result)), "was successfully updated.\n")
  }
  invisible(result)
}

#' @export
#' @rdname EntityClasses
listEntityClasses <- function (con=hiveConnection())
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  hiveEntityClassList(
    listData=hiveList(con, type="EntityClass", simplify=FALSE)@listData
  )
}
