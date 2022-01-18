#' @importFrom stats setNames

#' @rdname hiveUtilities
#' @name Hive utility functions
#' @title Utility functions used by other functions
#' @description
#' These convenience functions perform low-level tasks used by other GeneHive
#' functions.
#' @param type
#' A character string representing a record type;
#' defaults to \code{"Entity"}
#' @param id
#' A character string specifying a record identifier
#' @param con
#' A \code{\linkS4class{hiveConnection}} object
#' @param class
#' A character string representing an Entity class;
#' in function \code{hiveS4Class}, this is an optional argument
#' that is used to determine S4 class if type = \code{"Entity"}
#' @param Class
#' A character string representing an S4 object class
#' @param attribute
#' A character string representing an attribute (e.g., "id", "name") for which
#' the corresponding slot name is desired
#' @return
#' \describe{
#'   \item{hiveApp}{
#'     A character vector of length 1 specifying the app name to be used in the
#'     API URL
#'   }
#'   \item{hiveExists}{
#'     A logical value specifying whether a record of the given type exists
#'     with the given identifier
#'   }
#'   \item{hiveKeyFields}{
#'     A character vector containing the names of any \code{'key'} fields of an
#'     \code{Entity} class (i.e., those used to compute the UUID of the
#'     record), or, if no key fields exist for the class, a character vector of
#'     length 0
#'   }
#'   \item{hiveS4Class}{
#'     If the input corresponds to a valid S4 object class, it is returned;
#'     otherwise, \code{NULL}.
#'   }
#'   \item{hiveSlotName}{
#'     A character vector of length 1 containing the name of the slot that
#'     corresponds to the specified attribute
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveApp <- function (
  type=c("Entity", "EntityClass", "Group", "User", "WorkFileProperties")
)
{
  app.names <- c(
    "Entity"             = "Entities",
    "EntityClass"        = "EntityClasses",
    "Group"              = "Groups",
    "User"               = "Users",
    "WorkFileProperties" = "WorkFileProperties"
  )

  # Check arguments for errors
  type <- match.arg(type)

  app.names[type]
}

#' @export
#' @rdname hiveUtilities
hiveExists <- function (
  id, type=c("Entity", "EntityClass", "Group", "User", "WorkFileProperties"),
  con=hiveConnection()
)
{
  # Check arguments for errors
  if (missing(id) || length(id) != 1) {
    stop("Argument 'id' must be a vector of length 1")
  }
  id <- as.character(id)
  type <- match.arg(type)
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  arglist <- list(con=con, type=type, id=id)
  get.result <- try(do.call(hiveGet, args=arglist), silent=TRUE)
  !inherits(get.result, "try-error")
}

#' @export
#' @rdname hiveUtilities
hiveKeyFields <- function (class, con=hiveConnection())
{
  # Check arguments for errors
  if (!(is.character(class) && length(class) == 1)) {
    stop("Argument 'class' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  # Retrieve the Entity class definition,
  # terminating with an error if it is not a valid Entity class
  class.definition <- try(getEntityClass(class, con=con), silent=TRUE)
  if (inherits(class.definition, "try-error")) {
    stop(paste(sQuote(class), "is not a valid Entity class"))
  }
  # Extract the 'category' field of each variable definition
  # to a named character vector
  categories <- setNames(
    sapply(class.definition@variables, slot, "category"), 
    sapply(class.definition@variables, slot, "name")
  )
  # Return a vector of 'key' fields (if any)
  names(which(categories == "key"))
}

#' @export
#' @rdname hiveUtilities
hiveLabelFields <- function (class, con=hiveConnection())
{
  # Check arguments for errors
  if (!(is.character(class) && length(class) == 1)) {
    stop("Argument 'class' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  # Retrieve the Entity class definition,
  # terminating with an error if it is not a valid Entity class
  class.definition <- try(getEntityClass(class, con=con), silent=TRUE)
  if (inherits(class.definition, "try-error")) {
    stop(paste(sQuote(class), "is not a valid Entity class"))
  }
  # Extract the 'category' field of each variable definition
  # to a named character vector
  categories <- setNames(
    sapply(class.definition@variables, slot, "category"), 
    sapply(class.definition@variables, slot, "name")
  )
  # Return a vector of 'label' fields (if any)
  names(which(categories == "label"))
}

#' @export
#' @rdname hiveUtilities
hiveS4Class <- function (
  type=c("Entity", "EntityClass", "Group", "User", "WorkFileProperties"),
  class
)
{
  # Check arguments for errors
  type <- match.arg(type)

  if (type != "Entity") {
    if (!missing(class)) {
      warning(
        paste(
          "Argument 'class' is ignored when argument 'type' is:", sQuote(type)
        )
      )
    }
    paste0("hive", type)
  } else {
    if (missing(class) || class == "Entity") {
      "hiveEntity"
    } else {
      paste0("hive", class, "Entity")
    }
  }
}

#' @export
#' @rdname hiveUtilities
hiveSlotName <- function (
  Class,
  attribute=c(
    "creationDate", "creator", "description", "group", "id", "name", "owner",
    "permissions", "updated"
  )
)
{
  # Check arguments for errors
  if (missing(Class)) stop("Argument 'Class' is required")
  if (!(is.character(Class) && length(Class) == 1)) {
    stop("Argument 'Class' must be a character vector of length 1")
  }
  attribute <- match.arg(attribute)

  slots <- switch(
    attribute,
    creationDate=c(
      "hiveEntity"             = ".creation_date",
      "hiveEntityClass"        = ".creation_date",
      "hiveWorkFileProperties" = "creationDatetime"
    ),
    creator=c(
      "hiveEntity"             = ".creator",
      "hiveEntityClass"        = "creator",
      "hiveWorkFileProperties" = "creator"
    ),
    description=c(
      "hiveEntity"             = "description",
      "hiveEntityClass"        = "description"
    ),
    group=c(
      "hiveEntity"             = ".group",
      "hiveEntityClass"        = "group",
      "hiveWorkFileProperties" = "group"
    ),
    id=c(
      "hiveEntity"             = ".entity_id",
      "hiveEntityClass"        = "name",
      "hiveGroup"              = "name",
      "hiveUser"               = "username",
      "hiveVariableDefinition" = "name",
      "hiveWorkFileProperties" = "id"
    ),
    name=c(
      "hiveEntity"             = "name",
      "hiveWorkFileProperties" = "originalName"
    ),
    owner=c(
      "hiveEntity"             = ".owner",
      "hiveEntityClass"        = "owner"
    ),
    permissions=c(
      "hiveEntity"             = ".permissions",
      "hiveEntityClass"        = "permissions",
      "hiveWorkFileProperties" = "permissions"
    ),
    updated=c(
      "hiveEntity"             = ".updated",
      "hiveEntityClass"        = ".updated"
    )
  )

  slots[ifelse(extends(Class, "hiveEntity"), "hiveEntity", Class)]
}
