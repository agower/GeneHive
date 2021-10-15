#' @import S4Vectors

#' @rdname hiveProcessing
#' @name GeneHive processing
#' @title Pre- or post-process lists during GeneHive operations
#' @description
#' These functions pre-process lists that will be converted to JSON during PUT
#' or POST API calls, or post-process list objects converted from JSON in the
#' HTTP response body of API calls.  These are utility functions that are
#' called by other hive functions, and should not be called directly by the
#' user.
#' @param x
#' A list object (for \code{hivePreprocess}, must be named)
#' @param type
#' A character string specifying the type of the record;
#' defaults to \code{'Entity'}
#' @return
#' \describe{
#'   \item{\code{hivePreprocess}}{
#'     The list \code{x}, modified as needed for upload.
#'   }
#'   \item{\code{hivePostprocess}}{
#'     If the list \code{x} has no names, it represents a list of records, and
#'     a \code{\linkS4class{SimpleList}} object of the same length as \code{x}
#'     will be returned; otherwise, a single S4 object will be returned.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

hivePreprocess <- function (x)
{
  # Check arguments for errors
  if (!(is.list(x) && (!is.null(names(x))))) {
    stop("Argument 'x' must be a named list")
  }

  # All metadata fields (those with leading underscores)
  # correspond to slots with a leading dot
  names(x) <- sub("^\\.", "_", names(x))

  # Remove any names from each element of the list; otherwise, named arrays
  # will be translated to objects during conversion to JSON
  x <- lapply(x, unname)

  # Coerce any S4 objects to character vectors or lists as needed to enable
  # conversion to JSON
  x <- rapply(
    x, f=as, Class="character",
    classes=c("hiveWorkFileID", "UUID"),
    how="replace"
  )
  x <- rapply(
    x, f=as, Class="list",
    classes=c("hivePermissions", "hiveWorkFileIDList", "UUIDList"),
    how="replace"
  )

  # Return the preprocessed list
  x
}

#' @rdname hiveProcessing
hivePostprocess <- function (
  x, type=c("Entity", "EntityClass", "Group", "User", "WorkFileProperties")
)
{
  # Check arguments for errors
  if (!is.list(x)) stop("Argument 'x' must be a named list")
  type <- match.arg(type)

  # If the list has no names, it represents a (potentially empty) list of
  # records, i.e., this function was called from hiveList();
  # otherwise, the list represents a single hive record
  hiveList.call <- is.null(names(x))
  # For convenience, a single hive record is converted to a list of length 1
  # prior to processing
  result <- if (hiveList.call) x else list(x)

  if (type == "Entity" && length(x)) {
    if (hiveList.call) {
      Class <- hiveS4Class(type, .class=unique(sapply(x, "[[", "_class")))
    } else {
      Class <- hiveS4Class(type, .class=x[["_class"]])
    }
  } else {
    Class <- hiveS4Class(type)
  }
  slots <- getSlots(Class)

  for (i in seq_along(result)) {
    record <- result[[i]]
    # Change leading underscores in field names to leading dots
    names(record) <- sub("^_", ".", names(record))
    if (type == "EntityClass") {
      # Create the list of Variable definitions
      for (j in seq_along(record$variables)) {
        if (!is.null(record$variables[[j]]$codes)) {
          record$variables[[j]]$codes <- names(
            unlist(record$variables[[j]]$codes)
          )
        }
        record$variables[[j]] <- do.call(
          hiveVariableDefinition,
          args=c(name=names(record$variables)[j], record$variables[[j]])
        )
      }
      record$variables <- hiveVariableDefinitionCollection(
        unname(record$variables)
      )
      # Create hivePermissions object
      record$permissions <- do.call(hivePermissions, args=record$permissions)
    } else if (type == "Entity") {
      # For each remaining field, populate the slot
      for (slot.name in names(record)) {
        to.class <- slots[slot.name]
        if (!is(record[[slot.name]], to.class)) {
          # UUID and UUIDList slots are handled specially
          if (to.class == "UUID") {
            record[[slot.name]] <- UUIDparse(record[[slot.name]])[[1]]
          } else if (to.class == "UUIDList") {
            record[[slot.name]] <- UUIDparse(as.character(record[[slot.name]]))
          } else {
            # For remaining slots, use whichever comes first:
            # 1. an appropriate coercion method
            # 2. a constructor function with same name as the class of the slot
            # 3. a call to new()
            coerce.method.exists <- hasMethod(
              "coerce", signature(from=class(record[[slot.name]]), to=to.class)
            )
            if (coerce.method.exists) {
              record[[slot.name]] <- as(record[[slot.name]], to.class)
            } else if (exists(to.class, mode="function")) {
              # This is the case for slots of class 'hivePermissions'
              # or empty Entity array variables
              if (is.list(record[[slot.name]])) {
                record[[slot.name]] <- do.call(
                  to.class, args=record[[slot.name]]
                )
              } else {
                record[[slot.name]] <- do.call(
                  to.class, args=list(record[[slot.name]])
                )
              }
            } else {
              record[[slot.name]] <- do.call(
                new, args=list(Class=to.class, record[[slot.name]])
              )
            }
          }
        }
      }
    } else if (type == "Group") {
      # Coerce 'users' field to hiveUserList
      if (length(record$users)) {
        # Note: cannot pass function 'hiveUser' to mapply(), since it cannot
        #       be recycled; must pass string "hiveUser" instead, which can be
        record$users <- hiveUserList(
          mapply(do.call, "hiveUser", args=record$users)
        )
      } else {
        record$users <- hiveUserList()
      }
    } else if (type == "WorkFileProperties") {
      # Coerce fields to S4 objects as necessary
      record$id <- hiveWorkFileID(record$id)
      record$permissions <- do.call(hivePermissions, args=record$permissions)
    }
    # Create the object, using a constructor (if one exists)
    # or new() (if one does not)
    if (exists(Class, mode="function")) {
      result[[i]] <- do.call(Class, args=record)
    } else {
      result[[i]] <- do.call(new, args=c(Class=Class, record))
    }
  }

  # Return the result as a SimpleList of S4 objects or as an S4 object
  if (hiveList.call) {
    new("SimpleList", listData=result, elementType=Class)
  } else {
    result[[1]]
  }
}
