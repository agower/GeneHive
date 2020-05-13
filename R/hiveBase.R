#' @import S4Vectors uuidtools
#' @importFrom stats setNames

#' @rdname hiveBase
#' @name Hive base functions
#' @title Add, delete, retrieve, update, or list hive record(s)
#' @description
#' These functions attempt to add, retrieve, or update a GeneHive record, to
#' delete a Group, EntityClass or Entity record, or to list GeneHive records of
#' a given class.  In general, they should not be called directly by the user;
#' the appropriate wrapper functions should be used instead.
#' @param con
#' A \code{\linkS4class{hiveConnection}} object
#' @param type
#' A character string specifying the type of the record;
#' defaults to \code{'Entity'}
#' @param \dots
#' Additional arguments specifying the unique identifier field of the record to
#' be added, deleted, retrieved or updated (see \code{\link{hiveSlotName}}),
#' and/or any other fields to add or update or on which to limit a listing.
#' Note: when adding or listing Entity records, \dots must always include a
#' \code{.class} argument.
#' @param append
#' A logical value specifying whether to append new elements to array slots
#' (rather than replace them); defaults to \code{TRUE}
#' @param simplify
#' A logical value specifying whether to make listing output more
#' human-readable (i.e., convert to data frame, show only the lengths of
#' arrays, convert UUIDs to plain-text descriptions)
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @details
#' \describe{
#'   \item{\code{hiveAdd}}{
#'     \itemize{
#'       \item{
#'         If a record already exists for the given ID, a warning is produced.
#'         If the arguments provided differ from those associated with the
#'         existing record, the function terminates with an error; otherwise,
#'         the existing record is returned invisibly as an S4 object.
#'       }
#'     }
#'   }
#'   \item{\code{hiveUpdate}}{
#'     \itemize{
#'       \item{
#'         If a record is unchanged after the update, a warning is produced,
#'         and the existing record is returned invisibly as an S4 object.
#'       }
#'     }
#'   }
#' }
#' @return
#' \describe{
#'   \item{\code{hiveAdd}, \code{hiveUpdate}}{
#'     If the operation is successful, an S4 object is invisibly returned.
#'   }
#'   \item{\code{hiveGet}}{
#'     If the operation is successful, an S4 object is returned.
#'   }
#'   \item{\code{hiveDelete}}{
#'     A logical value is returned stating whether the operation was successful.
#'   }
#'   \item{\code{hiveList}}{
#'     \describe{
#'       \item{If \code{simplify} = \code{TRUE}}{
#'         A data frame containing one row per record and one column per field.
#'       }
#'       \item{If \code{simplify} = \code{FALSE}}{
#'         A \code{\linkS4class{SimpleList}} object
#'         containing one S4 object per record.
#'       }
#'     }
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveAdd <- function (
  con=hiveConnection(), type=c("Entity", "Group", "User"),
  ..., verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Determine the S4 object class of the output,
  # and if adding an Entity, stop if an Entity class was not specified
  Class <- hiveS4Class(type, ...)
  if (Class == "hiveEntity") {
    stop(
      "When adding Entity records, '...' must contain a valid '.class' argument"
    )
  }
  # Extract the '...' argument to a named list
  fields <- list(...)
  # If adding an Entity, refresh the local S4 class definition
  if (type == "Entity") refreshEntityS4Class(fields$.class, verbose=FALSE)
  slots <- getSlots(Class)

  # Check to make sure that the '...' argument is named properly
  # (This check produces a more informative error message than that returned by
  # initialize())
  # Note: allNames is used in the event that '...' contains >= 1 elements,
  #       all of which are unnamed
  if (any(!is.element(allNames(fields), names(slots)))) {
    stop(
      paste(
        "All arguments in '...' must be named,",
        "and these names must correspond to valid", Class, "object slots"
      )
    )
  }
  # Coerce all fields corresponding to S4 object slots to the appropriate class
  for (i in seq_along(fields)) {
    target.class <- slots[names(fields)[i]]
    if (target.class == "UUID") {
      if (is.character(fields[[i]])) {
        fields[[i]] <- UUIDparse(fields[[i]])[[1]]
      } else if (!is(fields[[i]], "UUID")) {
        stop("Argument ", names(fields)[i], " cannot be coerced to a UUID")
      }
    } else if (target.class == "UUIDList") {
      if (is.list(fields[[i]])) {
        fields[[i]] <- UUIDList(fields[[i]])
      } else if (!is(fields[[i]], "UUIDList")) {
        stop("Argument ", names(fields)[i], " cannot be coerced to a UUIDList")
      }
    } else {
      fields[[i]] <- as(fields[[i]], target.class)
    }
  }
  # Create an S4 object from the fields, performing a recursive check to ensure
  # that all of the data types, etc., are correct
  # Note: for hiveEntity subclasses, this step will also automatically compute
  # the ID of the object via initialize()
  validObject(object <- do.call("new", c(Class=Class, fields)), complete=TRUE)

  # If the object ID was automatically computed and not explicitly provided,
  # move it into the 'fields' list; otherwise, the GeneHive server will
  # automatically assign a random (version 4) UUID to the record
  if (type == "Entity" && isNil(objectId(object))) {
    object.exists <- FALSE
  } else {
    id.slot <- hiveSlotName(Class, "id")
    fields[[id.slot]] <- objectId(object)
    # Check whether the object is already present in the hive
    if (type == "Group") {
      object.exists <- fields[[id.slot]] %in% listGroups(con=con)
    } else {
      object.exists <- hiveExists(fields[[id.slot]], type, con)
    }
    if (object.exists) {
      # If the object is already present in the hive, produce a warning
      if (type == "Entity") {
        warning(
          paste(fields$.class, "record", fields[[id.slot]], "already exists")
        )
      } else {
        warning(
          paste(type, "record", sQuote(fields[[id.slot]]), "already exists")
        )
      }
      if (type != "Group") {
        # If the type is "Entity", remove any key fields
        # (these may not be updated)
        if (type == "Entity") {
          updates <- fields[
            setdiff(names(fields), hiveKeyFields(fields$.class))
          ]
        }
        # Identify the fields that do not match the existing record
        updates <- fields[
          unlist(
            lapply(
              names(fields),
              function (name) !identical(slot(object, name), fields[[name]])
            )
          )
        ]
        # If updates were specified, terminate with an error message
        if (length(updates)) {
          stop(
            paste(
              "Use hiveUpdate() to update fields:",
              paste(sQuote(names(updates)), collapse=", ")
            )
          )
        }
      }
      result <- object
    }
  }

  if (!object.exists) {
    # Ensure that Entity array variables of length 1 are converted to JSON
    # arrays by coercing to list first
    if (type == "Entity") {
      class.definition <- getEntityClass(fields$.class)
      array.variable.ids <- unlist(
        lapply(class.definition@variables, function (x) objectId(x)[x@is_array])
      )
      for (variable.id in array.variable.ids) {
        fields[[variable.id]] <- as.list(unname(fields[[variable.id]]))
      }
    }
    # Submit a POST request to the hive and stop if an error is returned
    response <- stopIfHiveError(
      httpRequest(
        url=hiveURL(hiveApp(type)), method="POST",
        content=hivePreprocess(fields), curl=con
      )
    )
    # Convert the response to an S4 object and return it invisibly
    result <- hivePostprocess(response, type)
    if (verbose) {
      if (type == "Entity") {
        cat(
          fields$.class, "record", as.character(objectId(result)),
          "was successfully created.\n"
        )
      } else {
        cat(
          type, "record", sQuote(objectId(result)),
          "was successfully created.\n"
        )
      }
    }
  }
  invisible(result)
}

#' @rdname hiveBase
hiveDelete <- function (
  con=hiveConnection(), type=c("Entity", "EntityClass", "Group"),
  ..., verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  Class <- hiveS4Class(type, ...)

  # Extract the '...' argument to a named list
  fields <- list(...)

  # Ensure that an ID was provided for the object
  id.slot <- hiveSlotName(Class, "id")
  if (length(fields[[id.slot]]) == 0) {
    stop(
      paste(
        "The", sQuote(id.slot), "argument is required to delete a",
        type, "record"
      )
    )
  } else {
    # Check to see if the record exists in the hive; if not, exit with an error
    object <- try(
      do.call("hiveGet", c(type=type, fields[id.slot], con=con)), silent=TRUE
    )
    if (inherits(object, "try-error")) {
      if (type == "Entity") {
        stop(paste(type, "record", fields[[id.slot]], "does not exist"))
      } else {
        stop(paste(type, "record", sQuote(fields[[id.slot]]), "does not exist"))
      }
    }
    # Submit a DELETE request and stop if an error is returned
    response <- stopIfHiveError(
      httpRequest(
        url=hiveURL(hiveApp(type), fields[[id.slot]]), method="DELETE", curl=con
      )
    )
    # Convert the list to a logical vector
    # (there is only one element, 'success', in the result)
    response <- unlist(response)
    # Return the response
    if (verbose) {
      if (type == "Entity") {
        cat(
          object@.class, "record", as.character(objectId(object)),
          "was successfully deleted.\n"
        )
      } else {
        cat(
          type, "record", sQuote(objectId(object)),
          "was successfully deleted.\n"
        )
      }
    }
    response
  }
}

#' @rdname hiveBase
hiveGet <- function (
  con=hiveConnection(),
  type=c("Entity", "EntityClass", "User", "WorkFileProperties"),
  ...
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)

  Class <- hiveS4Class(type, ...)

  # Extract the '...' argument to a named list
  fields <- list(...)

  # Ensure that an ID was provided for the object
  id.slot <- hiveSlotName(Class, "id")
  fields[[id.slot]] <- as.character(fields[[id.slot]])
  if (length(fields[[id.slot]]) == 0) {
    stop(
      paste(
        "The", sQuote(id.slot), "argument is required to retrieve a",
        type, "object"
      )
    )
  }

  # Submit a GET request and stop if an error is returned
  response <- stopIfHiveError(
    httpRequest(
      url=hiveURL(hiveApp(type), fields[[id.slot]]), method="GET", curl=con
    )
  )

  # If retrieving an Entity, refresh the local S4 class definition
  if (type == "Entity") {
    refreshEntityS4Class(response[["_class"]], verbose=FALSE)
  }
  # Return the response as an S4 object of the appropriate class
  hivePostprocess(response, type)
}

#' @rdname hiveBase
hiveUpdate <- function (
  con, type=c("Entity", "User", "WorkFileProperties"),
  ..., append=TRUE, verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (!(is.logical(append) && length(append) == 1)) {
    stop("Argument 'append' must be a logical vector of length 1")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Extract the '...' argument to a named list
  fields <- list(...)
  # Define the slot that holds the ID of the object
  Class <- hiveS4Class(type)
  id.slot <- hiveSlotName(Class, "id")
  # Ensure that an ID was provided for the object
  if (length(fields[[id.slot]]) == 0) {
    stop(
      paste(
        "The", sQuote(id.slot), "argument is required to update a",
        type, "record"
      )
    )
  }
  # Check to see if the record exists in the hive; if not, exit with an error
  object <- try(
    do.call("hiveGet", c(type=type, fields[id.slot], con=con)), silent=TRUE
  )
  if (inherits(object, "try-error")) {
    stop(
      paste(
        type, "record",
        ifelse(type == "Entity", fields[[id.slot]], sQuote(fields[[id.slot]])),
        "does not exist; add a new record instead"
      )
    )
  }

  if (type == "Entity") {
    Class <- hiveS4Class(type, .class=object@.class)
    # If updating an Entity, refresh the local S4 class definition
    refreshEntityS4Class(object@.class, verbose=FALSE)
  }
  slots <- getSlots(Class)

  # Check to make sure that the '...' argument is named properly
  # (This check produces a more informative error message than that returned by
  # initialize())
  # Note: allNames is used in the event that '...' contains >= 1 elements,
  #       all of which are unnamed
  if (any(!is.element(allNames(fields), names(slots)))) {
    stop(
      paste(
        "All arguments in '...' must be named,",
        "and these names must correspond to valid", Class, "object slots"
      )
    )
  }
  # Coerce all fields corresponding to S4 object slots to the appropriate class
  for (i in seq_along(fields)) {
    target.class <- slots[names(fields)[i]]
    if (target.class == "UUID") {
      if (is.character(fields[[i]])) {
        fields[[i]] <- UUIDparse(fields[[i]])[[1]]
      } else if (!is(fields[[i]], "UUID")) {
        stop("Argument ", names(fields)[i], " cannot be coerced to a UUID")
      }
    } else if (target.class == "UUIDList") {
      if (is.list(fields[[i]])) {
        fields[[i]] <- UUIDList(fields[[i]])
      } else if (!is(fields[[i]], "UUIDList")) {
        stop("Argument ", names(fields)[i], " cannot be coerced to a UUIDList")
      }
    } else {
      fields[[i]] <- as(fields[[i]], target.class)
    }
  }
  # Create an S4 object from the fields, performing a recursive check to ensure
  # that all of the data types, etc., are correct
  # Note: suppressWarnings() is used because a call to "new" without all key
  #       fields will produce a warning
  suppressWarnings(
    validObject(
      updates <- do.call("new", c(Class=Class, fields)), complete=TRUE
    )
  )
  # Convert the S4 object to a list of only those positions that were updated
  updates <- as(updates, "list")[setdiff(names(fields), id.slot)]

  # If type is 'Entity', issue a warning if an attempt was made to update any
  # key fields
  if (type == "Entity") {
    key.fields <- hiveKeyFields(object@.class)
    if (any(is.element(names(updates), key.fields))) {
      warning(
        paste(
          "The following", object@.class,
          "fields may not be updated and will be ignored:",
          paste(sQuote(key.fields), collapse=", ")
        )
      )
      updates[key.fields] <- NULL
    }
  }

  # Limit the list of updates to those that do not match the existing record
  updates <- updates[
    sapply(
      names(updates),
      function (name) !identical(slot(object, name), updates[[name]])
    )
  ]

  if (length(updates)) {
    if (type == "Entity") {
      class.definition <- getEntityClass(object@.class)
      array.variable.ids <- unlist(
        lapply(class.definition@variables, function (x) objectId(x)[x@is_array])
      )
    } else if (type == "User") {
      array.variable.ids <- "groups"
    } else {
      array.variable.ids <- NULL
    }

    # If append is TRUE, discordant array slots should be unified rather than
    # replaced
    # Note: coercion to character is performed first to ensure that setdiff()
    #       works properly
    if (append) {
      for (variable.id in intersect(names(updates), array.variable.ids)) {
        existing.elements <- as(slot(object, variable.id), "character")
        new.elements <- setdiff(
          as(updates[[variable.id]], "character"), existing.elements
        )
        if (verbose) {
          cat("Adding the following", sQuote(variable.id), "element(s) to ")
          if (type == "Entity") {
            cat(
              sprintf(
                "%s record %s: ", object@.class, as.character(fields[[id.slot]])
              )
            )
          } else {
            cat(sprintf("%s record %s: ", type, sQuote(fields[[id.slot]])))
          }
          cat(paste(sQuote(new.elements), collapse=", "))
          cat("\n")
        }
        if (slots[variable.id] == "UUIDList") {
          updates[[variable.id]] <- UUIDparse(
            c(existing.elements, new.elements)
          )
        } else {
          updates[[variable.id]] <- as(
            c(existing.elements, new.elements), slots[variable.id]
          )
        }
      }
    }

    if (type == "User") {
      # If a new password was provided, check to make sure that it is new
      # (user@password will always be NA because the hive does not return
      # passwords as part of User records)
      if (!is.null(updates$password)) {
        if (checkPassword(fields[[id.slot]], updates$password)) {
          updates$password <- NULL
        }
      }
      # If group names were provided, check to make sure that they exist
      if (!is.null(updates$groups)) {
        valid.groups <- sapply(
          is.element(fields$groups, listGroups(con=con)), isTRUE
        )
        if (any(!valid.groups)) {
          stop(
            paste(
              "The following groups do not exist:",
              paste(sQuote(updates$groups[!valid.groups]), collapse=", ")
            )
          )
        }
      }
      # If an email address was provided,
      # check to make sure that it is not already taken
      if (!is.null(updates$email)) {
        if (updates$email %in% listUsers(con=con)$email) {
          stop(
            paste(
              "Cannot change email address to", sQuote(updates$email),
              "as it is already in use"
            )
          )
        }
      }
    }

    # Create list of updates
    update.list <- c(fields[id.slot], updates)
    # For User records, ensure the 'group' and 'groups' fields are present;
    # the 'group' field is required by hive for updates, and the 'groups' field
    # will be overwritten by the hive if omitted
    # Note: the [[ operator must be used for the first command instead of $,
    #       which uses partial matching, and so update.list$group will match on
    #       update.list$groups if it exists
    if (type == "User") {
      if (is.null(update.list[["group"]])) {
        update.list[["group"]] <- object@group
      }
      if (is.null(update.list[["groups"]])) {
        update.list[["groups"]] <- object@groups
      }
    }
    if (type == "Entity") {
      # For Entity records, ensure '.permissions' field is present
      # (required by hive for updates)
      if (is.null(update.list$.permissions)) {
        update.list$.permissions <- objectPermissions(object)
      }
      # Ensure that array variables of length 1 are converted to JSON arrays
      # by coercing to list first
      for (variable.id in intersect(names(update.list), array.variable.ids)) {
        update.list[[variable.id]] <- as.list(
          unname(update.list[[variable.id]])
        )
      }
    }

    # Submit a PUT request and stop if an error is returned
    response <- stopIfHiveError(
      httpRequest(
        hiveURL(hiveApp(type), fields[[id.slot]]), method="PUT",
        content=hivePreprocess(update.list), curl=con
      )
    )
    # Convert the response to an S4 object
    result <- hivePostprocess(response, type)
    if (verbose) {
      cat("The following field(s) of ")
      if (type == "Entity") {
        cat(
          paste(object@.class, "record", as.character(fields[[id.slot]]))
        )
      } else {
        cat(paste(type, "record", sQuote(fields[[id.slot]])))
      }
      cat(" were updated: ")
      cat(paste(sQuote(names(updates)), collapse=", "))
      cat("\n")
    }
    invisible(result)
  } else {
    warning(paste(type, "record", sQuote(fields[[id.slot]]), "was unchanged"))
    invisible(object)
  }
}

#' @rdname hiveBase
hiveList <- function (
  con, type=c("Entity", "EntityClass", "Group", "User", "WorkFileProperties"),
  ..., simplify=TRUE
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (!(is.logical(simplify) && length(simplify) == 1)) {
    stop("Argument 'simplify' must be a logical vector of length 1")
  }

  # Determine the S4 object class of the output,
  # and if adding an Entity, stop if an Entity class was not specified
  Class <- hiveS4Class(type, ...)
  if (Class == "hiveEntity") {
    stop(
      "When listing Entity records, ",
      "'...' must contain a valid '.class' argument"
    )
  }
  # Extract the '...' argument to a named list
  fields <- list(...)
  # If listing Entities, refresh the local S4 class definition
  if (type == "Entity") refreshEntityS4Class(fields$.class, verbose=FALSE)
  slots <- getSlots(Class)

  # Check to make sure that the '...' argument is named properly
  # (This check produces a more informative error message than that returned by
  # initialize())
  # Note: allNames is used in the event that '...' contains >= 1 elements,
  #       all of which are unnamed
  if (any(!is.element(allNames(fields), names(slots)))) {
    stop(
      paste(
        "All arguments in '...' must be named,",
        "and these names must correspond to valid", Class, "object slots"
      )
    )
  }
  # Coerce all fields corresponding to S4 object slots to the appropriate class
  for (i in seq_along(fields)) {
    target.class <- slots[names(fields)[i]]
    if (target.class == "UUID") {
      if (is.character(fields[[i]])) {
        fields[[i]] <- UUIDparse(fields[[i]])[[1]]
      } else if (!is(fields[[i]], "UUID")) {
        stop("Argument ", names(fields)[i], " cannot be coerced to a UUID")
      }
    } else if (target.class == "UUIDList") {
      if (is.list(fields[[i]])) {
        fields[[i]] <- UUIDList(fields[[i]])
      } else if (!is(fields[[i]], "UUIDList")) {
        stop("Argument ", names(fields)[i], " cannot be coerced to a UUIDList")
      }
    } else {
      fields[[i]] <- as(fields[[i]], target.class)
    }
  }
  # Create an S4 object from the fields, performing a recursive check to ensure
  # that all of the data types, etc., are correct
  # Note: suppressWarnings() is used because a call to "new" without all key
  # fields will produce a warning
  suppressWarnings(
    validObject(
      parameters <- do.call("new", c(Class=Class, fields)), complete=TRUE
    )
  )
  # Convert the S4 object to a list of only the parameters that were provided
  parameters <- as(parameters, "list")[names(fields)]

  # Submit a GET request and stop if an error is returned
  if (type == "Entity") {
    response <- stopIfHiveError(
      httpRequest(
        url=hiveURL("EntityQuery", "All", query=hivePreprocess(parameters)),
        method="GET", curl=con
      )
    )
    # This workaround is needed until a bug is fixed in the hive: when an
    # Entity is returned as part of a listing operation and the user does not
    # have permission to read it, the '_class' field of the Entity is
    # mistakenly excluded
    response <- lapply(response, "[[<-", "_class", parameters$.class)
  } else {
    response <- stopIfHiveError(
      httpRequest(
        url=hiveURL(hiveApp(type), query=hivePreprocess(parameters)),
        method="GET", curl=con
      )
    )
  }

  # Convert response to a 'SimpleList' object
  if (length(response)) {
    result <- hivePostprocess(response, type)
  } else {
    result <- new("SimpleList", elementType=Class)
  }

  # Define the slot that holds the ID of the object
  id.slot <- hiveSlotName(Class, "id")

  if (type == "Group") {
    # If type is 'Group', reduce result to a vector of group names
    result <- sapply(response, "[[", id.slot)
  } else if (simplify) {
    result <- as(result, "data.frame")
    # Convert any columns corresponding to non-atomic, non-UUID slots to
    # character representations
    i <- names(
      which(
        !sapply(lapply(slots, new), is.atomic) &
        !sapply(slots, extends, "UUID")
      )
    )
    result[i] <- lapply(result[i], lapply, as, "character")
    if (type == "User") {
      # If type is 'User', remove the 'password' field, which will always be
      # empty (the hive never returns passwords)
      result$password <- NULL
    } else if (type == "WorkFileProperties") {
      # If type is 'WorkFileProperties', copy the unique ID into the row names
      # of the result for convenience
      rownames(result) <- result[[id.slot]]
    }
#    } else if (type == "Entity") {
#      # If type is 'Entity', move object IDs to row names of data frame and
#      # remove Entity class
#      rownames(result) <- sapply(result[[id.slot]], as.character)
#      result[[id.slot]] <- NULL
#      result[[".class"]] <- NULL
#      # If there are any results, proceed to make them human-readable
#      if (nrow(result)) {
#        # For columns containing lists of arrays in which any of element is of
#        # length > 1, change the column to a vector of array lengths
#        array.columns <- which(
#          sapply(lapply(lapply(result, sapply, length), ">", 1), any)
#        )
#        if (length(array.columns)) {
#          result[array.columns] <- lapply(result[array.columns], sapply, length)
#          colnames(result)[array.columns] <- paste("#", names(array.columns))
#          colnames(result)[colnames(result) == "# .Data"] <- "length"
#        }
#        # Change columns of UUIDs to columns of character vectors
#        # Note: the following line uses intersect() because the ID field has
#        #       been moved to the row names of 'result'
#        uuid.columns <- intersect(
#          colnames(result), names(which(slots == "UUID"))
#        )
#        for (i in seq_along(uuid.columns)) {
#          result[[i]] <- sapply(result[[i]], as.character)
#        }
#      }
#    }
  }
  result
}
