#' @import S4Vectors uuidtools
#' @importFrom stats setNames

#' @rdname hiveBase
#' @name Hive base functions
#' @title Add, delete, retrieve, update, or list record(s)
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
#' @param fields
#' A list of arguments specifying the unique identifier field of the record to
#' be updated (see \code{\link{hiveSlotName}}),
#' and/or any other fields to add or update or on which to limit a listing.
#' Note:
#' \itemize{
#'   \item{
#'     When limiting a listing, all of the elements of \code{fields} must be of
#'     length 1.
#'   }
#'   \item{
#'     When adding or listing Entity records, \code{fields} must always include
#'     a \code{.class} element.
#'   }
#' }
#' @param id
#' A vector of length 1 specifying the unique identifier field of the record to
#' be deleted or retrieved
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
#'     If the operation is successful, an S4 object (invisibly).
#'   }
#'   \item{\code{hiveGet}}{
#'     If the operation is successful, an S4 object.
#'   }
#'   \item{\code{hiveDelete}}{
#'     A logical value stating whether the operation was successful (invisibly).
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
  fields, verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (!(is.list(fields) && length(fields))) {
    stop("Argument 'fields' must be a list of nonzero length")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Determine the S4 object class of the output,
  # and if adding an Entity, stop if an Entity class was not specified
  Class <- do.call(hiveS4Class, args=c(type=type, fields))
  if (Class == "hiveEntity") {
    stop(
      "When adding Entity records, ",
      "argument 'fields' must contain a valid '.class' argument"
    )
  }
  # If adding an Entity, refresh the local S4 class definition
  if (type == "Entity") refreshEntityS4Class(fields$.class, verbose=FALSE)
  slots <- getSlots(Class)

  # Check to make sure that the 'fields' argument is named properly
  # (This check produces a more informative error message than initialize())
  # Note: allNames is used in case all elements of 'fields' are unnamed
  if (any(!is.element(allNames(fields), names(slots)))) {
    stop(
      "All arguments in argument 'fields' must be named, ",
      "and these names must correspond to valid ", Class, " object slots"
    )
  }

  # Convert list of arguments to S4 object to ensure all arguments are valid
  # and to coerce fields to proper classes
  object <- listToHiveS4(Class=Class, x=fields)
  # Replace contents of 'fields' variable with slots of S4 object
  fields <- as(object, "list")[names(fields)]

  # If the object ID was automatically computed and not explicitly provided,
  # move it into the 'fields' list; otherwise, the GeneHive server will
  # automatically assign a random (version 4) UUID to the record
  if (type == "Entity" && isNil(objectId(object))) {
    object.exists <- FALSE
  } else {
    id.slot <- hiveSlotName(Class, "id")
    fields[[id.slot]] <- objectId(object)
    # Check whether the record already exists
    object.exists <- hiveExists(fields[[id.slot]], type, con)
    if (object.exists) {
      # If the record already exists, produce a warning
      if (type == "Entity") {
        warning(
          paste(fields$.class, "record", fields[[id.slot]], "already exists")
        )
      } else {
        warning(
          paste(type, "record", sQuote(fields[[id.slot]]), "already exists")
        )
      }
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
          "Use hiveUpdate() to update fields: ",
          paste(sQuote(names(updates)), collapse=", ")
        )
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
    # Submit a POST request and stop if an error is returned
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
        cat(fields$.class, "record", as.character(objectId(result)))
      } else {
        cat(type, "record", sQuote(objectId(result)))
      }
      cat(" was successfully created.\n")
    }
  }
  invisible(result)
}

#' @rdname hiveBase
hiveDelete <- function (
  con=hiveConnection(), type=c("Entity", "EntityClass", "Group"),
  id, verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (missing(id) || length(id) != 1) {
    stop("Argument 'id' must be a vector of length 1")
  }
  id <- as.character(id)
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Get the record if it exists; if it does not exist, exit with an error
  object <- try(hiveGet(type=type, id=id, con=con), silent=TRUE)
  object.exists <- !inherits(object, "try-error")
  if (!object.exists) {
    if (type == "Entity") {
      stop(type, " record ", id, " does not exist")
    } else {
      stop(type, " record ", sQuote(id), " does not exist")
    }
  }
  # Submit a DELETE request and stop if an error is returned
  response <- stopIfHiveError(
    httpRequest(
      url=hiveURL(hiveApp(type), id), method="DELETE", curl=con
    )
  )
  if (is.list(response)) {
    # Convert the list to a logical vector
    # (there is only one element, 'success', in the result)
    response <- unname(unlist(response))
  } else {
    # When deleting a Group, 200 HTTP status code is returned with message:
    #   group: groupname successfully deleted -
    #          lets hope you didnt break something
    # so this line sets the result to TRUE as it would be for
    # type == "Entity" or type == "EntityClass"
    response <- TRUE
  }
  # Return the response
  if (verbose) {
    if (type == "Entity") {
      cat(object@.class, "record", as.character(objectId(object)))
    } else {
      cat(type, "record", sQuote(objectId(object)))
    }
    cat(" was successfully deleted.\n")
  }
  invisible(response)
}

#' @rdname hiveBase
hiveGet <- function (
  con=hiveConnection(),
  type=c("Entity", "EntityClass", "User", "WorkFileProperties"),
  id
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (missing(id) || length(id) != 1) {
    stop("Argument 'id' must be a vector of length 1")
  }
  id <- as.character(id)

  # Submit a GET request and stop if an error is returned
  response <- stopIfHiveError(
    httpRequest(url=hiveURL(hiveApp(type), id), method="GET", curl=con)
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
  fields, append=TRUE, verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (!(is.list(fields) && length(fields))) {
    stop("Argument 'fields' must be a list of nonzero length")
  }
  if (!(is.logical(append) && length(append) == 1)) {
    stop("Argument 'append' must be a logical vector of length 1")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Define the slot that holds the ID of the object
  Class <- hiveS4Class(type)
  id.slot <- hiveSlotName(Class, "id")
  # Ensure that an ID was provided for the object
  if (length(fields[[id.slot]]) == 0) {
    stop(
      "The ", sQuote(id.slot), " argument is required to update a ",
      type, "record"
    )
  }
  # Check to see if the record exists; if not, exit with an error
  object <- try(
    do.call(hiveGet, c(type=type, fields[id.slot], con=con)), silent=TRUE
  )
  if (inherits(object, "try-error")) {
    stop(
      type, " record ",
      ifelse(type == "Entity", fields[[id.slot]], sQuote(fields[[id.slot]])),
      " does not exist; add a new record instead"
    )
  }

  if (type == "Entity") {
    Class <- hiveS4Class(type, .class=object@.class)
    # If updating an Entity, refresh the local S4 class definition
    refreshEntityS4Class(object@.class, verbose=FALSE)
  }
  slots <- getSlots(Class)

  # Check to make sure that the 'fields' argument is named properly
  # (This check produces a more informative error message than initialize())
  # Note: allNames is used in case all elements of 'fields' are unnamed
  if (any(!is.element(allNames(fields), names(slots)))) {
    stop(
      "All arguments in argument 'fields' must be named, ",
      "and these names must correspond to valid ", Class, " object slots"
    )
  }

  # Convert list of arguments to S4 object to ensure all arguments are valid
  # Note: suppressWarnings() is used because a call to "new" without all key
  # fields will produce a warning
  suppressWarnings(updates <- listToHiveS4(Class=Class, x=fields))
  # Convert the S4 object to a list of only those fields that were provided
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
            cat(object@.class, "record", as.character(fields[[id.slot]]))
          } else {
            cat(type, "record", sQuote(fields[[id.slot]]))
          }
          cat(": ")
          cat(sQuote(new.elements), sep=", ")
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
      # (user@password will always be NA because passwords are never returned
      # as part of User records)
      if (!is.null(updates$password)) {
        if (checkPassword(fields[[id.slot]], updates$password)) {
          updates$password <- NULL
        }
      }
      # If group names were provided, check to make sure that they exist
      if (!is.null(updates$groups)) {
        valid.groups <- sapply(
          is.element(fields$groups, listGroups(con=con)$name), isTRUE
        )
        if (any(!valid.groups)) {
          stop(
            "The following groups do not exist: ",
            paste(sQuote(updates$groups[!valid.groups]), collapse=", ")
          )
        }
      }
      # If an email address was provided,
      # check to make sure that it is not already taken
      if (!is.null(updates$email)) {
        if (updates$email %in% listUsers(con=con)$email) {
          stop(
            "Cannot change email address to ", sQuote(updates$email),
            " as it is already in use"
          )
        }
      }
    }

    # Create list of updates
    update.list <- c(fields[id.slot], updates)
    # For User records, ensure the 'group' and 'groups' fields are present;
    # the 'group' field is required for updates, and the 'groups' field
    # of the record will be overwritten by if omitted
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
      # (required for updates)
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
        cat(object@.class, "record", as.character(fields[[id.slot]]))
      } else {
        cat(type, "record", sQuote(fields[[id.slot]]))
      }
      cat(" were updated: ")
      cat(sQuote(names(updates)), sep=", ")
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
  fields=list(), simplify=TRUE
)
{
  # Check arguments for errors
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  type <- match.arg(type)
  if (!missing(fields)) {
    if (!is.list(fields)) stop("Argument 'fields' must be a list")
  }
  # Only one value may be used to limit a listing
  if (any(sapply(fields, length) != 1)) {
    stop("All elements in argument 'fields' must be of length 1")
  }
  if (!(is.logical(simplify) && length(simplify) == 1)) {
    stop("Argument 'simplify' must be a logical vector of length 1")
  }

  # Determine the S4 object class of the output,
  # and if adding an Entity, stop if an Entity class was not specified
  Class <- do.call(hiveS4Class, args=c(type=type, fields))
  if (Class == "hiveEntity") {
    stop(
      "When listing Entity records, ",
      "argument 'fields' must contain a valid '.class' argument"
    )
  }
  # If listing Entities, refresh the local S4 class definition
  if (type == "Entity") {
    entityClassDef <- refreshEntityS4Class(fields$.class, verbose=FALSE)
  }
  slots <- getSlots(Class)

  # Check to make sure that the 'fields' argument is named properly
  # (This check produces a more informative error message than initialize())
  # Note: allNames is used in case all elements of 'fields' are unnamed
  if (any(!is.element(allNames(fields), names(slots)))) {
    stop(
      paste(
        "All arguments in 'fields' must be named,",
        "and these names must correspond to valid", Class, "object slots"
      )
    )
  }
  # Convert list of arguments to S4 object to ensure all arguments are valid
  # Note: suppressWarnings() is used because a call to "new" without all key
  # fields will produce a warning
  suppressWarnings(parameters <- listToHiveS4(Class=Class, x=fields))
  # Convert the S4 object to a list of only the parameters that were provided
  parameters <- as(parameters, "list")[names(fields)]

  # Submit a GET request and stop if an error is returned
  if (type == "Entity") {
    # If any of the parameters are array-type and non-empty,
    # replace them with their first (and only) element
    # (these should all be of length 1 due to the check made at top of function)
    for (i in seq_along(entityClassDef@variables)) {
      variable <- entityClassDef@variables[[i]]
      if (slot(variable, "is_array") && length(parameters[[variable@name]])) {
        parameters[[variable@name]] <- parameters[[variable@name]][[1]]
      }
    }
    # When hiveList() is called with type "Entity" and any UUID parameters
    # (i.e., to list only those Entities that refer to a given Entity), the
    # suffix ".id" must be added to the name of each UUID parameter.
    # For example, if FeatureSet Entities have a field 'featureSpace' that
    # holds the UUID of a FeatureSpace, a query for FeatureSets matching
    # FeatureSpace xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx would include:
    #   .class = "FeatureSet"
    #   featureSpace.id = xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
    i <- which(sapply(parameters, is, "UUID"))
    names(parameters)[i] <- paste0(names(parameters)[i], ".id")
    response <- stopIfHiveError(
      httpRequest(
        url=hiveURL("EntityQuery", "All", query=hivePreprocess(parameters)),
        method="GET", curl=con
      )
    )
    # This workaround is needed until a bug is fixed in the back end: when an
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

  if (simplify) {
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
      # empty (passwords are never returned)
      result$password <- NULL
    } else if (type == "WorkFileProperties") {
      # If type is 'WorkFileProperties', copy the unique ID into the row names
      # of the result for convenience
      rownames(result) <- result[[id.slot]]
    }
  }
  result
}
