#' @import methods

#' @export
#' @rdname refreshEntityS4Class
#' @name refreshEntityS4Class
#' @title Refresh Entity S4 class definition
#' @description
#' This function refreshes the local S4 class definition corresponding to a
#' given Entity class.
#' @param class
#' A character string specifying the Entity class
#' @param verbose
#' Logical value specifying whether messages should be printed
#' @details
#' \itemize{
#'   \item{
#'     If the S4 class definition does not exist, it will be set in
#'     the \code{\link{.GlobalEnv}} environment.
#'   }
#'   \item{
#'     If the S4 class definition exists in one namespace, is unsealed, and
#'     does not match the current EntityClass definition, it will be refreshed.
#'   }
#'   \item{
#'     If the S4 class definition exists in two namespaces, one of which is
#'     \code{.GlobalEnv}, the definition in \code{.GlobalEnv} will be removed.
#'     If the remaining definition is unsealed and does not match the current
#'     EntityClass definition, it will be refreshed.
#'   }
#' }
#' @return
#' \itemize{
#'   \item{
#'     If the S4 class definition is successfully set or refreshed, the
#'     class constructor function (invisibly).
#'   }
#'   \item{
#'     If the S4 class definition exists in two or more namespaces (not
#'     including \code{.GlobalEnv}) the function terminates with an error.
#'   }
#'   \item{
#'     If a single S4 class definition exists, but is sealed,
#'     the function terminates with an error.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

refreshEntityS4Class <- function (class, verbose=getOption("GeneHive.verbose"))
{
  # Check arguments for errors
  if (missing(class)) stop("Argument 'class' is required")
  if (!(is.character(class) && length(class) == 1)) {
    stop("Argument 'class' must be a character vector of length 1")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  Class <- paste0("hive", class, "Entity")
  # Identify all environments that contain a definition for the Entity S4 class
  Class.envs <- suppressMessages(findClass(Class))
  names(Class.envs) <- sapply(Class.envs, environmentName)

  if (length(Class.envs) > 1) {
    if ((length(Class.envs) == 2) && ("R_GlobalEnv" %in% names(Class.envs))) {
      # If it is defined in more than one namespace, including .GlobalEnv,
      # remove the redundant definition from .GlobalEnv
      if (verbose) {
        cat(
          "Removing redundant definition of class ", sQuote(Class),
          " from .GlobalEnv\n"
        )
      }
      removeClass(Class, .GlobalEnv)
      Class.envs[["R_GlobalEnv"]] <- NULL
    } else {
      # Otherwise, terminate with an error message
      stop(
        "Class ", sQuote(Class),
        " exists in more than one namespace: ",
        sprintf("(%s)", paste(sQuote(names(Class.envs)), collapse=", ")),
        "; cannot refresh class definition"
      )
    }
  }
  remote.classdef <- getEntityClass(class)
  if (length(Class.envs) == 0) {
    # If it is undefined, set the S4 class definition in .GlobalEnv
    Class.namespace <- .GlobalEnv
    if (verbose) {
      cat(
        sprintf(
          "Setting definition for class '%s' in '%s'.\n",
          Class, environmentName(Class.namespace)
        )
      )
    }
    constructor <- setEntityS4Class(remote.classdef, where=Class.namespace)
  } else {
    Class.namespace <- Class.envs[[1]]
### getSlots(remote.classdef) doesn't make any sense - why did I write this?
### getSlots() only works with class names or classRepresentation objects
#    classDef.changed <- !identical(
#      getSlots(getClass(Class, Class.namespace)), getSlots(remote.classdef)
#    )
    # For now, just force it to update the S4 class definition each time
    classDef.changed <- TRUE
    if (classDef.changed) {
      if (isSealedClass(Class, Class.namespace)) {
        stop(
          "Definition of class ", sQuote(Class),
          " is sealed and cannot be refreshed"
        )
      } else {
        if (verbose) {
          cat(
            sprintf(
              "Refreshing definition for class '%s' in namespace '%s'.\n",
              Class, environmentName(Class.namespace)
            )
          )
        }
        # Unlock the S4 class definition
        # Note: unlockBinding() is called via do.call()
        #       to avoid a NOTE during checks
        do.call(
          unlockBinding,
          args=list(sym=classMetaName(Class), env=Class.namespace)
        )
        # Reset the S4 class definition
        constructor <- setEntityS4Class(remote.classdef, where=Class.namespace)
      }
    } else {
      if (verbose) {
        cat(sprintf("Definition for class '%s' is unchanged.\n", Class))
      }
    }
  }
  # Lock the S4 class definition
  lockBinding(classMetaName(Class), Class.namespace)
  # Return the constructor function, invisibly
  invisible(constructor)
}

#' @export
#' @rdname setEntityS4Class
#' @name setEntityS4Class
#' @title Set an Entity S4 class definition
#' @description
#' This function creates a local S4 class definition corresponding to a given
#' Entity class definition.
#' @param entityClassDef
#' A \code{\linkS4class{hiveEntityClass}} object
#' @param where
#' An environment in which to create the S4 class definition if needed
#' @return
#' The function calls \link{setClass} to create the S4 class definition within
#' its namespace, or, if it does not already exist, to create the S4 class
#' definition within the environment specified in argument \code{where}.
#' @author Adam C. Gower \email{agower@@bu.edu}

setEntityS4Class <- function (entityClassDef, where=.GlobalEnv)
{
  # Check arguments for errors
  if (missing(entityClassDef)) stop("Argument 'entityClassDef' is required")
  if (!is(entityClassDef, "hiveEntityClass")) {
    stop("Argument 'entityClassDef' must be a hiveEntityClass object")
  }
  if (!is(where, "environment")) {
    stop("Argument 'where' must be an environment")
  }

  # Convenience vector to translate Entity class definition "types" to R classes
  type.classes <- c(
    B="logical", C="factor", D="character", E="UUID", F="numeric", I="integer",
    S="character", T="character", V="UUID", W="hiveWorkFileID"
  )

  # Initialize argument list to setClass()
  Class <- paste0("hive", entityClassDef@name, "Entity")
  setClass.arglist <- list(
    Class=Class, contains="hiveEntity", sealed=FALSE, slots=c(), where=where
  )
  # Initialize argument list to prototype()
  prototype.arglist <- list(".class" = entityClassDef@name)
  # Note: cannot use the loop construct
  #         for (variable in entityClassDef@variables)
  #       with a SimpleList
  for (i in seq_along(entityClassDef@variables)) {
    variable <- entityClassDef@variables[[i]]
    slot.class <- type.classes[variable@type]
    if (slot.class %in% c("UUID", "hiveWorkFileID") & variable@is_array) {
      setClass.arglist$slots[variable@name] <- paste0(slot.class, "List")
    } else {
      setClass.arglist$slots[variable@name] <- slot.class
    }
    if (is.element(slot.class, c("character", "integer", "numeric"))) {
      prototype.arglist[[variable@name]] <- vector(
        mode=slot.class, length=ifelse(variable@is_array, 0, 1)
      )
    } else if (slot.class == "factor") {
      prototype.arglist[[variable@name]] <- factor(levels=variable@codes)
    } else if (slot.class %in% c("UUID", "hiveWorkFileID")) {
      prototype.arglist[[variable@name]] <- new(
        setClass.arglist$slots[variable@name]
      )
    }
  }
  setClass.arglist$prototype <- do.call(prototype, args=prototype.arglist)

  # Create the S4 class definition
  do.call(setClass, args=setClass.arglist)
}
