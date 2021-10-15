#' @import io S4Vectors

#' @export
#' @title Upload FeatureSet(s) from a file
#' @description
#' This function attempts to upload one or more
#' \code{\linkS4class{hiveFeatureSetEntity}} records from a file to a GeneHive.
#' @param filename
#' A character string specifying the full path to a file containing one or more
#' FeatureSets
#' @param featureSpace
#' An optional character string or \code{\linkS4class{UUID}} specifying the
#' \code{\linkS4class{hiveFeatureSpaceEntity}} associated with the FeatureSet.
#' If present, the FeatureSet will be checked against it for validity;
#' if missing, an attempt is made to impute the \code{FeatureSpace}
#' automatically.
#' @param \dots
#' Optional arguments to \code{\link{addEntity}}, e.g., \code{description}
#' or \code{species}
#' @param na.rm
#' A logical value specifying how to deal with features with NA values:
#' if \code{TRUE}, such features are removed from input;
#' if \code{FALSE}, the function terminates with an error message
#' @param .permissions
#' A \code{\linkS4class{hivePermissions}} object specifying the permissions
#' to be used when creating the record
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' If the operation is successful, the function invisibly returns a
#' \code{\linkS4class{hiveEntityList}} object containing one or more
#' FeatureSet objects;
#' otherwise, the function terminates with an error message.
#' @seealso
#' This function is a wrapper for the \code{\link[io]{qread}} function.
#' Its converse is \code{\link{exportFeatureSets}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

importFeatureSets <- function (
  filename, featureSpace, ..., na.rm=TRUE,
  .permissions=getOption("GeneHive.permissions"),
  con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(filename)) stop("Argument 'filename' is required")
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character vector of length 1")
  }
  valid.featureSpace <- 
    (is.character(featureSpace) || is(featureSpace, "UUID")) &&
    (length(featureSpace) == 1)
  if (!valid.featureSpace) {
    stop(
      "Argument 'featureSpace' must be a UUID or a character vector of length 1"
    )
  }
  if (!(is.logical(na.rm) && length(na.rm) == 1)) {
    stop("Argument 'na.rm' must be a logical vector of length 1")
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

  if (verbose) cat(paste("Importing FeatureSet(s) from", filename), "\n")
  sets <- qread(filename)
  # Check that the input file contains one or more FeatureSets
  if (hasMethod("coerce", c(class(sets), "hiveEntityList"))) {
    sets <- as(sets, "hiveEntityList")
  } else if (hasMethod("coerce", c(class(sets), "hiveFeatureSet"))) {
    # If the object contains only one FeatureSet,
    # for convenience, convert to a hiveEntityList of length 1
    sets <- hiveEntityList(listData=list(as(sets, "hiveFeatureSet")))
  } else {
    stop("No method exists to read FeatureSet objects from ", filename)
  }

  # Initialize a list of NULL values to hold the result
  result <- vector("list", length(sets))

  # Add any FeatureSets that are already present in the system to the result
  for (i in 1:length(sets)) {
    featureSet <- try(getEntity(objectId(sets[[i]])), silent=TRUE)
    if (!inherits(featureSet, "try-error")) {
      warning(
        paste("hiveFeatureSet", sQuote(objectId(sets[[i]])), "already exists")
      )
      result[[i]] <- featureSet
    }
  }

  # The result will hold NULL for any FeatureSet that does not already exist
  new.featureSets <- which(sapply(result, is.null))

  # Check the new FeatureSets for NA values,
  # and remove them or terminate with error message
  for (i in new.featureSets) {
    na.features <- which(is.na(sets[[i]]))
    if (length(na.features)) {
      if (na.rm) {
        warning(
          paste("Removing", length(na.features), "NA values from FeatureSet", i)
        )
        sets[[i]]@features <- sets[[i]][-na.features]
      } else {
        stop(
          paste(
            length(na.features), "features in FeatureSet", i,
            "contain NA values"
          )
        )
      }
    }
  }

  # Validate or impute the FeatureSpace corresponding to each new FeatureSet
  if (missing(featureSpace)) {
    featureSpaces <- imputeFeatureSpace(sets[new.featureSets], con=con)
    for (i in new.featureSets) sets[[i]]@featureSpace <- featureSpaces[i]
  } else {
    matching.sets <- sapply(lapply(sets, belongsToFeatureSpace), all)
    if (!all(matching.sets)) {
      stop(
        paste(
          "FeatureSet(s)", paste(which(!matching.sets), collapse=", "),
          "do not match FeatureSpace", featureSpace
        )
      )
    }
    for (i in new.featureSets) sets[[i]]@featureSpace <- featureSpace
  }

  name.field <- hiveSlotName("hiveFeatureSet", "name")
  for (i in new.featureSets) {
    if (nchar(sets[[i]]@featureSpace) > 0) {
      # Initialize an argument list from the FeatureSet object
      # and any '...' arguments
      arglist <- list(
        .Data=sets[[i]]@features, featureSpace=sets[[i]]@featureSpace, ...,
        .permissions=.permissions, con=con, verbose=verbose
      )
      if (!is.null(arglist[[name.field]])) {
        # If a descriptive name was provided as an argument,
        # and a description was extracted from the file,
        # paste the two together, with the argument serving as a prefix
        if (objectName(sets[[i]]) != "") {
          arglist[[name.field]] <-
            paste(arglist[[name.field]], objectName(sets[[i]]))
        }
      } else {
        # Otherwise, use the description extracted from the file
        arglist[[name.field]] <- objectName(sets[[i]])
      }
      # Add the FeatureSet record
      if (verbose) {
        cat(
          sprintf(
            "Adding FeatureSet '%s' ('%s').\n",
            objectId(sets[[i]]), arglist[[name.field]]
          )
        )
      }
      result[[i]] <- do.call(addEntity, args=c(.class="FeatureSet", arglist))
    }
  }

  # Coerce the result to a hiveEntityList of FeatureSets and return it invisibly
  result <- hiveEntityList(listData=result)
  invisible(result)
}

#' @export
#' @title Write FeatureSet(s) to a file
#' @description
#' This function attempts to write one or more
#' \code{\linkS4class{hiveFeatureSetEntity}} objects to a file.
#' @param object
#' A \code{\linkS4class{hiveFeatureSetEntity}} object or a
#' \code{\linkS4class{hiveEntityList}} object containing one or more
#' \code{hiveFeatureSetEntity} objects
#' @param filename
#' A character string specifying the full path to an output file
#' @return
#' If the operation is successful, the function writes the FeatureSet(s)
#' to the file specified by \code{filename}; otherwise, the function terminates
#' with an error message.
#' @seealso
#' This function is a wrapper for the \code{\link[io]{qwrite}} function.
#' Its converse is \code{\link{importFeatureSets}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

exportFeatureSets <- function (object, filename)
{
  # Check arguments for errors
  if (missing(object)) stop("Argument 'object' is required")
  valid.object <-
    is(object, "hiveFeatureSetEntity") ||
    (
      is(object, "hiveEntityList") && length(object) > 0 &&
      object@elementType == "hiveFeatureSetEntity"
    )
  if (!valid.object) {
    stop(
      "Argument 'object' must be a hiveFeatureSetEntity object ",
      "or a hiveEntityList of one or more hiveFeatureSetEntity objects"
    )
  }
  if (missing(filename)) stop("Argument 'filename' is required")
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character vector of length 1")
  }

  qwrite(as(object, "gene.sets"), filename)
}
