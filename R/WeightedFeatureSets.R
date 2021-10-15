#' @import io S4Vectors

#' @export
#' @title Upload WeightedFeatureSet from a file
#' @description
#' This function attempts to upload a
#' \code{\linkS4class{hiveWeightedFeatureSetEntity}} record from a file.
#' @param filename
#' Character string specifying the full path to a file containing a
#' \code{WeightedFeatureSet}
#' @param featureSpace
#' An optional \code{\linkS4class{UUID}} specifying the
#' \code{\linkS4class{hiveFeatureSpaceEntity}} associated with the
#' \code{WeightedFeatureSet}. If present, the \code{WeightedFeatureSet} will be
#' checked against it for validity; if missing, an attempt is made to impute
#' the \code{FeatureSpace} automatically.
#' @param \dots
#' Optional arguments to \code{\link{addEntity}},
#' e.g., \code{description}
#' @param na.rm
#' Logical value specifying how to deal with features with NA weights and/or
#' names: if \code{TRUE}, such features are removed from input; if \code{FALSE},
#' the function terminates with an error message
#' @param .permissions
#' A \code{\linkS4class{hivePermissions}} object specifying the permissions to
#' be used when creating the record
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#  if not provided, a new connection will be established
#' @param verbose
#' Logical value specifying whether messages should be printed
#' @return
#' If the operation is successful, the function invisibly returns a
#' \code{\linkS4class{hiveWeightedFeatureSetEntity}} object;
#' otherwise, the function
#' terminates with an error message.
#' @seealso
#' This function is a wrapper for the \code{\link[io]{qread}} function.
#' Its converse is \code{\link{exportWeightedFeatureSet}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

importWeightedFeatureSet <- function (
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

  if (!missing(featureSpace)) {
    valid.featureSpace <- 
      (is.character(featureSpace) && length(featureSpace) == 1) ||
      is(featureSpace, "UUID")
    if (!valid.featureSpace) {
      stop(
        "Argument 'featureSpace' must be a character vector of length 1 ",
        "or a UUID"
      )
    }
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

  if (verbose) cat(paste("Importing WeightedFeatureSet from", filename), "\n")
  set <- qread(filename)
  # Check that the input file contains a WeightedFeatureSet
  if (hasMethod("coerce", c(class(set), "hiveWeightedFeatureSet"))) {
    set <- as(set, "hiveWeightedFeatureSet")
  } else {
    stop("No method exists to read a WeightedFeatureSet from ", filename)
  }

  # Check for any features with NA weights and/or names,
  # and remove them or terminate with error message
  na.features <- which(is.na(set) | is.na(names(set)))
  if (length(na.features)) {
    if (na.rm) {
      warning(
        paste(
          "Removing", length(na.features),
          "features with NA weights and/or names"
        )
      )
      set@features <- set[-na.features]
    } else {
      stop(
        paste(length(na.features), "features contain NA weights and/or names")
      )
    }
  }

  # Validate or impute the FeatureSpace corresponding to the WeightedFeatureSet
  if (missing(featureSpace)) {
    featureSpace <- imputeFeatureSpace(set, con=con)
  } else {
    matching.features <- belongsToFeatureSpace(set, featureSpace)
    if (!all(matching.features)) {
      stop(
        paste(
          sum(!matching.features), "features do not match FeatureSpace",
          featureSpace
        )
      )
    }
  }

  # Add the WeightedFeatureSet record and return the result invisibly
  result <- addEntity(
    .class="WeightedFeatureSet",
    features=set@features, weights=set@weights, featureSpace=featureSpace, ...,
    .permissions=.permissions, con=con, verbose=verbose
  )
  invisible(result)
}

#' @export
#' @title Write WeightedFeatureSet to a file
#' @description
#' This function attempts to write a
#' \code{\linkS4class{hiveWeightedFeatureSetEntity}}
#' object to a file.
#' @param object
#' A \code{\linkS4class{hiveWeightedFeatureSetEntity}} object
#' @param filename
#' Character string specifying the full path to an output file
#' @param split
#' Logical value specifying whether a \code{WeightedFeatureSet} with weights of
#' only \code{+1} and \code{-1} should be split into two separate
#' \code{\linkS4class{hiveFeatureSetEntity}} records when exporting to the file
#' @param suffixes
#' Character vector of length 2, with names \code{"positive"} and
#' \code{"negative"}, indicating the suffixes to be added to the labels of each
#' \code{FeatureSet} if \code{split=TRUE}
#' @param verbose
#' Logical value specifying whether messages should be printed
#' @return
#' If the operation is successful, the function writes the
#' \code{WeightedFeatureSet} to the file specified by \code{filename};
#' otherwise, the function terminates with an error message.
#' @seealso
#' This function is a wrapper for the \code{\link[io]{qwrite}} function.
#' Its converse is \code{\link{importWeightedFeatureSet}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

exportWeightedFeatureSet <- function (
  object, filename, split=TRUE,
  suffixes=c(positive="_positive", negative="_negative"),
  verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(object) || missing(filename)) {
    stop("Arguments 'object' and 'filename' are required")
  }
  if (!(is(object, "hiveWeightedFeatureSet") && length(filename) > 0)) {
    stop("Argument 'object' must be a hiveWeightedFeatureSet of nonzero length")
  }
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character vector of length 1")
  }
  if (!(is.logical(split) && length(split) == 1)) {
    stop("Argument 'split' must be a logical vector of length 1")
  }
  if (!(is.character(suffixes) && length(suffixes) == 2)) {
    stop("Argument 'suffixes' must be a character vector of length 2")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  if (split) {
    if (!identical(sort(names(suffixes)), c("negative", "positive"))) {
      stop(
        "The elements of argument 'suffixes' must be named ",
        "'positive' and 'negative'"
      )
    }
  }

  # If the weights are only -1 and +1, and split == TRUE,
  # create a hiveEntityList object of two FeatureSets,
  # for features with weights of +1 and -1, respectively
  if (split && identical(sort(as.numeric(unique(object@weights))), c(-1, +1))) {
    featureSets <- list(
      new(
        "hiveFeatureSet", features=object@features[which(object@weights == +1)],
#        description=paste0(objectName(object), suffixes["positive"]),
        description=paste0(object@name, suffixes["positive"]),
        featureSpace=object@featureSpace
      ),
      new(
        "hiveFeatureSet", features=object@features[which(object@weights == -1)],
#        description=paste0(objectName(object), suffixes["negative"]),
        description=paste0(object@name, suffixes["negative"]),
        featureSpace=object@featureSpace
      )
    )
    object <- hiveEntityList(listData=object)
  }
  qwrite(as(object, "gene.sets"), filename)
}
