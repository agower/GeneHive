#' @import bitops digest filenamer io methods RCurl S4Vectors uuidtools
#' @importFrom stats setNames

# [ ############################################################################

#' @rdname extract-methods
#' @name Entity extraction methods
#' @title Extract part of a hiveEntity
#' @description
#' These methods extract part of a \code{\linkS4class{hiveEntity}}
#' corresponding to a vector of indices.
#' @param x
#' A \code{\linkS4class{hiveEntity}} object from which to extract element(s)
#' @param i
#' A numeric, character or logical vector specifying elements to extract
#' @return
#' A \code{hiveEntity} object of the same length as \code{i}.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases [,hiveFeatureSpaceEntity-method
setMethod(
  "[",
  signature("hiveFeatureSpaceEntity"),
  function (x, i) 
  {
    if (missing(i)) i <- seq_along(x)
    x@values <- x@values[i]
    x
  }
)

#' @export
#' @rdname extract-methods
#' @aliases [,hiveFeatureSetEntity-method
setMethod(
  "[",
  signature("hiveFeatureSetEntity"),
  function (x, i) 
  {
    if (missing(i)) i <- seq_along(x)
    x@features <- x@features[i]
    x
  }
)

#' @export
#' @rdname extract-methods
#' @aliases [,hiveWeightedFeatureSetEntity-method
setMethod(
  "[",
  signature("hiveWeightedFeatureSetEntity"),
  function (x, i) 
  {
    if (missing(i)) i <- seq_along(x)
    x@features <- x@features[i]
    x@weights <- x@weights[i]
    x
  }
)

# coerce #######################################################################

#' @rdname coerce-io-methods
#' @name Coercion methods for 'io'
#' @title Coerce between GeneHive and io object classes
#' @description
#' These methods coerce GeneHive S4 objects to objects of classes defined in
#' package \pkg{io}, or vice versa.
#' @author Adam C. Gower \email{agower@@bu.edu}

setOldClass("gene.sets")

#' @export
#' @rdname coerce-io-methods
#' @name as
#' @aliases coerce,gene.sets,hiveEntityList-method
setAs(
  from="gene.sets", to="hiveEntityList",
  def = function (from)
  {
    new(
      "SimpleList",
      listData=mapply(hiveFeatureSetEntity, name=from$meta, features=from$data),
      elementType="hiveFeatureSetEntity"
    )
  }
)

#' @export
#' @rdname coerce-io-methods
#' @name as
#' @aliases coerce,hiveEntityList,gene.sets-method
setAs(
  from="hiveEntityList", to="gene.sets",
  def = function (from)
  {
    if (elementType(from) != "hiveFeatureSetEntity") {
      stop("The hiveEntityList does not contain hiveFeatureSetEntity objects")
    } else {
      list(
        meta = lapply(from, slot, "name"),
        data = lapply(from, slot, "features")
      )
    }
  }
)

#' @rdname coerce-GeneHive-methods
#' @name GeneHive coercion methods
#' @title Coerce GeneHive object classes
#' @description
#' These methods coerce GeneHive S4 objects to objects of other classes, and
#' vice versa.
#' @author Adam C. Gower \email{agower@@bu.edu}

# Note: as.POSIXct() cannot accept "%Z" as input in the format string
setAs(
  from="character", to="hiveDate",
  def=function (from) {
    tz <- getOption("GeneHive.timezone")
    hiveDate(as.POSIXct(from, tz=tz, format=paste("%a %b %d %T", tz, "%Y")))
  }
)
setAs(
  from="character", to="hiveFeatureSetEntity",
  def=function (from) hiveFeatureSetEntity(features=from)
)
setAs(
  from="character", to="hiveWorkFileID",
  def=function (from) hiveWorkFileID(.Data=from)
)
setAs(
  from="character", to="hiveWorkFileIDList",
  def = function (from) {
    new("hiveWorkFileIDList", listData=lapply(from, hiveWorkFileID))
  }
)

setAs(
  from="Date", to="hiveDate",
  def=function (from) hiveDate(as.POSIXct(from))
)

setAs(
  from="hiveDate", to="character",
  def=function (from) {
    format(from, tz=getOption("GeneHive.timezone"), "%a %b %d %T %Z %Y")
  }
)
as.character.hiveDate <- function (from) as(from, "character")

setAs(
  from="hiveDate", to="Date",
  def=function (from) as.Date(format(from, "%F"))
)
as.Date.hiveDate <- function (from) as(from, "Date")

#setAs(
#  from="hiveEntitySet", to="UUIDList",
#  def=function (from) from@ids
#)
setAs(
  from="hiveFeatureSpaceEntity", to="character",
  def=function (from) from@values
)
setAs(
  from="hiveFeatureSetEntity", to="character",
  def=function (from) from@features
)

# Used for tabulating permissions in hiveList() output
setAs(
  from="hivePermissions", to="character",
  def = function (from)
  {
    # By default, all objects have R/U/D permissions for owners
    result <- "RUD"
    for (name in c("group","other")) {
      for (permission in c("R","U","D")) {
        result <- paste0(
          result, if (permission %in% slot(from, name)) permission else "-"
        )
      }
    }
    result
  }
)

setAs(
  from="hiveUserList", to="character",
  def = function (from) sapply(from@listData, objectId)
)

#setAs(
#  from="hiveWeightedEntitySet", to="numeric",
#  def=function (from) setNames(from@weights, from@features)
#)
setAs(
  from="hiveWeightedFeatureSetEntity", to="numeric",
  def=function (from) setNames(from@weights, from@features)
)

# Used for tabulating WorkFile IDs in hiveList() output
setAs(
  from="hiveWorkFileIDList", to="character",
  def = function (from) as.character(unlist(from@listData))
)

setAs(
  from="numeric", to="hiveDate",
  def=function (from) hiveDate(as.POSIXct(from, origin="1970-01-01", tz="UTC"))
)

#setAs(
#  from="numeric", to="hiveWeightedEntitySet",
#  def = function (from) hiveWeightedEntitySet(ids=names(from), weights=from)
#)

setAs(
  from="numeric", to="hiveWeightedFeatureSetEntity",
  def = function (from) {
    hiveWeightedFeatureSetEntity(features=names(from), weights=from)
  }
)

setAs(
  from="numeric", to="hiveWorkFileID",
  def=function (from) hiveWorkFileID(.Data=as.character(from))
)

setAs(
  from="numeric", to="hiveWorkFileIDList",
  def = function (from) {
    new("hiveWorkFileIDList", listData=lapply(from, hiveWorkFileID))
  }
)

setAs(
  from="POSIXct", to="hiveDate",
  def=function (from) hiveDate(from)
)

#' @rdname coerce-S4-list-method
#' @name Coerce S4 to list
#' @aliases coerce,S4,list-method
#' @title Coerce an S4 object to a list
#' @description
#' This method is a convenience function used to coerce an S4 object to a list.
#' @details
#' \itemize{
#'   \item{
#'     This function will never be called directly, because
#'     \code{\link[=S4-class]{S4}} is a virtual class;
#'     however, it is useful to indicate that this function is intended to be
#'     used generally with S4 objects.
#'   }
#'   \item{
#'     \code{\link{setIs}(\linkS4class{hiveEntity}, S4)} cannot be used
#'     to ensure that this method is applied to Entity objects, as a conflict
#'     results for any Entity subclass with a .Data part; as a workaround, the
#'     definitions for classes that need to use this method are followed by a
#'     call to \code{setIs} with the parameter \code{class2="S4"} to ensure that
#'     they inherit the virtual class \code{S4}.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

setAs(
  from="S4", to="list",
  def = function (from)
  {
    result <- list()
    for (name in slotNames(class(from))) {
      result[[name]] <- unname(slot(from, name))
    }
    result
  }
)

#' @rdname coerce-SimpleList-data.frame-method
#' @name coerce SimpleList to data.frame
#' @aliases coerce,SimpleList,data.frame-method
#' @title Coerce a SimpleList object to a data frame
#' @description
#' This method is a convenience function used to coerce a
#' \code{\linkS4class{SimpleList}} object to a data frame.
#' @author Adam C. Gower \email{agower@@bu.edu}

setAs(
  from="SimpleList", to="data.frame",
  def = function (from)
  {
    # Initialize a data frame; note that if length(from) == 0,
    # the data frame will have 0 rows
    result <- data.frame(
      row.names = if (is.null(names(from))) seq_along(from) else names(from)
    )
    slot.classes <- getSlots(elementType(from))
    if (length(from)) {
      # Copy each slot into a new column of the data frame
      for (slot.name in names(slot.classes)) {
        result[[slot.name]] <- lapply(from, slot, slot.name)
        # If the column is a list with non-S4 elements that are all of length 1,
        # coerce it to a vector
        if (all(sapply(result[[slot.name]], length) == 1)) {
          if (length(getSlots(slot.classes[slot.name])) == 0) {
            result[[slot.name]] <- unlist(result[[slot.name]])
          }
        }
      }
    } else {
      # Create a list of empty character vectors
      result[names(slot.classes)] <- list(character(0))
    }
    result
  }
)

# annotateDatasetEntities ######################################################

#' @rdname annotateDatasetEntities
#' @name annotateDatasetEntities
#' @title Annotate the Entities in a dataset
#' @description
#' This is a generic function for annotating the Entities (instances) in a
#' dataset (e.g., the samples of an \code{eSet}) using some object.
#' @param dataset
#' An object containing a dataset (e.g., an \code{eSet}).
#' @param object
#' An object containing information to be used to annotate the Entities
#' (instances) of the dataset.
#' @param \dots
#' Additional arguments to methods of \code{annotateDatasetEntities}.
#' @return
#' Each method returns an S4 object of the same class as \code{dataset}.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
setGeneric(
  "annotateDatasetEntities",
  function (dataset, object, ...) dataset
)

# annotateDatasetFeatures ######################################################

#' @rdname annotateDatasetFeatures
#' @name annotateDatasetFeatures
#' @title Annotate the Features in a dataset
#' @description
#' This is a generic function for annotating the Features (observations) in a
#' dataset (e.g., the features of an \code{eSet}) using some object
#' (e.g., an annotation database).
#' @param dataset
#' An object containing a dataset (e.g., an @code{eSet}).
#' @param database
#' An object (e.g., an annotation database) containing information to be used
#' to annotate the Features (observations) of the dataset.
#' @param \dots
#' Additional arguments to methods of @code{annotateDatasetFeatures}.
#' @return
#' Each method returns an S4 object of the same class as @code{dataset}.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
setGeneric(
  "annotateDatasetFeatures",
  function (dataset, database, ...) dataset
)

# imputeFeatureSpace ###########################################################

#' @rdname imputeFeatureSpace
#' @name imputeFeatureSpace
#' @title Impute the FeatureSpace associated with an object
#' @description
#' This is a generic function for imputing the \code{FeatureSpace} associated
#' with one or more sets of features.
#' @param object
#' Either a \code{\linkS4class{hiveFeatureSetEntity}} or
#' \code{\linkS4class{hiveWeightedFeatureSetEntity}} object, a
#' \code{\linkS4class{hiveEntityList}} containing one or more of these objects,
#' or a \code{character} vector
#' @param con
#' A \code{\linkS4class{hiveConnection}} object
#' @section Methods:
#' Class-specific methods exist for:
#' \itemize{
#'   \item{\code{\linkS4class{hiveFeatureSetEntity}}}
#'   \item{\code{\linkS4class{hiveWeightedFeatureSetEntity}}}
#'   \item{\code{\linkS4class{hiveEntityList}}}
#'   \item{\code{\linkS4class{character}}}
#' }
#' @details
#' If a given set of features matches one or more FeatureSpaces consisting of a
#' set of features, as well as one or more FeatureSpaces consisting of regular
#' expressions, precedence is given to the FeatureSpace(s) that consist of a
#' set of features.
#' @return
#' If successful, each class-specific method returns a
#' \code{\linkS4class{UUIDList}} containing the following output for each
#' object in the \code{hiveEntityList}:
#' \itemize{
#'   \item{
#'     If the features in a given record do not match any
#'     \code{FeatureSpace}s, a nil \code{\linkS4class{UUID}}, with a warning.
#'   }
#'   \item{
#'     If the features match exactly one \code{FeatureSpace},
#'     the UUID of that \code{FeatureSpace}.
#'   }
#'   \item{
#'     If the features match more than one \code{FeatureSpace},
#'     the \code{UUID} of the smallest \code{FeatureSpace}.
#'   }
#' }
#' Otherwise, the function terminates with an error message.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
setGeneric(
  "imputeFeatureSpace",
  function (object, con=hiveConnection()) standardGeneric("imputeFeatureSpace")
)

#' @export
#' @rdname imputeFeatureSpace
setMethod(
  "imputeFeatureSpace",
  signature(object="hiveEntityList"),
  function (object, con=hiveConnection())
  {
    elementTypes <- c("hiveFeatureSetEntity", "hiveWeightedFeatureSetEntity")
    if (!((elementType(object) %in% elementTypes) && length(object) > 0)) {
      stop(
        "Argument 'object' must contain at least one object ",
        "of the following classes: ", paste(sQuote(elementTypes), collapse=", ")
      )
    }
    # For convenience, convert the object to a list of vectors of features
    sets <- lapply(object, slot, "features")
    # Retrieve a data frame of all FeatureSpaces and determine which contain
    # regular expressions
    featureSpaces <- listEntities(
      .class="FeatureSpace", simplify=FALSE, con=con
    )
    regex.spaces <- sapply(featureSpaces, length) == 1

    # Attempt to impute the FeatureSpace for each object in the list
    result <- UUIDList()
    for (i in seq_along(sets)) {
      set.identifier <- switch(
        elementType(object),
        "hiveFeatureSetEntity" = paste(elementType(object), i),
        "hiveWeightedFeatureSetEntity" = elementType(object)
      )
      # Identify which FeatureSpaces (if any)
      # correspond to the feature identifiers
      valid <- unlist(
        lapply(
          mapply(belongsToFeatureSpace, sets[i], featureSpaces, SIMPLIFY=FALSE),
          all
        )
      )
      # FeatureSpaces defined by vectors instead of regular expressions
      # take precedence
      if (any(valid[!regex.spaces])) valid <- valid & !regex.spaces
      # If the features did not match exactly one FeatureSpace,
      # issue a warning and return an empty string
      if (sum(valid) == 0) {
        warning.message <- paste(
          set.identifier, "does not match any FeatureSpaces"
        )
        excel.date.regex <-
          "^[0-9]+\\-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)$"
        excel.dates <- grepl(excel.date.regex, sets[[i]])
        if (any(excel.dates)) {
          warning.message <- paste(
            warning.message,
            "and contains the following identifiers, which appear to be gene",
            "symbols converted to dates by Excel:",
            paste(sQuote(sort(sets[[i]][excel.dates])), collapse=", ")
          )
        }
        warning(warning.message)
        result[[i]] <- UUID()
      } else if (sum(valid) == 1) {
        result[[i]] <- objectId(featureSpaces[[which(valid)]])
      } else if (sum(valid) > 1) {
        # If the feature identifiers match more than one FeatureSpace,
        # choose the smallest one and issue a warning
        j <- which.min(sapply(featureSpaces, length)[valid])
        valid.ids <- UUIDList(lapply(featureSpaces[which(valid)], objectId))
#        valid.names <- sapply(featureSpaces[which(valid)], objectName)
        valid.names <- sapply(featureSpaces[which(valid)], slot, "name")
        warning(
          paste(
            set.identifier, "matches FeatureSpaces:",
            paste0(
              paste(
                sprintf("%s (%s)", as.character(valid.ids), valid.names),
                collapse=", "
              ),
              ";"
            ),
            "using smallest FeatureSpace",
            sprintf("%s (%s)", as.character(valid.ids[[j]]), valid.names[j])
          )
        )
        result[[i]] <- valid.ids[[j]]
      }
    }
    result
  }
)

#' @export
#' @rdname imputeFeatureSpace
setMethod(
  "imputeFeatureSpace",
  signature(object="hiveFeatureSetEntity"),
  function (object, con=hiveConnection())
  {
    input <- hiveEntityList(listData=list(object))
    result <- tryCatch(imputeFeatureSpace(input, con=con)[1], warning=identity)
    if (is(result, "warning")) stop(result$message) else result
  }
)

#' @export
#' @rdname imputeFeatureSpace
setMethod(
  "imputeFeatureSpace",
  signature(object="hiveWeightedFeatureSetEntity"),
  function (object, con=hiveConnection())
  {
    input <- hiveEntityList(listData=list(object))
    result <- tryCatch(imputeFeatureSpace(input, con=con)[1], warning=identity)
    if (is(result, "warning")) stop(result$message) else result
  }
)

#' @export
#' @rdname imputeFeatureSpace
setMethod(
  "imputeFeatureSpace",
  signature(object="character"),
  function (object, con=hiveConnection())
  {
    imputeFeatureSpace(as(object, "hiveFeatureSetEntity"), con=con)
  }
)

# initialize ###################################################################

#' @export
#' @rdname initialize-hiveConnection
#' @name Initialize a hiveConnection
#' @aliases initialize,hiveConnection-method
#' @title Initialize a hiveConnection object
#' @description
#' This method is required to initialize the external pointer \code{ref} in a
#' \code{\linkS4class{hiveConnection}} object
#' (i.e., \code{\linkS4class{CURLHandle}}).
#' Note: in package \code{RCurl}, this is accomplished by
#' \code{\link{getCurlHandle}}, not by \code{\link{new}}.
#' @param .Object
#' An uninitialized \code{\linkS4class{hiveConnection}} object
#' @param \dots
#' Parameters used to populate the slots of \code{.Object}
#' @return
#' An initialized \code{\linkS4class{hiveConnection}} object.
#' @author Adam C. Gower \email{agower@@bu.edu}

setMethod(
  "initialize",
  signature("hiveConnection"),
  function (.Object, ...)
  {
    # Create a new CURLHandle and transfer the external pointer
    # into the hiveConnection object
    curl <- getCurlHandle()
    .Object@ref <- curl@ref
    # Return the initialized hiveConnection
    .Object
  }
)

#' @export
#' @rdname initialize-hiveEntity
#' @name Initialize a hiveEntity
#' @aliases initialize,hiveEntity-method
#' @title Initialize a hiveEntity object
#' @description
#' The \code{hiveEntity}-specific method performs the following tasks:
#' \itemize{
#'   \item{
#'     Initializes \code{.Object} using the parameters defined in the \dots
#'     argument
#'   }
#'   \item{
#'     Computes the \code{.entity_id} slot from the key fields (if any)
#'   }
#'   \item{
#'     If the key fields are empty,
#'     the slot \code{.entity_id} remains a nil UUID (default)
#'   }
#' }
#' @param .Object
#' An uninitialized \code{\linkS4class{hiveEntity}} object
#' @param \dots
#' Parameters used to populate the slots of \code{.Object}
#' @return
#' An initialized \code{\linkS4class{hiveEntity}} object.
#' @author Adam C. Gower \email{agower@@bu.edu}

setMethod(
  "initialize",
  signature("hiveEntity"),
  function (.Object, ...)
  {
    # Extract the '...' argument to a list,
    # and clear any '.class' argument if it exists
    # Note: slot '.class' should ONLY be set by the call to prototype()
    # in the class definition!
    dots <- list(...)
    dots$.class <- NULL
    # This step is required to populate the slots of the hiveEntity object
    # from '...'
    # Note: this call will also set the UUID of the object to a nil UUID
    #       (default) if one was not provided
    .Object <- do.call(callNextMethod, args=c(.Object=.Object, dots))
    if (.Object@.class != "Entity") {
      # Compute the UUID if one was not provided
      if (isNil(objectId(.Object))) {
        key.fields <- hiveKeyFields(.Object@.class)
        if (length(key.fields)) {
          # Set convenience variables
          hashing.algorithm <- getOption("GeneHive.hashing.algorithm")
          digest.args <- list(algo=hashing.algorithm, serialize=FALSE, raw=TRUE)
          # Convenience function for converting hexadecimal strings
          # (e.g., hashes) to raw vectors
          hexToRaw <- function (x) {
            n <- nchar(x) %/% 2
            as.raw(as.hexmode(mapply(substr, x, start=2*(1:n)-1, stop=2*(1:n))))
          }

          # Check whether any of the keys is missing
          # if so, throw a warning and exit
          keys.present <- structure(
            rep(NA, length(key.fields)), names=key.fields
          )
          for (key.field in key.fields) {
            key <- slot(.Object, key.field)
            keyClass <- class(key)
            if (keyClass == "hiveDate") {
              keys.present[[key.field]] <- length(key) > 0
            } else if (keyClass == "hiveWorkFileID") {
              keys.present[[key.field]] <- nchar(key) > 0
            } else if (keyClass == "UUID") {
              keys.present[[key.field]] <- !isNil(key)
            } else if (extends(keyClass, "character")) {
              keys.present[[key.field]] <- length(key) > 0
            } else {
              if (any(mapply(extends, keyClass, c("numeric", "SimpleList")))) {
                keys.present[[key.field]] <- length(key) > 0
              }
            }
          }
          if (any(!keys.present)) {
            warning(
              paste(
                "A UUID cannot be computed for an Entity record of class",
                sQuote(.Object@.class),
                 "if any of the following key slots are empty:",
                paste(sQuote(names(which(!keys.present))), collapse=", ")
              )
            )
          } else {
            # Initialize a list of UUIDs to be computed from each key
            key.hashes <- list()
            for (key.field in key.fields) {
              key <- slot(.Object, key.field)
              if (class(key) == "hiveWorkFileID") {
                key.hashes[[key.field]] <- hexToRaw(
                  getWorkFileProperties(key)@hash
                )
              } else if (class(key) == "hiveWorkFileIDList") {
                workFile.hashes <- sapply(
                  lapply(key, getWorkFileProperties), slot, "hash"
                )
                key.hashes[[key.field]] <- bitwiseParity(
                  lapply(workFile.hashes, hexToRaw)
                )
              } else if (class(key) == "UUID") {
                key.hashes[[key.field]] <- as.raw(key)
              } else if (class(key) == "UUIDList") {
                key.hashes[[key.field]] <- as.raw(
                  bitwiseParity(lapply(key, as.raw))
                )
              } else if (extends(class(key), "character")) {
                key.hashes[[key.field]] <- bitwiseParity(
                  do.call(
                    lapply, args=c(list(as.character(key), digest), digest.args)
                  )
                )
              } else if (extends(class(key), "numeric")) {
                if (!is.null(names(key))) {
                  # Convert names of vector to UUIDs if possible;
                  # otherwise, compute UUIDs from names of vector
                  key.names <- UUIDparse(names(key))
                  if (all(sapply(key.names, isNil))) {
                    key.names <- do.call(
                      lapply, args=c(list(names(key), digest), digest.args)
                    )
                  }
                  # If the vector is named, the names will be used
                  # along with the values (weights) to compute the UUID
                  if (all(is.element(key, c(-1, +1)))) {
                    # If all elements are weighted equally in one or two
                    # directions, compute the bitwise (even) parity of:
                    #   1. bitwise even parity of all elements with weight +1
                    #   2. bitwise odd parity of all elements with weight -1
                    key.hashes[[key.field]] <- as.raw(UUID())
                    if (any(key == +1)) {
                      key.hashes[[key.field]] <- bitwiseParity(
                        list(
                          key.hashes[[key.field]],
                          bitwiseParity(key.names[which(key == +1)], even=TRUE)
                        )
                      )
                    }
                    if (any(key == -1)) {
                      key.hashes[[key.field]] <- bitwiseParity(
                        list(
                          key.hashes[[key.field]],
                          bitwiseParity(key.names[which(key == -1)], even=FALSE)
                        )
                      )
                    }
                  } else {
                    # Otherwise, compute bitwise (even) parity of all elements
                    # and weights
                    # Note: weights are coerced to character before computing
                    #       the UUID; this is because it will be converted to
                    #       character (as JSON) during API call
                    key.hashes[[key.field]] <- bitwiseParity(
                      c(
                        key.names,
                        do.call(
                          lapply,
                          args=c(list(as.character(key), digest), digest.args)
                        )
                      )
                    )
                  }
                } else {
                  # If the vector is unnamed, the values alone will be used
                  key.hashes[[key.field]] <- bitwiseParity(
                    do.call(
                      lapply,
                      args=c(list(as.character(key), digest), digest.args)
                    )
                  )
                }
                key.hashes[[key.field]] <- as.raw(key.hashes[[key.field]])
              }
            }
            # Compute hash of the Entity class (used to establish a namespace)
            entity.class.hash <- do.call(
              digest, args=c(.Object@.class, digest.args)
            )
            # Truncate all hashes to 128 bits, in case SHA-1 hashing was used
            # (i.e., some key hashes may be 128-bit md5sums or UUIDs, and others
            # may be 160-bit SHA-1 hashes); then, combine key hashes with Entity
            # class hash by bitwise parity (namespace-based strategy)
            id <- bitwiseParity(
              lapply(c(list(entity.class.hash), key.hashes), "[", 1:16)
            )
            # Set the UUID variant bits to 1
            id[9] <- as.raw(bitOr(bitAnd(id[9], 0x3f), 0x80))
            # Set the UUID version bits to reflect the hashing algorithm used
            uuid.version <- c(md5=3L, sha1=5L)[hashing.algorithm]
            id[7] <- as.raw(
              bitOr(bitAnd(id[7], 0x0f), bitShiftL(uuid.version, 4))
            )
            # Convert to UUID and place in .Object
            objectId(.Object) <- UUID(id)
          }
        }
      }
    }
    .Object
  }
)

# length #######################################################################

#' @rdname length-methods
#' @name length
#' @title Get the length of a hiveEntity
#' @description
#' These methods get the length of a \code{\linkS4class{hiveEntity}} containing
#' a vector of identifiers or features.
#' @param x
#' A \code{\linkS4class{hiveEntity}} object
#' @return
#' An integer corresponding to the length of the internal vector.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases length,hiveFeatureSpaceEntity-method
setMethod(
  "length",
  signature("hiveFeatureSpaceEntity"), function (x) length(x@values)
)
#' @export
#' @rdname length-methods
#' @aliases length,hiveFeatureSetEntity-method
setMethod(
  "length",
  signature("hiveFeatureSetEntity"), function (x) length(x@features)
)
#' @export
#' @rdname length-methods
#' @aliases length,hiveWeightedFeatureSetEntity-method
setMethod(
  "length",
  signature("hiveWeightedFeatureSetEntity"), function (x) length(x@features)
)

# show #########################################################################

#' @rdname show-methods
#' @name S4 show methods
#' @title Methods for showing S4 objects
#' @description
#' These methods show the contents of various S4 objects in a human-readable
#' format.
#' @param object
#' An R object
#' @section Methods:
#' Class-specific methods exist for:
#' \itemize{
#'   \item \code{\linkS4class{hiveDate}}
#'   \item \code{\linkS4class{hiveEntity}}
#'   \item \code{\linkS4class{hiveEntityList}}
#'   \item \code{\linkS4class{hiveEntityClass}}
#'   \item \code{\linkS4class{hiveEntityClassList}}
#'   \item \code{\linkS4class{hivePermissions}}
#'   \item \code{\linkS4class{hiveUser}}
#'   \item \code{\linkS4class{hiveWorkFileProperties}}
#' }
#' @return
#' \code{show} returns an invisible \code{NULL}.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases show,hiveDate-method
setMethod(
  "show",
  signature("hiveDate"),
  function (object) cat(as(object, "character"), sep="\n")
)

#' @export
#' @rdname show-methods
#' @aliases show,hiveEntity-method
setMethod(
  "show",
  signature("hiveEntity"),
  function (object)
  {
    # Try to create a named vector of the variable types
    # within the Entity class definition
    variable.types <- try(
      sapply(
        getEntityClass(object@.class)@variables,
        function (variable) setNames(variable@type, variable@name)
      ),
      silent=TRUE
    )

    # Retrieve a vector of the class of each slot
    # in the corresponding S4 object class
    slot.classes <- getSlots(class(object))
    # Reorder slot classes in descending order from hiveEntity
    # through any S4 superclasses
    # e.g., for hiveSample: hiveEntity slots, then hiveWorkFileSet slots,
    # then hiveSample slots
    slot.classes <- slot.classes[
      unique(unlist(lapply(rev(extends(class(object))), slotNames)))
    ]
    # Print a header line containing the Entity UUID
    cat(
      object@.class,
      ifelse(
        isNil(objectId(object)), "(nil UUID)", as.character(objectId(object))
      ),
      "\n"
    )
    for (slot.name in grep("^[^\\.]", names(slot.classes), value=TRUE)) {
      # Determine whether the slot contains a "V" type variable
      # (Variable Entity)
      if (!inherits(variable.types, "try-error")) {
        treat.as.V.type <- (variable.types[slot.name] == "V")
      } else {
        treat.as.V.type <- FALSE
      }

      # Construct a character representation of the slot's contents
      slot.class <- slot.classes[slot.name]
      value <- slot(object, slot.name)
      if (slot.class == "UUID") {
        labels <- ifelse(isNil(value), "(nil UUID)", as.character(value))
      } else if (slot.class == "UUIDList") {
        labels <- ifelse(
          sapply(value, isNil), "(nil UUID)", sapply(value, as.character)
        )
        # If a GeneHive connection could be made and the variable is of type V,
        # look up the classes of the Entity records that the variable refers to
        if (treat.as.V.type) {
          entities <- lapply(labels, getEntity)
          entity.classes <- sapply(entities, slot, ".class")
          labels <- paste(entity.classes, labels)
        }
      } else if (slot.class == "hiveWorkFileID") {
        # Print "WorkFile WorkFile_ID"
        labels <- sprintf("WorkFile %s", value)
      } else if (slot.class == "hiveWorkFileIDList") {
        # Print "[WorkFile ID_1, ..., WorkFile ID_N]"
        labels <- sprintf("WorkFile %s", unlist(value@listData))
      } else{
        labels <- as.character(value)
      }
      if (slot.class == "character") labels <- sQuote(labels)
      # Print the name of the slot, and its contents
      if (treat.as.V.type) {
        cat(
          sprintf(
            "  %s: %d %s\n", slot.name, length(value),
            ifelse(length(value) == 1, "value", "values")
          )
        )
        for (i in seq_along(labels)) {
          cat(rep(" ", 2+nchar(slot.name)+2), labels[i], "\n", sep="")
        }
      } else {
        cat("  ", slot.name, ": ", sep="")
        if (length(value) == 1) {
          cat(labels)
        } else if (length(value) %in% 2:3) {
          cat(sprintf("[%s]", paste(labels, collapse=", ")))
        } else {
          cat(
            sprintf(
              "%d values [%s, %s, ..., %s]",
              length(value), labels[1], labels[2], labels[length(value)]
            )
          )
        }
        cat("\n")
      }
    }
    # Print metadata
    cat(
      "Created by", objectCreator(object),
      "on", as.character(objectCreationDate(object)), "\n"
    )
    cat("Last updated:", as.character(objectLastUpdated(object)), "\n")
    cat("Owned by:", objectOwner(object), "\n")
    cat("Associated with group:", objectGroup(object), "\n")
    show(objectPermissions(object))
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,hiveEntityList-method
setMethod(
  "show",
  signature("hiveEntityList"),
  function (object)
  {
    if (length(object)) {
      entity.class <- sub("^hive(.+)Entity$", "\\1", object@elementType)
      cat("List of", length(object), entity.class, "records\n")
      # Retrieve any label fields from the Entity class definition
      label.fields <- hiveLabelFields(entity.class)
      label.header <- "Label"
      # If none exist, use key fields instead
      if (length(label.fields) == 0) {
        label.fields <- hiveKeyFields(entity.class)
        label.header <- "Keys"
      }
      if (length(label.fields) > 0) {
        # Convenience function to extract vector of specified slots from object
        slots <- function (object, names) {
          mapply(slot, list(object), names, SIMPLIFY=FALSE)
        }
        labels <- sapply(
          lapply(lapply(object, slots, label.fields), sapply, as, "character"),
          paste, collapse=", "
        )
        field.widths <- c()
        field.widths["ID"] <- 36
        field.widths[label.header] <- max(nchar(labels))
        sprintf.fmt <- setNames(
          paste0("%-", field.widths, "s"), names(field.widths)
        )
        # Write an indented column header
        cat("  ")
        cat(sprintf(sprintf.fmt, names(sprintf.fmt)), "\n")
        for (i in seq_along(object)) {
          # Write an indented line for each Entity record
          cat("  ")
          cat(
            sprintf(sprintf.fmt["ID"], objectId(object[[i]])),
            sprintf(sprintf.fmt[label.header], labels[i])
          )
          cat("\n")
        }
      }
    } else {
      cat("Empty hiveEntityList\n")
    }
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,hiveEntityClass-method
setMethod(
  "show",
  signature("hiveEntityClass"),
  function (object)
  {
    cat("Definition for Entity class", objectId(object), "\n")
    cat("Description:", objectDescription(object), "\n")
    cat("Variables:\n")
    variable.types <- c(
      B="Boolean", C="Code", D="Date", E="Entity", F="Float", I="Integer",
      S="String", T="Text", W="WorkFile"
    )
    field.widths <- c(
      Name=max(nchar(sapply(object@variables, slot, "name"))),
      Key=max(nchar(c(TRUE,FALSE))),
      Array=max(nchar(c(TRUE,FALSE))),
      Type=0
    )
    sprintf.fmt <- setNames(
      paste0("%-", field.widths, "s"), names(field.widths)
    )
    # Write an indented column header
    cat("  ")
    cat(sprintf(sprintf.fmt, names(sprintf.fmt)), "\n")
    for (i in seq_along(object@variables)) {
      variable <- object@variables[[i]]
      # Write an indented line for each variable
      cat("  ")
      cat(
        sprintf(sprintf.fmt["Name"], variable@name),
        sprintf(sprintf.fmt["Key"], isTRUE(variable@category == "key")),
        sprintf(sprintf.fmt["Array"], variable@is_array),
        sprintf(sprintf.fmt["Type"], variable.types[[variable@type]])
      )
      # Add a qualifier for specific variable types
      if (variable@type == "C") {
        cat(" [", paste(variable@codes, collapse=", "), "]", sep="")
      } else if (variable@type == "E") {
        cat(" (", variable@entity_class_name, ")", sep="")
      }
      cat("\n")
    }
    # Print metadata
    cat(
      "Created by", objectCreator(object),
      "on", as.character(objectCreationDate(object)), "\n"
    )
    cat("Last updated:", as.character(objectLastUpdated(object)), "\n")
    cat("Owned by:", objectOwner(object), "\n")
    cat("Associated with group:", objectGroup(object), "\n")
    show(objectPermissions(object))
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,hiveEntityClassList-method
setMethod(
  "show",
  signature("hiveEntityClassList"),
  function (object)
  {
    if (length(object)) {
      cat("List of", length(object), "Entity class definitions\n")
      field.widths <- c(
        Name=max(nchar(sapply(object, objectId))),
        Description=0
      )
      sprintf.fmt <- setNames(
        paste0("%-", field.widths, "s"), names(field.widths)
      )
      # Write an indented column header
      cat("  ")
      cat(sprintf(sprintf.fmt, names(sprintf.fmt)), "\n")
      for (i in seq_along(object)) {
        # Write an indented line for each Entity class
        cat("  ")
        cat(
          sprintf(sprintf.fmt["Name"], objectId(object[[i]])),
          sprintf(sprintf.fmt["Description"], objectDescription(object[[i]]))
        )
        cat("\n")
      }
    } else {
      cat("Empty hiveEntityClassList\n")
    }
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,hivePermissions-method
setMethod(
  "show",
  signature("hivePermissions"),
  function (object)
  {
    permission.names <- c("R"="Read", "U"="Update", "D"="Delete")
    cat("Permissions:\n")
    cat("  Group: ")
    if (length(object@group) == 0) {
      cat("None") 
    } else {
      cat(paste(permission.names[object@group], collapse=", "))
    }
    cat("\n")
    cat("  Others: ")
    if (length(object@other) == 0) {
      cat("None")
    } else {
      cat(paste(permission.names[object@other], collapse=", "))
    }
    cat("\n")
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,hiveUser-method
setMethod(
  "show",
  signature("hiveUser"),
  function (object)
  {
    cat(
      sprintf(
        "User %s (%s, %s)\n",
        objectId(object),
        ifelse(object@active, "active", "inactive"),
        ifelse(object@superuser, "superuser", "non-superuser")
      )
    )
    cat("  Name:", object@firstName, object@lastName, "\n")
    cat("  Email:", object@email, "\n")
    cat("  Member of groups:", paste(object@groups, collapse=", "), "\n")
    cat("  Default group:", object@group, "\n")
    invisible(NULL)
  }
)

#' @export
#' @rdname show-methods
#' @aliases show,hiveWorkFileProperties-method
setMethod(
  "show",
  signature("hiveWorkFileProperties"),
  function (object)
  {
    cat(
      sprintf(
        "WorkFile %s (hash %s) (%s, %s)\n",
        objectId(object), object@hash,
        ifelse(object@isTransient, "temporary", "permanent"),
        ifelse(object@isTrashed, "trashed", "untrashed")
      )
    )
    cat("  ")
    if (nchar(object@fileType)) cat(object@fileType, "file: ")
    cat(format(object@length, big.mark=","), "bytes\n")
    # Print metadata
    cat("  Uploaded ")
    if (nchar(objectName(object))) {
      cat("as", sQuote(objectName(object)), "")
    }
    cat(
      "by", objectCreator(object),
      "on", as.character(objectCreationDate(object))
    )
    cat("\n")
    cat("  Associated with group:", objectGroup(object))
    cat("\n")
    cat("  Token:", object@token)
    cat("\n")
    show(objectPermissions(object))
    invisible(NULL)
  }
)

# translateFeatures ############################################################

#' @rdname translateFeatures
#' @name translateFeatures
#' @title Translate a set of features from one feature space to another
#' @description
#' This is a generic function for translating a set of features from one
#' feature space to another.
#' @param object
#' An object containing a set of features that will be translated from one
#' feature space to another
#' @param translator
#' An object containing a table, etc., that may be used to translate the
#' features in \code{object}
#' @param \dots
#' Additional arguments to methods of \code{translateFeatures},
#' e.g., \code{species}
#' @return
#' Each method returns either an S4 object of the same class as \code{object},
#' or a \code{\linkS4class{SimpleList}} of such objects.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
setGeneric(
  "translateFeatures",
  function (object, translator, ...) standardGeneric("translateFeatures")
)
