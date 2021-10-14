#' @import methods RCurl S4Vectors uuidtools

# Classes are placed roughly in order of dependence and sorted alphabetically
# where possible.
#
# The setClass argument 'contains' arranges the superclasses in descending
# order by importance, so that calls to callNextMethod() in class-specific
# methods of initialize() will work as expected.
#
# Most class definitions in this file are sealed with the setClass() argument
#   sealed=TRUE
#
# However, the following classes are set as extensions of the virtual class
# "S4" using setIs():
#   hivePermissions, hiveUser, hiveGroup, hiveWorkFileProperties,
#   hiveEntityClass, hiveEntity
#
# This is to allow hiveList() and hiveUpdate() to use as() to convert object of
# these classes to lists using the method
#   coerce(signature(from="S4", to="list"))
#
# Because these class definitions must remain unsealed prior to the call to
# setIs(), a call to sealClass() is issued after the call to setIs().
#
# Furthermore, because of a bug in the sealClass() function (reported to
# R Bugzilla on 2018-11-02), the argument 'where' must be explicitly specified.

#' @export hiveConnection
#' @rdname hiveConnection-class
#' @title Class to contain a hive connection handle
#' @description
#' This S4 class is a container for a \code{cURL} handle that allows connection
#' to a GeneHive instance.
#' @slot .Data
#' A \code{\linkS4class{CURLHandle}} object.
#' @section Extends:
#' Directly extends class \code{\linkS4class{CURLHandle}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

setClass(
  "hiveConnection",
  contains = "CURLHandle",
  sealed = TRUE
)

#' @rdname hiveConnection-class
#' @export
#' @title Establish a connection to the hive via a cURL handle
#' @description
#' This function creates a new \code{\linkS4class{hiveConnection}} object to
#' establish a connection to the hive.
#' @param username
#' A character string denoting the user name to use when connecting
#' @param netrc.file
#' A character string specifying the location of the .netrc file
#' @param proxy
#' A logical value specifying whether or not a proxy server is to be used
#' @param verbose
#' A logical value specifying whether or not calls made using the
#' \code{\linkS4class{CURLHandle}} should be verbose
#' @return
#' A \code{\linkS4class{hiveConnection}} object containing the specified
#' parameters.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveConnection <- function (
  username = getOption("GeneHive.username"),
  netrc.file = getOption("GeneHive.netrc.file"),
  proxy = getOption("GeneHive.proxy"),
  verbose = FALSE
)
{
  # Check arguments for errors
  if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character vector of length 1")
  }
  if (!(is.character(netrc.file) && length(netrc.file) == 1)) {
    stop("Argument 'netrc.file' must be a character vector of length 1")
  }
  if (!(is.logical(proxy) && length(proxy) == 1)) {
    stop("Argument 'proxy' must be a logical vector of length 1")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Initialize a hiveConnection object containing a CURLHandle
  # Note: httpauth=TRUE is required to make API calls such as Entities and
  #       EntityClasses, which return HTTP code 200 even without any credentials
  con <- new("hiveConnection")
  curlSetOpt(httpauth=TRUE, verbose=verbose, curl=con)

  # Add the authentication information to the CURLHandle
  # Note: this persists across calls as long as this CURLHandle object is used

  # A hiveConnection object can only be established using a .netrc file.
  # There are several reasons why there is no password argument or prompt:
  # - A password argument leaves open the possibility of a user including a
  #   password in an R script, which is very poor security practice.
  # - A manually entered password would be echoed to the terminal
  #   (unless 'stty -echo' was used in a *nix environment; there is no such
  #   option for Windows environments), which is also not a secure option.
  # - Manual password entry is only especially convenient for the first login
  #   (i.e., when no .netrc file yet exists); changePassword() creates the
  #   .netrc file automatically, so this should not be a burden to the user.
  # - Prompting for a password would require either that hiveConnection()
  #   return two different types of CURLHandle (those with 'netrc'/'netrc.file'
  #   set, and those with 'userpwd' set), which is not good practice, or that
  #   hiveConnection() be allowed to create a .netrc file for first use, which
  #   would be redundant with changePassword().

  # If the netrc.file exists, then use it; otherwise, terminate with an error
  if (file.exists(netrc.file)) {
    curlSetOpt(netrc=TRUE, netrc.file=netrc.file, username=username, curl=con)
  } else {
    stop("File ", netrc.file, " does not exist")
  }

  # Shut off the proxy if requested
  if (!proxy) curlSetOpt(proxy="", curl=con)

  # Return the hiveConnection object
  con
}

#' @export hivePermissions
#' @rdname hivePermissions-class
#' @title Class to contain permissions for a hive record
#' @description
#' This S4 class is a container for a set of permissions associated with a
#' GeneHive record.
#' @slot group
#' A character vector specifying the permissions that are granted
#' to members of the Group associated with the record:
#' may include \code{"R"}ead, \code{"U"}pdate, or \code{"D"}elete.
#' Defaults to an empty vector (i.e., no permissions).
#' @slot other
#' A character vector specifying the permissions that are granted
#' to all Users:
#' may include \code{"R"}ead, \code{"U"}pdate, or \code{"D"}elete.
#' Defaults to an empty vector (i.e., no permissions).
#' @author Adam C. Gower \email{agower@@bu.edu}

setClass(
  "hivePermissions",
  slots = c(
    group = "character",
    other = "character"
  ),
  prototype = prototype(
    group = character(0),
    other = character(0)
  ),
  validity = function (object)
  {
    errors <- c()
    # Then check to make sure all operations are valid
    operations <- c("R","U","D")
    for (name in slotNames("hivePermissions")) {
      if (any(!is.element(slot(object, name), operations))) {
        errors <- c(
          errors,
          paste(
            "Values in slot", sQuote(name), "must be one of the following:",
            paste(sQuote(operations), collapse=",")
          )
        )
      }
    }
    if (length(errors) == 0) TRUE else errors
  }
)
setIs("hivePermissions", "S4")
sealClass("hivePermissions", where=.GlobalEnv)

#' @rdname hivePermissions-class
#' @param \dots
#' Optional arguments containing permission information to be used to populate
#' slots of output

hivePermissions <- function (...)
{
  result <- getOption("GeneHive.permissions")
  arglist <- list(...)
  slots <- getSlots("hivePermissions")
  for (slot.name in names(arglist)) {
    to.class <- slots[slot.name]
    slot(result, slot.name) <- as(arglist[[slot.name]], to.class)
    if (to.class == "character") {
      # Convert to unique uppercase characters without NA values
      slot(result, slot.name) <- unique(
        toupper(na.omit(slot(result, slot.name)))
      )
    }
  }
  result
}

#' @export hiveUser
#' @rdname hiveUser-class
#' @title Class to contain data about a User
#' @description
#' This class is the S4 representation of the User record type.
#' It is a container for data about a given user.
#' @slot username
#' A character string specifying the name of the user.
#' Defaults to \code{""}.
#' @slot password
#' A character string specifying the password of the user.
#' Defaults to \code{NA}.
#' @slot group
#' A character string specifying the name of the Group associated with
#' the user. Defaults to \code{""}.
#' @slot superuser
#' A logical value specifying whether the user is a superuser.
#' Defaults to \code{FALSE}.
#' @slot email
#' A character string specifying the email address of the user.
#' Defaults to \code{""}.
#' @slot firstName
#' A character string specifying the first name of the user.
#' Defaults to \code{""}.
#' @slot lastName
#' A character string specifying the last name of the user.
#' Defaults to \code{""}.
#' @slot dateJoined
#' A character string specifying the date that the User record was
#' created. Automatically populated by the hive.
#' @slot untrashedFileUsage
#' A numeric value specifying the file usage of the user.
#' Automatically populated by the hive.
#' @slot token
#' A character string specifying a token that can be used for password-free
#' authentication. Automatically populated by the hive.
#' @slot active
#' A logical value specifying whether the user is active.
#' Defaults to \code{FALSE}.
#' @seealso
#' \describe{
#'   \item{group}{
#'     This slot corresponds to the \code{\linkS4class{hiveGroup}} object slot
#'     \code{name}.
#'   }
#' }
#' Functions for working with User records in a GeneHive are described in
#' \code{\link{Users}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveUser <- setClass(
  "hiveUser",
  slots = c(
    username           = "character",
    password           = "character",
    group              = "character",
    groups             = "character",
    superuser          = "logical",
    confirmed          = "logical",
    email              = "character",
    firstName          = "character",
    lastName           = "character",
    dateJoined         = "character",
    lastLogin          = "character",
    untrashedFileUsage = "numeric",
    token              = "character",
    active             = "logical"
  ),
  prototype = prototype(
    username           = "",
    password           = NA_character_,
    group              = "",
    groups             = character(0),
    superuser          = FALSE,
    confirmed          = FALSE,
    email              = "",
    firstName          = "",
    lastName           = "",
    dateJoined         = "",
    lastLogin          = "",
    untrashedFileUsage = NA_real_,
    token              = "",
    active             = FALSE
  )
)
setIs("hiveUser", "S4")
sealClass("hiveUser", where=.GlobalEnv)

#' @export hiveUserList
#' @rdname hiveUserList-class
#' @title Class to contain a list of User objects
#' @description
#' This class is a container for one or more User objects.
#' @section Extends:
#' This class directly extends class \code{\linkS4class{SimpleList}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

# Container for a list of hiveUser objects
setClass(
  "hiveUserList",
  prototype = new("SimpleList", elementType="hiveUser"),
  validity = function (object)
  {
    error.messages <- list(
      too.many.classes =
        "A hiveUserList may only contain objects of a single class",
      not.users =
        "A hiveUserList must contain objects of class 'hiveUser'"
    )
    if (length(object@elementType) > 1) {
      error.messages$too.many.classes
    } else if (object@elementType != "hiveUser") {
      error.messages$not.users
    } else if (length(unique(unlist(lapply(object@listData, class)))) > 1) {
      error.messages$too.many.classes
    } else {
      TRUE
    }
  },
  contains = "SimpleList",
  sealed = TRUE
)

#' @rdname hiveUserList-class
#' @param listData
#' A list of objects of class \code{\linkS4class{hiveUser}}
#' @details
#' This constructor function does not require that the \code{listData} argument
#' be named.

hiveUserList <- function (listData=list())
{
  # Check arguments for errors
  if (!is.list(listData)) stop("Argument 'listData' must be a list")

  # Return a list named by the IDs of the Users it contains (if any)
  if (length(listData)) {
    new(
      "hiveUserList",
      listData=setNames(
        listData, sapply(lapply(listData, objectId), as.character)
      ),
      elementType=unique(sapply(listData, class))
    )
  } else {
    new("hiveUserList", listData=listData)
  }
}

#' @export hiveGroup
#' @rdname hiveGroup-class
#' @title Class to contain data about a Group
#' @description
#' This class is the S4 representation of the Group record type.
#' It is a container for data about a given group of Users.
#' @slot name
#' A character string specifying the name of the group.
#' Defaults to \code{""}.
#' @slot users
#' A \code{\linkS4class{hiveUserList}} specifying the users in the group.
#' Defaults to an empty \code{hiveUserList}.
#' @seealso
#' Functions for working with Group records in the hive are described in
#' \code{\link{Groups}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveGroup <- setClass(
  "hiveGroup",
  slots = c(
    name  = "character",
    users = "hiveUserList"
  ),
  prototype = prototype(
    name  = "",
    users = hiveUserList()
  )
)
setIs("hiveGroup", "S4")
sealClass("hiveGroup", where=.GlobalEnv)

#' @export hiveWorkFileID
#' @rdname hiveWorkFileID-class
#' @title Class to contain a WorkFile identifier
#' @description
#' This class is a container for the unique identifier of a WorkFile.
#' @slot .Data
#' A character string specifying the unique identifier of the WorkFile
#' (most likely a character representation of an integer, though this is not
#' guaranteed).
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveWorkFileID <- setClass(
  "hiveWorkFileID",
  prototype = "",
  contains = "character",
  sealed = TRUE
)

#' @export hiveWorkFileProperties
#' @rdname hiveWorkFileProperties-class
#' @title Class to contain WorkFile metadata
#' @description
#' This class is the S4 representation of the WorkFileProperties record
#' type.  It is a container for metadata about a given WorkFile.
#' @slot id
#' A \code{\linkS4class{hiveWorkFileID}} specifying the unique identifier of the
#' WorkFile. Automatically created when the file is uploaded.
#' @slot hash
#' A character string specifying the MD5 checksum of the file.
#' Automatically created when the file is uploaded.
#' @slot creator
#' A character string specifying the \code{username} of the User
#' associated with the record. Automatically created when the file is uploaded.
#' @slot group
#' A character string specifying the \code{name} of the Group associated
#' with the record. Automatically created when the file is uploaded.
#' @slot creatorJobRun
#' A character string specifying the name of a JobRun associated with the
#' WorkFile. (Not currently used.)
#' Automatically created when the file is uploaded.
#' @slot originalName
#' A character string specifying the original name of the file.
#' Automatically created when the file is uploaded.
#' @slot fileType
#' A character string specifying the type (e.g., extension) of the file.
#' Created when the file is uploaded.
#' @slot isTrashed
#' A logical value specifying whether the WorkFile is flagged to be
#' trashed. Defaults to \code{FALSE}.
#' @slot isTransient
#' A logical value specifying whether the WorkFile is flagged as
#' temporary. Defaults to \code{FALSE}.
#' @slot creationDatetime
#' A character string specifying the date and time that the record was created.
#' Automatically created when the file is uploaded.
#' @slot length
#' A numeric value specifying the length of the file in bytes.
#' Automatically created when the file is uploaded.
#' @slot token
#' A character string specifying a token that can be used for password-free
#' authentication. Automatically created when the file is uploaded.
#' @slot permissions
#' A \code{\linkS4class{hivePermissions}} object specifying the permissions
#' associated with the record. Defaults to read-only.
#' @seealso
#' Functions for working with WorkFileProperties records in a GeneHive
#' are described in \code{\link{WorkFileProperties}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveWorkFileProperties <- setClass(
  "hiveWorkFileProperties",
  slots = c(
    id                   = "hiveWorkFileID",
    hash                 = "character",
    creator              = "character",
    group                = "character",
    storage              = "character",
    creatorJobRun        = "character",
    originalName         = "character",
    originalModifiedTime = "numeric",
    fileType             = "character",
    isTrashed            = "logical",
    isTransient          = "logical",
    creationDatetime     = "character",
    length               = "numeric",
    token                = "character",
    permissions          = "hivePermissions"
  ),
  prototype = prototype(
    id                   = new("hiveWorkFileID"),
    hash                 = "",
    creator              = "",
    group                = "",
    storage              = "",
    creatorJobRun        = "",
    originalName         = "",
    originalModifiedTime = -1,
    fileType             = "",
    isTrashed            = FALSE,
    isTransient          = FALSE,
    creationDatetime     = "",
    length               = 0,
    token                = "",
    permissions          = new("hivePermissions")
  )
)
setIs("hiveWorkFileProperties", "S4")
sealClass("hiveWorkFileProperties", where=.GlobalEnv)

#' @export hiveWorkFileIDList
#' @rdname hiveWorkFileIDList-class
#' @title Class to contain a list of WorkFile identifiers
#' @description
#' This class is a container for the unique identifier of one or more
#' WorkFiles.
#' @section Extends:
#' This class directly extends class \code{\linkS4class{SimpleList}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

# Container for a list of hiveWorkFileID objects
setClass(
  "hiveWorkFileIDList",
  prototype = new("SimpleList", elementType="hiveWorkFileID"),
  validity = function (object)
  {
    error.message <- 
      "A hiveWorkFileIDList may only contain hiveWorkFileID objects"
    if (!identical(object@elementType, "hiveWorkFileID")) {
      error.message
    } else if (!all(unlist(lapply(object@listData, is, "hiveWorkFileID")))) {
      error.message
    } else {
      TRUE
    }
  },
  contains = "SimpleList",
  sealed = TRUE
)

#' @rdname hiveWorkFileIDList-class
#' @param listData
#' A list of \code{\linkS4class{hiveWorkFileID}} objects
#' @details
#' This constructor function does not require that the \code{listData} argument
#' be named.

hiveWorkFileIDList <- function (listData=list())
{
  # Check arguments for errors
  if (!is.list(listData)) stop("Argument 'listData' must be a list")

  new("hiveWorkFileIDList", listData=listData)
}

########## EntityClass-related classes ##########

#' @export hiveVariableDefinition
#' @rdname hiveVariableDefinition-class
#' @title Class to contain VariableDefinition records
#' @description
#' This class is the S4 representation of the hive VariableDefinition
#' record type.  It is a container for a single variable associated with an
#' EntityClass record.
#' @slot name
#' A character string specifying the name of the record.
#' Defaults to \code{""}.
#' @slot category
#' A character string describing the category of the record.
#' Defaults to \code{NA}.
#' @slot type
#' A one-character string specifying the type of the record.
#' Defaults to \code{""}.  May take the value \code{'C'} (Code),
#' \code{'D'} (Date), \code{'E'} (Entity), \code{'F'} (Float),
#' \code{'I'} (Integer), \code{'S'} (String), \code{'T'} (Text),
#' or \code{'W'} (WorkFile).
#' @slot codes
#' A character vector specifying the allowed values that the variable may take
#' (if \code{type} is \code{'C'}).
#' Defaults to an empty vector.
#' @slot entity_class_name
#' A character string specifying the name of the EntityClass that the
#' variable must belong to (if \code{type} is \code{'E'}).
#' Defaults to \code{NA}.
#' @slot is_array
#' A logical value specifying whether the variable is an array.
#' Defaults to \code{FALSE}.
#' @seealso
#' VariableDefinition objects are contained in
#' \code{\linkS4class{hiveVariableDefinitionCollection}} objects.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveVariableDefinition <- setClass(
  "hiveVariableDefinition",
  slots = c(
    name              = "character",
    category          = "character",
    type              = "character",
    codes             = "character",
    entity_class_name = "character",
    is_array          = "logical"
  ),
  prototype = prototype(
    name              = "",
    category          = NA_character_,
    type              = "",
    codes             = character(0),
    entity_class_name = NA_character_,
    is_array          = FALSE
  ),
  validity = function (object)
  {
    errors <- c()
    # The 'type' slot must contain a single letter corresponding to a known type
    types <- c(
      boolean="B", code="C", date="D", entity="E", float="F", integer="I",
      string="S", text="T", variable="V", workFile="W"
    )
    if (length(object@type) != 1) {
      errors <- paste("The vector in slot 'type' must be of length 1")
    } else {
      if (!is.element(object@type, types)) {
        errors <- paste(
          "The value in slot 'type' must be one of the following:",
          paste(sQuote(types), collapse=",")
        )
      } else {
        # Check slot 'codes'
        if (((length(object@codes) > 0) != (object@type == "C"))) {
          errors <-
            "Slot 'codes' must contain values if and only if slot 'type' = 'C'"
        } else {
          # If type = 'C', the 'codes' slot must not contain NA values
          if (object@type == "C" && any(is.na(object@codes))) {
            errors <- "Slot 'codes' may not contain any NA values"
          }
        }
        # Check slot 'entity_class_name'
        if (length(object@entity_class_name) != 1) {
          errors <- c(
            errors,
            paste("The vector in slot 'entity_class_name' must be of length 1")
          )
        } else {
          if (((!is.na(object@entity_class_name)) != (object@type == "E"))) {
            errors <- c(
              errors,
              paste(
                "Slot 'entity_class_name' must contain a non-NA value",
                "if and only if slot 'type' = 'E'"
              )
            )
          }
        }
      }
    }
    if (length(errors) == 0) TRUE else errors
  },
  sealed = TRUE
)

#' @export hiveVariableDefinitionCollection
#' @rdname hiveVariableDefinitionCollection-class
#' @title Class to contain VariableDefinitionCollection records
#' @description
#' This class is the S4 representation of the hive
#' VariableDefinitionCollection record type.
#' It is a container for the variables and metadata associated with a collection
#' of variable definitions in an EntityClass record.
#' @slot elementType
#' Contains the character string \code{"VariableDefinition"}.
#' @slot listData
#' A list of \code{\linkS4class{hiveVariableDefinition}} objects.
#' @section Extends:
#' Directly extends class \code{\linkS4class{SimpleList}}.
#' @seealso
#' VariableDefinitionCollection objects are contained in
#' \code{\linkS4class{hiveEntityClass}} objects.
#' @author Adam C. Gower \email{agower@@bu.edu}

setClass(
  "hiveVariableDefinitionCollection",
  prototype = new("SimpleList", elementType = "hiveVariableDefinition"),
  contains = "SimpleList",
  sealed = TRUE
)

#' @rdname hiveVariableDefinitionCollection-class
#' @param listData
#' A list of hiveVariableDefinition objects

hiveVariableDefinitionCollection <- function (listData=list())
{
  new("hiveVariableDefinitionCollection", listData=listData)
}

#' @export hiveEntityClass
#' @rdname hiveEntityClass-class
#' @title Class to contain EntityClass records
#' @description
#' This class is the S4 representation of the EntityClass record type.
#' It is a container for the definition of an Entity class.
#' @slot name
#' A character string specifying the name of the record.
#' Defaults to \code{""}.
#' @slot description
#' A character string containing a description of the record.
#' Defaults to \code{""}.
#' @slot variables
#' A \code{\linkS4class{hiveVariableDefinitionCollection}} object containing
#' any variables associated with the record.
#' @slot creator
#' A character string specifying the \code{username} of the User
#' who created the record. Defaults to \code{""}.
#' @slot owner
#' A character string specifying the \code{username} of the User
#' associated with the record. Defaults to \code{""}.
#' @slot group
#' A character string specifying the \code{name} of the Group associated
#' with the record. Defaults to \code{""}.
#' @slot permissions
#' A \code{\linkS4class{hivePermissions}} object specifying the permissions
#' associated with the record. Defaults to read-only.
#' @seealso
#' Functions for working with EntityClass records in the hive are
#' described in \code{\link{EntityClasses}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveEntityClass <- setClass(
  "hiveEntityClass",
  slots = c(
    name           = "character",
    description    = "character",
    variables      = "hiveVariableDefinitionCollection",
    creator        = "character",
    owner          = "character",
    group          = "character",
    permissions    = "hivePermissions",
    .creation_date = "character",
    .updated       = "character"
  ),
  prototype = prototype(
    name           = "",
    description    = "",
    variables      = new("hiveVariableDefinitionCollection"),
    creator        = "",
    owner          = "",
    group          = "",
    permissions    = new("hivePermissions"),
    .creation_date = "",
    .updated       = ""
  )
)
setIs("hiveEntityClass", "S4")
sealClass("hiveEntityClass", where=.GlobalEnv)

#' @export hiveEntityClassList
#' @rdname hiveEntityClassList-class
#' @title Class to contain a list of EntityClass objects
#' @description
#' This class is a container for one or more EntityClass objects.
#' @section Extends:
#' This class directly extends class \code{\linkS4class{SimpleList}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

# Container for a list of hiveEntityClass objects
setClass(
  "hiveEntityClassList",
  prototype = new("SimpleList", elementType="hiveEntityClass"),
  validity = function (object)
  {
    error.message <- 
      "A hiveEntityClassList may only contain hiveEntityClass objects"
    if (!identical(object@elementType, "hiveEntityClass")) {
      error.message
    } else if (!all(unlist(lapply(object@listData, is, "hiveEntityClass")))) {
      error.message
    } else {
      TRUE
    }
  },
  contains = "SimpleList",
  sealed = TRUE
)

#' @rdname hiveEntityClassList-class
#' @param listData
#' A list of \code{\linkS4class{hiveEntityClass}} objects
#' @details
#' This constructor function does not require that the \code{listData} argument
#' be named.

hiveEntityClassList <- function (listData=list())
{
  # Check arguments for errors
  if (!is.list(listData)) stop("Argument 'listData' must be a list")

  # Return a list named by the Entity classes it contains (if any)
  new("hiveEntityClassList", listData=listData)
  if (length(listData)) {
    new(
      "hiveEntityClassList",
      listData=setNames(listData, sapply(listData, objectId))
    )
  } else {
    new("hiveEntityClassList", listData=listData)
  }
}

########## Entity-related classes ##########

#' @export hiveEntity
#' @exportClass hiveEntity
#' @rdname hiveEntity-class
#' @title Class to contain Entity records
#' @description
#' This class is the S4 representation of the hive Entity record type.
#' It is a container for structured metadata and references to other GeneHive
#' records.
#' @slot .class
#' A character string specifying the Entity class of the record.
#' @slot .class_name
#' A character string specifying the Entity class of the record; appears to be a
#' duplicate field that should be deprecated soon. Not currently used.
#' @slot .entity_id
#' A \code{\linkS4class{UUID}} specifying the unique identifier of the record.
#' See Details.
#' @slot .creator
#' A character string specifying the \code{username} of the User that
#' created the record. Defaults to \code{""}.
#' @slot .owner
#' A character string specifying the \code{username} of the User that
#' owns the record. Defaults to \code{""}.
#' @slot .group
#' A character string specifying the \code{name} of the Group associated
#' with the record. Defaults to \code{""}.
#' @slot .creation_date
#' A character string specifying the time and date at which the record was
#' created. Defaults to \code{""}.
#' @slot .updated
#' A character string specifying the time and date at which the record was last
#' updated. Defaults to \code{""}.
#' @slot .permissions
#' A \code{\linkS4class{hivePermissions}} object specifying the permissions
#' associated with the record. Defaults to read-only.
#' @slot .workfiles
#' An integer vector containing the IDs of any WorkFiles associated with the
#' record; used internally by the server-side app.
#' Defaults to an empty integer vector.
#' @section Details:
#' \code{hiveEntity} slots whose names begin with \code{.} are treated
#' specially.  (The corresponding Entity fields are denoted with a
#' leading \code{_} character.)
#'
#' The \code{.entity_id} slot is computed in one of two ways:
#' \enumerate{
#'   \item{
#'     If 'key' fields have been defined for the Entity class, the hash of the
#'     Entity class name (e.g., \code{"FeatureSet"}) is combined with a hash
#'     computed from the contents of the key fields (e.g., \code{features}).
#'   }
#'   \item{
#'     If no 'key' fields have been defined for the Entity class, a random
#'     (version 4) UUID is assigned.
#'   }
#' }
#' This process ensures that Entity record identifiers are unique, both within
#' and across Entity classes.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveEntity <- setClass(
  "hiveEntity",
  slots = c(
    .class           = "character",
    .class_name      = "character",
    .entity_id       = "UUID",
    .creator         = "character",
    .owner           = "character",
    .group           = "character",
    .creation_date   = "character",
    .updated         = "character",
    .permissions     = "hivePermissions",
    .workfiles       = "integer"
  ),
  prototype = prototype(
    .class           = "Entity",
    .class_name      = "Entity",
    .entity_id       = new("UUID"),
    .creator         = "",
    .owner           = "",
    .group           = "",
    .creation_date   = "",
    .updated         = "",
    .permissions     = new("hivePermissions"),
    .workfiles       = integer(0)
  ),
  contains = "VIRTUAL"
)
setIs("hiveEntity", "S4")
sealClass("hiveEntity", where=.GlobalEnv)

#' @export hiveEntityList
#' @rdname hiveEntityList-class
#' @title Class to contain a list of Entity objects
#' @description
#' This class is a container for one or more Entity objects.
#' @section Extends:
#' This class directly extends class \code{\linkS4class{SimpleList}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

# Container for a list of hiveEntity objects
setClass(
  "hiveEntityList",
  prototype = new("SimpleList", elementType="hiveEntity"),
  validity = function (object)
  {
    error.messages <- list(
      too.many.classes =
        "A hiveEntityList may only contain objects of a single class",
      not.entities =
        "A hiveEntityList must contain objects that extend class 'hiveEntity'"
    )
    if (length(object@elementType) > 1) {
      error.messages$too.many.classes
    } else if (!extends(object@elementType, "hiveEntity")) {
      error.messages$not.entities
    } else if (length(unique(unlist(lapply(object@listData, class)))) > 1) {
      error.messages$too.many.classes
    } else {
      TRUE
    }
  },
  contains = "SimpleList",
  sealed = TRUE
)

#' @rdname hiveEntityList-class
#' @param listData
#' A list of objects of or extending class \code{\linkS4class{hiveEntity}}
#' @details
#' This constructor function does not require that the \code{listData} argument
#' be named.

hiveEntityList <- function (listData=list())
{
  # Check arguments for errors
  if (!is.list(listData)) stop("Argument 'listData' must be a list")

  # Return a list named by the IDs of the Entities it contains (if any)
  if (length(listData)) {
    new(
      "hiveEntityList",
      listData=setNames(
        listData, sapply(lapply(listData, objectId), as.character)
      ),
      elementType=unique(sapply(listData, class))
    )
  } else {
    new("hiveEntityList", listData=listData)
  }
}

#' @export hiveFeatureSpaceEntity
#' @exportClass hiveFeatureSpaceEntity
#' @rdname hiveFeatureSpaceEntity-class
#' @title Class to describe a feature space
#' @description
#' This class is the S4 representation of the Entity class FeatureSpace.
#' It is a container for \emph{either} a set of features or a regular expression
#' defining a feature space (e.g., Entrez Gene IDs, Affymetrix probeset IDs for
#' a specific microarray platform, etc.).
#' @slot .entity_id
#' A \code{\linkS4class{UUID}} automatically computed from the contents of the
#' record.
#' @section Extends:
#' Directly extends class \code{\linkS4class{hiveEntity}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveFeatureSpaceEntity <- setClass(
  "hiveFeatureSpaceEntity", contains="hiveEntity"
)

#' @export hiveFeatureSetEntity
#' @exportClass hiveFeatureSetEntity
#' @rdname hiveFeatureSetEntity-class
#' @title Class to describe a set of features
#' @description
#' This class is the S4 representation of the Entity class FeatureSet.
#' It is a container for an unweighted set of features (e.g., a gene set).
#' @slot .entity_id
#' A \code{\linkS4class{UUID}} automatically computed from the features
#' when adding a new FeatureSet record.
#' @section Extends:
#' Directly extends classes \code{\linkS4class{hiveEntity}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveFeatureSetEntity <- setClass(
  "hiveFeatureSetEntity", contains="hiveEntity"
)

#' @export hiveWeightedFeatureSetEntity
#' @exportClass hiveWeightedFeatureSetEntity
#' @rdname hiveWeightedFeatureSetEntity-class
#' @title Class to describe a set of features and weights
#' @description
#' This class is the S4 representation of the Entity class
#' WeightedFeatureSet.  It is a container for a set of features and their
#' associated weights (e.g., a ranked list or a biomarker).
#' @slot .entity_id
#' A \code{\linkS4class{UUID}} automatically computed from the features and
#' weights when adding a new WeightedFeatureSet record.
#' @section Extends:
#' Directly extends class \code{\linkS4class{hiveEntity}}.
#' @seealso
#' The \code{\linkS4class{hiveFeatureSetEntity}} class is used to contain a set
#' of unweighted features.
#' @author Adam C. Gower \email{agower@@bu.edu}

hiveWeightedFeatureSetEntity <- setClass(
  "hiveWeightedFeatureSetEntity", contains="hiveEntity"
)

hiveEntityAnnotationEntity <- setClass(
  "hiveEntityAnnotationEntity", contains="hiveEntity"
)
hiveMirrorEntity <- setClass(
  "hiveMirrorEntity", contains="hiveEntity"
)
hivePackageEntity <- setClass(
  "hivePackageEntity", contains="hiveEntity"
)
hivePlatformEntity <- setClass(
  "hivePlatformEntity", contains="hiveEntity"
)
hivePublicationEntity <- setClass(
  "hivePublicationEntity", contains="hiveEntity"
)
hiveReferenceEntity <- setClass(
  "hiveReferenceEntity", contains="hiveEntity"
)
hiveStudyEntity <- setClass(
  "hiveStudyEntity", contains="hiveEntity"
)
