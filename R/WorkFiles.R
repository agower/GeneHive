#' @import digest
#' @importFrom tools file_ext
#' @importFrom R.utils gunzip isGzipped

#' @rdname WorkFiles
#' @name WorkFiles
#' @title
#' Functions for working with WorkFiles
#' @description
#' These functions attempt to add or retrieve a single \code{WorkFile}, to list
#' \code{WorkFile} records, to add a set of \code{WorkFile}s (specified via a
#' set of filenames or in an archive), to pass data between R objects and
#' \code{WorkFile}s, or to test whether a character string represents a valid
#' \code{WorkFile} identifier.
#' @param id
#' A \code{\linkS4class{hiveWorkFileID}} object (or coercible to one)
#' specifying the unique identifier of a \code{WorkFileProperties} record.
#' Automatically populated when a \code{WorkFile} is uploaded.
#' @param filename
#' A character string specifying the full path to a file; for
#' \code{importWorkFileArchive}, this must be a .tar, .tgz, .tar.gz or .zip file
#' @param filenames
#' A character vector specifying the full path to one or more files
#' @param x
#' An R object to be uploaded as a \code{WorkFile}
#' @param fileType
#' A character string specifying the file type (e.g., extension) of the 
#' \code{WorkFile}
#' @param compress
#' A logical value specifying whether to gzip-compress the \code{WorkFile} on
#' upload; if the contents of the \code{WorkFile} are already gzipped, this is
#' ignored
#' @param group
#' A character string specifying the name of the \code{Group} associated with
#' the \code{WorkFile}. If missing, the group belonging to the user uploading
#' the \code{WorkFile} is automatically used by the system.
#' @param storage
#' A character string specifying the location in which the WorkFile is stored.
#' @param token
#' A character string specifying a token that can be used for password-free 
#' authentication. Automatically populated when a \code{WorkFile} is uploaded.
#' @param \dots
#' Additional arguments specifying \code{WorkFileProperties} fields on which to 
#' limit a listing
#' @param permissions
#' A \code{\linkS4class{hivePermissions}} object specifying the permissions to 
#' be used when creating the record
#' @param con
#' A \code{\linkS4class{hiveConnection}} object; if not provided, a new
#' connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' \describe{
#'   \item{\code{addWorkFile}}{
#'     If a \code{WorkFile} exists with the same hash as the input file, it is
#'     assumed that they are identical; the existing record will be returned
#'     invisibly as a \code{linkS4class{hiveWorkFileProperties}} object, and a
#'     warning message will be issued. If a \code{WorkFile} does not exist with
#'     the same MD5 hash as the input file, the file is uploaded and the
#'     resulting \code{WorkFile} is returned invisibly as a
#'     \code{linkS4class{hiveWorkFileProperties}} object.
#'   }
#'   \item{\code{getWorkFile}}{
#'     No value is returned.
#'   }
#'   \item{\code{importWorkFiles, importWorkFileArchive}}{
#'     If the operation is successful, a
#'     \code{\linkS4class{hiveWorkFileIDList}} object is returned invisibly;
#'     otherwise, the function terminates with an error message.
#'   }
#'   \item{\code{storeObjectAsWorkFile}}{
#'     If the operation is successful, a
#'     \code{\linkS4class{hiveWorkFileProperties}} object is invisibly returned.
#'   }
#'   \item{\code{getWorkFileAsObject}}{
#'     If the operation is successful, an R object is invisibly returned:
#'     either the R object encoded in the file (if the WorkFile is an
#'     \link[=serialize]{RDS} file) or the contents of the WorkFile (as
#'     a raw vector).
#'   }
#'   \item{\code{listWorkFiles}}{
#'     A data frame containing one row per record and one column per field.
#'   }
#'   \item{\code{validWorkFileId}}{
#'     A logical value specifying whether the string corresponds to a
#'     WorkFile identifier.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
addWorkFile <- function (
  filename,
  fileType = file_ext(sub("\\.gz$", "", filename)), compress = TRUE,
  group, storage, token, permissions = getOption("GeneHive.permissions"),
  con = hiveConnection(), verbose = getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(filename)) stop("Argument 'filename' is required")
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character vector of length 1")
  }
  if (!(is.character(fileType) && length(fileType) == 1)) {
    stop("Argument 'fileType' must be a character vector of length 1")
  }
  if (!(is.logical(compress) && length(compress) == 1)) {
    stop("Argument 'compress' must be a logical vector of length 1")
  }
  if (!is(permissions, "hivePermissions")) {
    stop("Argument 'permissions' must be a hivePermissions object")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  if (!missing(group)) {
    if (!(is.character(group) && length(group) == 1)) {
      stop("Argument 'group' must be a character vector of length 1")
    }
    if (!is.element(group, listGroups(con)$name)) {
      stop(paste(sQuote(group), "is not a valid Group"))
    }
  }
  if (!missing(storage)) {
    if (!(is.character(storage) && length(storage) == 1)) {
      stop("Argument 'storage' must be a character vector of length 1")
    }
  }
  if (!missing(token)) {
    if (!(is.character(token) && length(token) == 1)) {
      stop("Argument 'token' must be a character vector of length 1")
    }
    if (!grepl("^[0-9A-Fa-f]{64}$", token)) {
      stop(paste(sQuote(token), "is not a 256-bit hexadecimal string"))
    }
  }

  # Error function for file-reading operations
  readError <- function(x) {
    cat(sprintf("Could not read file '%s'.\n", filename), as.character(x))
  }

  if (!file.exists(filename)) {
    stop(paste("File", sQuote(filename), "does not exist"))
  }

  if (file_ext(filename) %in% c(".tar",".tar.gz",".tgz",".zip")) {
    warning(
      paste(
        "File", sQuote(filename), "contains an archive;",
        "consider using importWorkFileArchive() instead."
      )
    )
  }

  # Create a temporary file
  temp_filename <- tempfile()
  # Read the file into a raw vector
  buf <- tryCatch(
    readBin(con=filename, what="raw", n=file.info(filename)$size),
    error=readError
  )
  if (isGzipped(filename, method="content") && file_ext(filename) == "gz") {
    # Two identical files that are independently gzipped may result in
    # non-identical .gz files, depending on the compression level and whether
    # the file header includes the original filename or timestamp.
    # For this reason, to avoid the storage of functionally duplicate WorkFiles,
    # the hashes of .gz files are computed from their decompressed contents.
    #
    # This procedure is not applied to files with extensions other than .gz,
    # even though they may be of formats that support native gzip compression
    # (e.g., RDS); this is because such files are typically gzipped with a
    # consistent compression level and do not contain a gzip file header,
    # meaning that the same data is likely to produce the same compressed file
    # regardless of its origin. 
    # 
    # Note: gunzip() is used to decompress the file, even though it has
    #       already been read into memory, because:
    #       - memDecompress() does not work with standard gzip files
    #       - digest() cannot be used with connections,
    #         i.e., digest(gzcon(rawConnection(buf)))
    #       - readBin(gzcon(rawConnection(buf))) requires knowledge of total
    #         size of uncompressed data, which is not easily computed
    #         from gzip files (which only store the size modulo 2^32)
    gunzip(filename, destname=temp_filename, remove=FALSE)
    originalName <- digest(
      temp_filename, algo=getOption("GeneHive.hashing.algorithm"),
      serialize=FALSE, file=TRUE
    )
    if (compress) {
      warning(
        "File ", sQuote(filename), " is already compressed; ",
        "ignoring compress = TRUE"
      )
    }
    # Compute file extension
    fileType <- ifelse(fileType == "", "gz", paste0(fileType, ".gz"))
  } else {
    if (file_ext(filename) == "gz") {
      warning("File ", sQuote(filename), " is not a gzip file")
    }
    originalName <- digest(
      buf, algo=getOption("GeneHive.hashing.algorithm"),
      serialize=FALSE, file=FALSE
    )
    if (compress) {
      # Compress the buffer to a tempfile and read it back into the buffer
      # Note:
      # - memCompress() does not produce standard gzip file and is unused
      # - gzcon(rawConnection(raw(0),open="w")) does not work as advertised in
      #   gzcon() man page to compress directly to a raw vector
      tempfile_con <- gzfile(temp_filename, open="wb")
      writeBin(buf, tempfile_con)
      close(tempfile_con)
      buf <- readBin(
        temp_filename, what="raw", n=file.info(temp_filename)$size
      )
      # Compute file extension
      fileType <- ifelse(fileType == "", "gz", paste0(fileType, ".gz"))
    }
  }
  # Clean up
  unlink(temp_filename)

  # Check whether a WorkFile record with the computed hash exists
  workFile_id <- hiveWorkFileID(
    listWorkFiles(originalName=originalName, con=con)$id
  )

  if (length(workFile_id) == 1) {
    warning(
      "WorkFile ", sQuote(workFile_id), " already exists with original hash ",
      originalName
    )
    result <- getWorkFileProperties(workFile_id)
  } else {
    if (verbose) cat(
      sprintf("Uploading contents of file '%s'.\n", basename(filename))
    )

    # Submit a POST request and stop if an error is returned
    query <- list(originalName=originalName, fileType=fileType)
    if (!missing(group)) query$group <- group
    if (!missing(storage)) query$storage <- storage
    if (!missing(token)) query$token <- token
    response <- stopIfHiveError(
      httpRequest(
        url=hiveURL("WorkFiles", query=query), method="POST", content=buf,
        httpheader=c(
          "Accept"="application/json", 
          "Content-Type"="application/octet-stream"
        ),
        curl=con
      )
    )
    # Convert response to a WorkFileProperties object
    result <- hivePostprocess(response, "WorkFileProperties")
    # Silently add the specified permissions to the WorkFileProperties record
    # (This is a workaround because setting the permissions during the add
    # operation does not work)
    updateWorkFileProperties(
      objectId(result), permissions=permissions, verbose=FALSE
    )
    if (verbose) {
      cat(
        sprintf(
          "File '%s' was successfully uploaded to WorkFile '%s'.\n",
          filename, objectId(result)
        )
      )
    }
  }
  # Return the WorkFileProperties object invisibly
  invisible(result)
}   

#' @export
#' @rdname WorkFiles
getWorkFile <- function (
  id, filename, con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(id) || missing(filename)) {
    stop("Arguments 'id' and 'filename' are required")
  }
  if (!is(id, "hiveWorkFileID")) {
    id <- try(as(id, "hiveWorkFileID"), silent=TRUE)
    if (inherits(id, "try-error")) {
      stop("Argument 'id' must be a hiveWorkFileID object or coercible to one")
    }
  }
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character vector of length 1")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Try retrieving the WorkFile, and exit with an error message if unsuccessful
  if (verbose) {
    cat(
      sprintf("Downloading WorkFile '%s' to local file '%s'.\n", id, filename)
    )
  }
  response <- try(
    stopIfHiveError(
      httpRequest(
        url=hiveURL("WorkFileContents", id), method="GET",
        httpheader=c("Accept"="application/octet-stream"), curl=con
      )
    ),
    silent=TRUE
  )
  if (inherits(response, "try-error")) {
    stop(paste("WorkFile", sQuote(id), "does not exist"))
  }

  # Try to write to a file
  # Note: as.raw() is used to drop all attributes of 'response'
  # (an error is thrown otherwise)
  write_res <- try(writeBin(object=as.raw(response), con=filename), silent=TRUE)
  # Report back with error/success status
  if (inherits(write_res, "try-error")) {
    stop(paste("Could not write to file", sQuote(filename)))
  } else {
    if (verbose) {
      cat(
        sprintf(
          "WorkFile '%s' was successfully downloaded to file '%s'.\n",
          id, filename
        )
      )
    }
  }
}

#' @export
#' @rdname WorkFiles
getWorkFileAsObject <- function (
  id, con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(id)) stop("Argument 'id' is required")
  if (!is(id, "hiveWorkFileID")) {
    id <- try(as(id, "hiveWorkFileID"), silent=TRUE)
    if (inherits(id, "try-error")) {
      stop("Argument 'id' must be a hiveWorkFileID object or coercible to one")
    }
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Download a WorkFile to a temporary file
  temp_filename <- tempfile()
  on.exit(unlink(temp_filename, force=TRUE))
  if (verbose) cat(sprintf("Retrieving WorkFile '%s'.\n", id))
  getWorkFile(id, filename=temp_filename, con=con, verbose=FALSE)
  # If the file is in RDS format, read it into an R object;
  # otherwise, read it into a raw vector
  result <- try(readRDS(temp_filename), silent=TRUE)
  if (inherits(result, "try-error")) {
    result <- readBin(
      temp_filename, what="raw", n=file.info(temp_filename)$size
    )
  }
  # Return the R object invisibly
  invisible(result)
}

#' @export
#' @rdname WorkFiles
importWorkFiles <- function (
  filenames, ...,
  permissions=getOption("GeneHive.permissions"),
  con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(filenames)) stop("Argument 'filenames' is required")
  if (!(is.character(filename) && length(filename) > 0)) {
    stop("Argument 'filename' must be a character vector of nonzero length")
  }
  if (!is(permissions, "hivePermissions")) {
    stop("Argument 'permissions' must be a hivePermissions object")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  if (verbose) cat("Processing file(s):", sQuote(filenames), sep="\n")

  # Determine whether each file is present, and if not, add it
  result <- list()
  for (filename in filenames) {
    workFile_record <- addWorkFile(filename, con=con, verbose=FALSE)
    # Note: list() is required to keep the WorkFile ID from being coerced
    #       to a character vector
    result <- c(result, list(objectId(workFile_record)))
  }
  # Convert result to a hiveWorkFileID list and return it invisibly
  result <- hiveWorkFileIDList(result)
  names(result@listData) <- basename(filenames)
  invisible(result)
}

#' @export
#' @rdname WorkFiles
importWorkFileArchive <- function (
  filename, ...,
  permissions=getOption("GeneHive.permissions"),
  con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(filename)) stop("Argument 'filename' is required")
  if (!(is.character(filename) && length(filename) == 1)) {
    stop("Argument 'filename' must be a character vector of length 1")
  }
  if (!is(permissions, "hivePermissions")) {
    stop("Argument 'permissions' must be a hivePermissions object")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  if (!file.exists(filename)) {
    stop(paste("File", sQuote(filename), "does not exist"))
  }

  # Extract files to a temporary directory
  temp_dir <- file.path(tempdir(), digest(filename))
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive=TRUE, force=TRUE))

  if (file_ext(filename) == "zip") {
    system(sprintf("unzip '%s' -d %s", filename, temp_dir))
  } else if (grepl("\\.(tar\\.|t)gz$", filename)) {
    system(sprintf("tar xzvf '%s' --directory=%s", filename, temp_dir))
  } else if (file_ext(filename) == "tar") {
    system(sprintf("tar xvf '%s' --directory=%s", filename, temp_dir))
  } else {
    stop(paste("File", sQuote(filename), "is not an archive"))
  }

  # Attempt to upload import each file to a Sample; if unsuccessful, skip it with a warning message
  temp_filenames <- list.files(temp_dir, full.names=TRUE, recursive=TRUE)
  result <- importWorkFiles(temp_filenames)
  names(result@listData) <- basename(temp_filenames)

  # Return the result invisibly
  invisible(result)
}

#' @export
#' @rdname WorkFiles
storeObjectAsWorkFile <- function (
  x,
  permissions=getOption("GeneHive.permissions"),
  con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(x)) stop("Argument 'x' is required")
  if (!is(permissions, "hivePermissions")) {
    stop("Argument 'permissions' must be a hivePermissions object")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Save the R object to a temporary RDS file
  temp_filename <- tempfile()
  on.exit(unlink(temp_filename, force=TRUE))
  saveRDS(x, file=temp_filename)
  # Try to add the temporary RDS file as a WorkFile
  result <- suppressWarnings(
    addWorkFile(
      temp_filename, fileType="rds",
      con=con, permissions=permissions, verbose=FALSE
    )
  )
  if (verbose) {
    cat(sprintf("Object stored in WorkFile '%s'.\n", objectId(result)))
  }
  # Return the WorkFileProperties object invisibly
  invisible(result)
}

#' @export
#' @rdname WorkFiles
validWorkFileId <- function (id, con=hiveConnection())
{
  # Check arguments for errors
  if (missing(id)) stop("Argument 'id' is required")
  if (!is(id, "hiveWorkFileID")) {
    id <- try(as(id, "hiveWorkFileID"), silent=TRUE)
    if (inherits(id, "try-error")) {
      stop("Argument 'id' must be a hiveWorkFileID object or coercible to one")
    }
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }

  # Try to get the WorkFile and return whether an error occurred
  workFile <- try(getWorkFileProperties(id=id, con), silent=TRUE)
  !inherits(workFile, "try-error")
}
