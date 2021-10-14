#' @rdname Passwords
#' @name Passwords
#' @title Change, check, or reset passwords
#' @description
#' These functions allow Users to change their password or (for superusers)
#' other Users' passwords, to check a username/password combination, to
#' generate a random password, or (for superusers) to send a password reset
#' email to a User.
#' @param username
#' A character string containing the name of the User;
#' defaults to \code{getOption("GeneHive.username")}
#' @param random
#' A logical value specifying whether a random password should be generated;
#' defaults to \code{TRUE}. If available, \code{/dev/urandom} is used to
#' generate random passwords; otherwise, \code{\link{sample}} is used.
#' @param netrc.file
#' A character string specifying the location of the .netrc file
#' @param password
#' A character string containing the password that will be checked for a User
#' @param nchars
#' The length of the password to be generated, in characters
#' @param prefix
#' An optional character string containing the URL prefix of a landing page to
#' use in email template. This will have \code{"?user={user}&token={token}"}
#' appended in the actual template.
#' If this is absent, then the template for direct API use will be sent instead
#' of the template linking to a landing page.
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' \describe{
#'   \item{\code{changePassword}}{
#'     \code{TRUE} (invisibly) if the password was successfully
#'     changed; otherwise, the function terminates with an error.
#'   }
#'   \item{\code{checkPassword}}{
#'     \code{TRUE} if the username and password are valid,
#'     \code{FALSE} otherwise.
#'   }
#'   \item{\code{randomPassword}}{
#'     The password (invisibly).
#'   }
#'   \item{\code{resetPassword}}{
#'     The HTTP response body of the request (invisibly).
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
changePassword <- function (
  username = getOption("GeneHive.username"),
  random = TRUE, netrc.file = getOption("GeneHive.netrc.file"),
  verbose = getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character vector of length 1")
  }
  if (!(is.logical(random) && length(random) == 1)) {
    stop("Argument 'random' must be a logical vector of length 1")
  }
  if (!(is.character(netrc.file) && length(netrc.file) == 1)) {
    stop("Argument 'netrc.file' must be a character vector of length 1")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Convenience variables
  hostname <- getOption("GeneHive.hostname")
  windows <- Sys.info()["sysname"] == "Windows"

  # Convenience function to create entry in .netrc file
  netrc.entry <- function (machine, login, password) {
    paste("machine", machine, "login", login, "password", password)
  }

  # Create tempfile in user's home directory (most secure location)
  # Note: normalizePath() is necessary in Windows environments
  netrc.tempfile <- tempfile(tmpdir=normalizePath("~"))
  # Remove tempfile automatically if function terminates prematurely
  # (otherwise it will be renamed below and will not exist by end of function)
  on.exit(unlink(netrc.tempfile))

  # Establish a connection to the hive #########################################

  valid.password <- FALSE
  # If a .netrc file exists, try using that first to connect
  if (file.exists(netrc.file)) {
    con <- hiveConnection(username=username, netrc.file=netrc.file)
    # To check password, try to list Users (always returns at least one result)
    valid.password <- isJSON(RCurl::getURL(url=hiveURL("Users"), curl=con))
  }
  # If the .netrc file does not exist or does not contain valid password,
  # try prompting the user next, creating a temporary .netrc file
  if (!valid.password) {
    current.password <- readline(
      prompt=sprintf("Please enter current password for user '%s': ", username)
    )
    if (!windows) cat("\n")
    cat(netrc.entry(hostname, username, current.password), file=netrc.tempfile)
    con <- hiveConnection(username=username, netrc.file=netrc.tempfile)
    # To check password, try to list Users (always returns at least one result)
    valid.password <- isJSON(RCurl::getURL(url=hiveURL("Users"), curl=con))
  }
  # If user still cannot authenticate, terminate with an error
  if (!valid.password) {
    stop(
      "Cannot authenticate with current password for user ", sQuote(username)
    )
  }

  # Get new password and update User record ####################################

  # Generate or prompt for new password
  if (random) {
    new.password <- randomPassword()
  } else {
    # Prompt 2x for password and attempt to change password if the two match,
    # terminating with an error message if they do not
    new.password <- readline(
      prompt=sprintf("Please enter new password for user '%s': ", username)
    )
    if (!windows) cat("\n")
    new.password.repeat <- readline(
      prompt=sprintf("Please re-enter new password for user '%s': ", username)
    )
    if (!windows) cat("\n")
    if (new.password != new.password.repeat) {
      stop(sprintf("Passwords do not match"))
    }
  }

  # Update the User record in the hive
  updateUser(username=username, password=new.password, con=con, verbose=FALSE)
  if (verbose) {
    cat("Password for user", sQuote(username), "was successfully changed.\n")
  }

  # Create/update the netrc file ###############################################

  # Prepend a date-stamped comment line and new credentials to .netrc tempfile,
  # followed by current contents of existing .netrc file (if any)
  cat(
    paste("# Following line added automatically at:", Sys.time()),
    netrc.entry(hostname, username, new.password),
    if (file.exists(netrc.file)) readLines(netrc.file),
    file=netrc.tempfile, sep="\n"
  )
  # Attempt to overwrite any existing .netrc file with the tempfile
  # Note: this two-step process makes it less likely that the original .netrc
  #       file will be accidentally destroyed due to a system glitch
  success <- file.rename(netrc.tempfile, netrc.file)
  if (verbose && success) cat("File", netrc.file, "successfully updated.\n")

  # Return the success code, invisibly
  invisible(TRUE)
}

#' @export
#' @rdname Passwords
checkPassword <- function (
  username = getOption("GeneHive.username"), password
)
{
  # Check arguments for errors
  if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character vector of length 1")
  }
  if (missing(password)) stop("Argument 'password' is required")
  if (!(is.character(password) && length(password) == 1)) {
    stop("Argument 'password' must be a character vector of length 1")
  }

  # Submit a GET request to the hive and stop if an error is returned
  response <- stopIfHiveError(
    httpRequest(
      url=hiveURL(
        "CheckPassword", query=list(username=username, password=password)
      ),
      method="GET", curl=new("hiveConnection")
    )
  )
  # Convert the list to logical vector (only one element, 'correct', in result)
  # and return it
  unlist(response)
}

#' @export
#' @rdname Passwords
randomPassword <- function (nchars=20)
{
  # Check arguments for errors
  if (!(is.numeric(nchars) && length(nchars) == 1)) {
    stop("Argument 'nchars' must be a numeric vector of length 1")
  }

  # Select password using alphanumeric characters and symbols that are 'safe'
  # for use within an unencoded URL (as defined by RFC1738, see
  # http://www.ietf.org/rfc/rfc1738.txt); this avoids characters that may cause
  # problems with JSON and/or URL encoding
  allowed.chars <- c(
    0:9, LETTERS, letters, c("$","-","_",".","+","!","*","'","(",")",",")
  )

  if (file.exists("/dev/urandom")) {
    # If available, extract a random string of 8-bit integers from /dev/urandom
    # Note: file() is called with raw=TRUE because /dev/urandom is a device,
    #       not a regular file; otherwise, a warning is thrown
    i <- as.numeric(
      readBin(
        con=file("/dev/urandom", open="rb", raw=TRUE), what="raw", n=nchars
      )
    )
  } else {
    # Otherwise, use the system RNG
    i <- sample(0:255, size=nchars, replace=TRUE)
  }
  # Convert these to random characters
  random.indices <- round(length(allowed.chars) * i / 255) + 1
  # Paste the corresponding characters together to obtain a random password
  result <- paste(allowed.chars[random.indices], collapse="")
  # Return the password, invisibly
  invisible(result)
}

#' @export
#' @rdname Passwords
resetPassword <- function (
  username, prefix, verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(username)) stop("Argument 'username' is required")
  if (!(is.character(username) && length(username) == 1)) {
    stop("Argument 'username' must be a character vector of length 1")
  }
  if (!missing(prefix)) {
    if (!(is.character(prefix) && length(prefix) == 1)) {
      stop("Argument 'prefix' must be a character vector of length 1")
    }
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Create a list of parameters for constructing a URL query string
  parameters <- list(user=username)
  if (!missing(prefix)) parameters$prefix <- prefix

  # Submit a POST request to the hive and stop if an error is returned
  response <- stopIfHiveError(
    httpRequest(
      url=hiveURL("PasswordTokens"), method="POST", content=parameters,
      curl=new("hiveConnection")
    )
  )
  # Return the response invisibly
  if (verbose) {
    cat("A password reset email was sent to user", sQuote(username))
    cat(".\n")
  }
  invisible(response)
}
