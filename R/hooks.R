.onLoad <- function (libname, pkgname)
{
  # Set global options used by functions in the GeneHive package
  options(
    GeneHive.api.base.path = "hive/v2",
    GeneHive.hashing.algorithm = "md5",
    GeneHive.hostname = Sys.getenv("HIVE_HOSTNAME"),
    GeneHive.https = as.logical(toupper(Sys.getenv("HIVE_HTTPS"))),
    # Note: normalizePath() is necessary in Windows environments
    # Note: suppressWarnings() is used in case .netrc file does not exist;
    #       still want to keep this as default location for creating a new one
    GeneHive.netrc.file = suppressWarnings(
      normalizePath(
        ifelse(Sys.info()["sysname"] == "Windows", "~/_netrc", "~/.netrc")
      )
    ),
    GeneHive.permissions = new(
      "hivePermissions", group=c("R"), other=character(0)
    ),
    GeneHive.proxy = FALSE,
    GeneHive.username = Sys.getenv("HIVE_USERNAME"),
    GeneHive.verbose = TRUE
  )
  # If any environment variables were not set (or logical variables were not set
  # properly), revert to default settings
  if (options("GeneHive.hostname") == "") {
    options(GeneHive.hostname = "localhost")
  }
  if (is.na(options("GeneHive.https"))) {
    options(GeneHive.https = TRUE)
  }
  if (options("GeneHive.username") == "") {
    options(GeneHive.username = unname(Sys.info()["user"]))
  }
  invisible()
}

.onAttach <- function (libname, pkgname)
{
  packageStartupMessage(
    "\n",
    "=========================================================================",
    "\n",
    "Welcome to GeneHive.\n",
    "========================================================================="
  )
}
