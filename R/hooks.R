#' @importFrom utils packageVersion

.onLoad <- function (libname, pkgname)
{
  # Set global options used by functions in the GeneHive package
  options(
    GeneHive.api.base.path = Sys.getenv("HIVE_API_BASE_PATH"),
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
    GeneHive.port = as.integer(Sys.getenv("HIVE_PORT")),
    GeneHive.proxy = FALSE,
    GeneHive.timezone = Sys.getenv("HIVE_TIMEZONE"),
    GeneHive.username = Sys.getenv("HIVE_USERNAME"),
    GeneHive.verbose = TRUE
  )
  # If any environment variables were not set (or logical variables were not set
  # properly), revert to default settings
  if (getOption("GeneHive.api.base.path") == "") {
    options(GeneHive.api.base.path = "api/v2")
  }
  if (getOption("GeneHive.hostname") == "") {
    options(GeneHive.hostname = "localhost")
  }
  if (is.na(getOption("GeneHive.https"))) {
    options(GeneHive.https = TRUE)
  }
  if (is.na(getOption("GeneHive.port"))) {
    options(GeneHive.port = NULL)
  }
  if (getOption("GeneHive.timezone") == "") {
    options(GeneHive.timezone = "UTC")
  }
  if (getOption("GeneHive.username") == "") {
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
    paste0(
      "Welcome to ", pkgname, " (version ", packageVersion(pkgname), ").\n"
    ),
    "========================================================================="
  )
}
