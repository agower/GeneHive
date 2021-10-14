#' @useDynLib GeneHive
#' @import uuidtools

#' @rdname bitwiseParity
#' @name bitwiseParity
#' @title Compute bitwise parity over a vector or list
#' @description
#' This function computes the bitwise parity of a set of integer, logical, or
#' raw values, or a list of such vectors.  It is a convenient way to collapse a
#' set of \code{\linkS4class{UUID}}s into a summary value, regardless of the
#' order of the \code{UUID}s.
#' @param x
#' An integer, logical or raw vector or array, or a character vector or array
#' containing hexadecimal values (leading \code{"0x"} optional)
#' @param even
#' A logical value specifying whether the parity should be even or odd
#' @return
#' \itemize{
#'   \item{
#'     If \code{x} is of length zero, a vector of length zero of the same
#'     storage mode as \code{x}.
#'   }
#'   \item{
#'     If \code{x} is a character vector consisting solely of empty strings,
#'     \code{""}.
#'   }
#'   \item{
#'     If \code{x} is a character vector containing invalid hexadecimal
#'     strings, \code{NA}.
#'   }
#'   \item{
#'     Otherwise, a vector of length 1 of the same storage mode as \code{x},
#'     containing the bitwise parity computed across all elements of \code{x}.
#'   }
#' }
#' @note
#' If the storage mode of \code{x} is \code{"character"}, the output will
#' be in hexadecimal notation with no leading \code{"0x"}.
#' @author Adam C. Gower \email{agower@@bu.edu}

setGeneric(
  "bitwiseParity",
  function (x, even=TRUE) standardGeneric("bitwiseParity")
)

#' @rdname bitwiseParity
setMethod(
  "bitwiseParity",
  signature(x="integer"),
  function (x, even=TRUE)
  {
    if (length(even) != 1) stop("Argument 'even' must be of length 1")
    .Call("bitwiseParity", x, even)
  }
)

#' @rdname bitwiseParity
setMethod(
  "bitwiseParity",
  signature(x="logical"),
  function (x, even=TRUE)
  {
    if (length(even) != 1) stop("Argument 'even' must be of length 1")
    .Call("bitwiseParity", x, even)
  }
)

#' @rdname bitwiseParity
setMethod(
  "bitwiseParity",
  signature(x="raw"),
  function (x, even=TRUE)
  {
    if (length(even) != 1) stop("Argument 'even' must be of length 1")
    .Call("bitwiseParity", x, even)
  }
)

#' @rdname bitwiseParity
setMethod(
  "bitwiseParity",
  signature(x="UUIDList"),
  function (x, even=TRUE)
  {
    if (length(even) != 1) stop("Argument 'even' must be of length 1")
    UUID(.Call("bitwiseParity", lapply(x, as.raw), even))
  }
)

#' @rdname bitwiseParity
setMethod(
  "bitwiseParity",
  signature(x="list"),
  function (x, even=TRUE)
  {
    if (length(even) != 1) stop("Argument 'even' must be of length 1")
    n <- unique(sapply(x, length))
    storage.modes <- unique(sapply(x, storage.mode))
    if (length(n) > 1 || length(storage.modes) > 1) {
      stop(
        "All elements of list argument 'x' ",
        "must be of the same length and storage mode"
      )
    } else {
      .Call("bitwiseParity", x, even)
    }
  }
)
