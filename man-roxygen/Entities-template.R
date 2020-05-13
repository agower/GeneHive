#' @rdname <%= plural %>
#' @name <%= plural %>
#' @title Add, delete, retrieve, update, or list <%= singular %> record(s)
#' @description
#' These functions attempt to add, delete, or retrieve a
#' \code{<%= singular %>}
#' record, or to list
#' \code{<%= singular %>}
#' records.
#' @param .entity_id
#' A character string specifying the unique identifier of a
#' \code{<%= singular %>}
#' record to be deleted, retrieved, or updated.
#' This field is <%= UUIDComputationMethod %> when adding a new
#' \code{<%= singular %>}
#' record.
#' @param \dots
#' Additional arguments specifying fields of the
#' \code{<%= singular %>}
#' record to be added or updated, or fields on which to limit a listing
#' @param .permissions
#' A \code{\linkS4class{hivePermissions}} object
#' specifying the permissions to be used when creating the record
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' \describe{
#'   \item{
#'     \code{add<%= singular %>},
#'     \code{get<%= singular %>},
#'     \code{update<%= singular %>}
#'   }{
#'     If the operation was successful, a
#'     \code{\linkS4class{hive<%= singular %>}}
#'     object is invisibly returned.
#'   }
#'   \item{\code{delete<%= singular %>}}{
#'     A logical value is returned stating whether the operation was successful.
#'   }
#'   \item{\code{list<%= plural %>}}{
#'     A data frame containing one row per record and one column per field.
#'   }
#'   \item{All functions}{
#'     If an error is encountered, the function terminates with a message.
#'   }
#' }
#' @author Adam C. Gower \email{agower@@bu.edu}
