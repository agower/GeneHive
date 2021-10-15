#' @rdname hiveSlots
#' @name Get or set S4 object slots
#' @title Get or set various slots of a S4 objects
#' @description
#' These are convenience functions for getting and setting various slots of
#' an S4 representation of a record.
#' @param object
#' An R object
#' @param value
#' The value that the slot should take
#' @return
#' \describe{
#'   \item{
#'     objectCreationDate, objectCreator, objectDescription, objectGroup,
#'     objectName, objectOwner
#'   }{
#'     A character vector of length 1.
#'   }
#'   \item{objectId}{
#'     The unique identifier is returned; for a
#'     \code{\linkS4class{hiveEntity}}, this will be a
#'     \code{\linkS4class{UUID}} object; for a
#'     \code{\linkS4class{hiveWorkFileProperties}} object, this will be a
#'     \code{\linkS4class{hiveWorkFileID}} object; and for
#'     \code{\linkS4class{hiveEntityClass}}, \code{\linkS4class{hiveGroup}} or
#'     \code{\linkS4class{hiveUser}} objects, this will be a character string.
#'   }
#'   \item{objectPermissions}{
#'     A \code{linkS4class{hivePermissions}} object.
#'   }
#' }
#' @seealso
#' The \code{\link{hiveSlotName}} function returns the name of the slot that
#' corresponds to these fields, rather than the field itself.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
objectCreationDate <- function (object) {
  slot(object, hiveSlotName(class(object), "creationDate"))
}
#' @export
#' @rdname hiveSlots
objectCreator <- function (object) {
  slot(object, hiveSlotName(class(object), "creator"))
}
#' @export
#' @rdname hiveSlots
objectDescription <- function (object) {
  slot(object, hiveSlotName(class(object), "description"))
}
#' @export
#' @rdname hiveSlots
objectGroup <- function (object) {
  slot(object, hiveSlotName(class(object), "group"))
}
#' @export
#' @rdname hiveSlots
objectId <- function (object) {
  slot(object, hiveSlotName(class(object), "id"))
}
#' @export
#' @rdname hiveSlots
objectLastUpdated <- function (object) {
  slot(object, hiveSlotName(class(object), "updated"))
}
#' @export
#' @rdname hiveSlots
objectName <- function (object) {
  slot(object, hiveSlotName(class(object), "name"))
}
#' @export
#' @rdname hiveSlots
objectOwner <- function (object) {
  slot(object, hiveSlotName(class(object), "owner"))
}
#' @export
#' @rdname hiveSlots
objectPermissions <- function (object) {
  slot(object, hiveSlotName(class(object), "permissions"))
}

#' @export
#' @rdname hiveSlots
`objectDescription<-` <- function (object, value){
  `slot<-`(object, name=hiveSlotName(class(object), "description"), value=value)
}
#' @export
#' @rdname hiveSlots
`objectGroup<-` <- function (object, value){
  `slot<-`(object, name=hiveSlotName(class(object), "group"), value=value)
}
#' @export
#' @rdname hiveSlots
`objectId<-` <- function (object, value){
  `slot<-`(object, name=hiveSlotName(class(object), "id"), value=value)
}
#' @export
#' @rdname hiveSlots
`objectName<-` <- function (object, value){
  `slot<-`(object, name=hiveSlotName(class(object), "name"), value=value)
}
#' @export
#' @rdname hiveSlots
`objectOwner<-` <- function (object, value){
  `slot<-`(object, name=hiveSlotName(class(object), "owner"), value=value)
}
#' @export
#' @rdname hiveSlots
`objectPermissions<-` <- function (object, value){
  `slot<-`(object, name=hiveSlotName(class(object), "permissions"), value=value)
}
