#' @import methods uuidtools

#' @rdname listToHiveS4
#' @name List to hive S4 object
#' @title Convert a list of fields to a hive S4 object
#' @description
#' This function creates an S4 object from a list of fields that correspond to
#' slots of a hive S4 object, performing a recursive validity check.
#' @param Class
#' A character string representing an S4 object class
#' @param x
#' A list containing fields that will be coerced to slots of the S4 object
#' @return
#' An object of the class specified in \code{Class}.
#' @author Adam C. Gower \email{agower@@bu.edu}

listToHiveS4 <- function (Class, x)
{
  slots <- getSlots(Class)
  for (slotName in names(x)) {
    toClass <- slots[slotName]
    if (!is(x[[slotName]], toClass)) {
      coerceMethodExists <- hasMethod(
        "coerce", signature(from=class(x[[slotName]]), to=toClass)
      )
      if (coerceMethodExists) {
        x[[slotName]] <- as(x[[slotName]], toClass)
      } else if (toClass == "UUID" && is.character(x[[slotName]])) {
        x[[slotName]] <- UUIDparse(x[[slotName]])[[1]]
      } else if (toClass == "UUIDList" && is.character(x[[slotName]])) {
        x[[slotName]] <- UUIDparse(x[[slotName]])
      } else if (toClass == "UUIDList" && is.list(x[[slotName]])) {
        x[[slotName]] <- UUIDList(x[[slotName]])
      } else if (toClass == "factor") {
        # There is no S4 method for coerce() from 'character' to 'factor'
        x[[slotName]] <- as.factor(x[[slotName]])
      } else {
        stop(
          "Argument ", sQuote(slotName),
          " cannot be coerced to class ", sQuote(toClass)
        )
      }
    }
  }
  # Create an S4 object from the arguments, performing a recursive check
  # to ensure that all of the data types, etc., are correct
  # Note: for hiveEntity subclasses, this step will also automatically compute
  # the ID of the object via initialize()
  validObject(result <- do.call(new, c(Class=Class, x)), complete=TRUE)
  result
}
