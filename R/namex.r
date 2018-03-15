# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#' Get the names of an object, so the same as names(), with the added possibility
#' to exclude certain names.
#'
#' @param obj object
#' @param excl list of names to be excluded
#' @return List of names in the object, excluding the ones mentioned
#' @author Wim Verbruggen
#' @export
namex <- function(obj,excl=""){
  n <- names(obj)
  n <- n[!n %in% excl]
  return(n)
}
