#' Converts FALSE logicals to NA.
#'
#' @param expr Logical expression
#'
#' @export f2na
f2na <- function(expr){
  expr[!expr]<-NA
  return(expr)
}
