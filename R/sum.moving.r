#' Moving sum based on index values
#'
#' @param dat Data
#' @param win Window size (index values)
#'
#' @export sum.moving
sum.moving <- function(dat,win=5) {
  return(as.numeric(filter(dat,rep(1,win), sides=2)))
}
