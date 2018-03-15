#' Moving average based on index values
#'
#' @param dat Data
#' @param win Window size (index values)
#'
#' @export
avg.moving <- function(dat,win=10){
  return(as.numeric(filter(dat,rep(1/win,win), sides=2)))
}
