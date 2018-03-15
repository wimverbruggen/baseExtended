#' Shift index of variable by given lag.
#'
#' @param var The variable of interest
#' @param lag The lag, either positive or negative.
#' @param rotate Add dropped values back at other side?
#'
#' @export
index.shift <- function(var, lag, rotate=FALSE) {
  n <- length(var)
  if(abs(lag)>=n) stop("Lag should be smaller than length of variable.")
  xnew <- rep(NA, n)
  if (lag < 0) {
    xnew[1:(n-abs(lag))] <- var[(abs(lag)+1):n]
    if(rotate) xnew[(n-abs(lag)+1):n] <- var[1:abs(lag)]
  } else if (lag > 0) {
    xnew[(lag+1):n] <- var[1:(n-lag)]
    if(rotate) xnew[1:lag] <- var[(n-lag+1):n]
  } else {
    xnew <- var
  }
  return(xnew)
}
