#' Mean absolute error
#'
#' @param obs the 'observed' data (can also be field data for example)
#' @param sim the 'simulated' data (can also be satellite data for example)
#' @param na_remove whether to ignore NA values
#' @return Mean absolute error
#' @export error.mae
error.mae <- function(obs,sim,normalize="absmean",na_remove=TRUE){
  err <- mean(abs(obs-sim),na.rm = na_remove)
  normfact <- 1
  if(normalize=="mean")      normfact <- mean(obs,na.rm = na_remove)
  if(normalize=="absmean")   normfact <- mean(abs(obs),na.rm = na_remove)
  if(normalize=="stdev")     normfact <- sd(obs,na.rm = na_remove)
  if(normalize=="range")     normfact <- (max(obs,na.rm = na_remove)-min(obs,na.rm = na_remove))
  return(err/normfact)
}
