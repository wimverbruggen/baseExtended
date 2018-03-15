#' Root mean square deviation
#'
#' @param obs the 'observed' data (can also be field data for example)
#' @param sim the 'simulated' data (can also be satellite data for example)
#' @param normalize the normalizing method: none, mean, range, stdev
#' @param na_remove whether to ignore NA values
#' @return Root mean square deviation (fractional)
#' @export error.rmsd
error.rmsd <- function(obs,sim,normalize="mean",na_remove=TRUE){
  err <- sqrt(mean((obs-sim)^2, na.rm = na_remove))
  if(normalize=="mean")   err <- err/mean(obs,na.rm = na_remove)
  if(normalize=="stdev")  err <- err/sd(obs,na.rm = na_remove)
  if(normalize=="range")  err <- err/(max(obs,na.rm = na_remove)-min(obs,na.rm = na_remove))
  # if no normalize is specified we don't do any normalization at all
  return(err)
}
