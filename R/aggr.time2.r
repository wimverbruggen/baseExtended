#' Aggregate data over time.
#' Much faster than version 1, but that one is kept for backwards compatibility.
#' This will aggregate *all* fields in the dataframe, except the POSIX. NA values will be omitted.
#' @param dat    (data)    Dataframe containing everything (data,posix)
#' @param by     (string)  A time interval. Possible values: day, week, month, year
#' @param f      (string)  A function. Possible values: mean (default), sum, max, min, sd
#' @return A list containing the $data and the coresponding $posix dates (*first* moment of day/week/month/year is given)
#' @author Wim Verbruggen
#' @export
aggr.time2 <- function(dat,by,f="mean"){

  require(lubridate)
  timezone <- attr(dat$posix,"tzone"); if(is.null(timezone)) timezone<-"UTC"

  # Set up date formatting, depending on what is chosen for "by"
  if(by=="day")   {fm.agg <- "%Y%j"  ; fm.pos <- "%Y%j"}
  if(by=="week")  {fm.agg <- "%Y%W1" ; fm.pos <- "%Y%W%u"}
  if(by=="month") {fm.agg <- "%Y%m01"; fm.pos <- "%Y%m%d"}
  if(by=="year")  {fm.agg <- "%Y0101"; fm.pos <- "%Y%m%d"}

  agg.by   <- list(strftime(dat$posix,   format=fm.agg, tz=timezone))
  newposix <- as.POSIXct(unique(unlist(agg.by)), format=fm.pos, tz=timezone)
  oldposix <- dat$posix

  newsteps <- length(newposix)

  # Remove the original posix, as we don't want it aggregated and replace it later
  dat$posix <- NULL

  # Get all remaining variables and their dimensions
  vars     <- names(dat)
  vars.dim <- list()
  for(v in vars) vars.dim[[v]] <- dim(dat[[v]])

  # Permute so that the time dimension is put first. This will fail miserably if there's another variable dimension of the time dimension's length.
  tdim <- length(oldposix)
  vars.perm <- names(vars.dim)
  vdim <- list()
  rdim <- list()
  for(v in vars.perm){
    vdim[[v]] <- which(vars.dim[[v]]==tdim); if(length(vdim[[v]])!=1) stop("Not exactly one time dimension!") # Which dimension is our time dimension?
    rdim[[v]] <- which(vars.dim[[v]]!=tdim)       # List of all the other dimensions
    dat[[v]] <- aperm(dat[[v]],c(vdim[[v]],rdim[[v]])) # Permute so that the time dimension is first
  }

  # Actual aggregating
  agg <- as.list(aggregate(dat,agg.by,FUN=f))

  # Group together again in a matrix
  for(v in vars.perm){
    vars.to.group <- names(agg[which(startsWith(names(agg),v))])
    vars.to.group <- vars.to.group[!vars.to.group %in% names(dat)] # to prevent that we add variables that accidentally start with the same prefix...
    agg[[v]] <- array(NA,dim=c(vars.dim[[v]][rdim[[v]]],newsteps))
    for(g in vars.to.group){
      dimpos <- as.numeric(sub(paste0(v,".")," ",g))
      for(t in 1:newsteps){
        indx <- matrix(ncol = length(dimpos)+1,c(dimpos,t))
        agg[[v]][indx] <- agg[[g]][indx[length(indx)]]
      }
      agg[[g]] <- NULL
    }
  }

  # Replace posix by first day of week/month/year
  agg$posix <- newposix

  # Clean up
  agg$Group.1 <- NULL

  return(agg)

}
