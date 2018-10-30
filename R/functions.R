#' pupillometry.app
#'
#' Launches the pupillometry app.
#'
#' @export
pupillometry.app <- function(){
  library(shiny)
  shinyApp(app.ui(), app.server())
}

#' getTimeDriftCoefficient
#'
#' Return the coefficient of a Diameter~Time rlm on non-response timepoints
#'
#' @param dat A list of data.frames containing at least the `Time` and `Diameter`
#' @param responseBins A matrix of response bins (each row with start and end of the bin)
#'
#' @return A numeric vector of the same length as `dat`
#' @export
getTimeDriftCoefficient <- function(dat, responseBins){
  # we first remove the time points in the response bins
  w <- sapply(dat$Time,bins=responseBins, FUN=function(x, bins){
    any(apply(bins,1,y=x,FUN=function(x,y){ y>=x[1] & y<=x[2] }))
  })
  dat <- dat[which(!w),]
  # we then fit a robust linear model
  library(MASS)
  mod <- try(rlm(Diameter~Time, data=dat), silent=T)
  if(is(mod, "try-error")) return(NA)
  coef(mod)["Time"]
}


#' normDiameters
#'
#' @param dat A list of datasets, each being a data.frame with at least the `Time` and `Diameter` columns.
#' @param bins A list with slots `baseline` and `response`, each corresponding a matrix of bins (one per row, start-end)
#' @param tf A time scaling factor (default 1, i.e. no scaling)
#' @param normalizeToBaseline Logical; whether to normalize to the baseline (Default TRUE)
#' @param normDrift Normalize baseline drift across time. Use 'no' to disable (default), 'global' to identify the drift
#' across all samples, or 'specific' to correct for sample-specific drift.
#'
#' @return A list of normalized datasets.
#' @export
normDiameters <- function(dat, bins, tf=1, normalizeToBaseline=TRUE, normDrift="no"){
  normDrift <- match.arg(normDrift, c("no","global","specific"))
  ll <- lapply(dat$diameters, tf=tf, FUN=function(x,tf){
    x$Time <- x$Time*tf
    return(x)
  })
  if(normalizeToBaseline){
    # we normalize to the first baseline bin
    ll <- lapply(ll, bb=bins$baseline[1,], FUN=function(x,bb){
      bl <- mean(x[which(x$Time>=bb[1] & x$Time<=bb[2]),"Diameter"],na.rm=T)
      x$Diameter <- 100*x$Diameter/bl
      x
    })
  }
  if(normDrift!="no"){
    # we remove the lienar drift across time
    if(normDrift=="global"){
      dcoefs <- getTimeDriftCoefficient(do.call(rbind,ll), bins$response)
      dcoefs <- rep(dcoefs, length(ll))
    }else{
      dcoefs <- sapply(ll, responseBins=bins$response, FUN=getTimeDriftCoefficient)
    }
    for(i in 1:length(ll)){
      if(!is.na(dcoefs[i])){
        ll[[i]]$Diameter <- ll[[i]]$Diameter-ll[[i]]$Time*dcoefs[i]
      }
    }
  }
  return(ll)
}



# extracts bins from text input
.parseBins <- function(x){
  for(cc in c(";","\r")) x <- gsub(cc,"\n",x,fixed=T)
  x <- gsub(" ","",x)
  x <- lapply(strsplit(x,"\n",fixed=T)[[1]], FUN=function(x){
    as.numeric(strsplit(x,"-",fixed=T)[[1]])
  })
  x <- x[which(sapply(x,length)>0)]
  if(length(x)==0 || any(sapply(x,length)!=2)) return(NULL)
  do.call(rbind, x)
}

# prepares the bin coordinates for plotting
.getBinAreas <- function(bins, yrange, color="black", opacity=0.1){
  shapes <- list()
  if(is.null(bins)) return(shapes)
  for(i in 1:nrow(bins)){
    shapes[[i]] <- list(type = "rect", fillcolor=color, opacity=opacity, x0=bins[i,1], x1=bins[i,2], y0=yrange[1], y1=yrange[2], xref="x", yref="y")
  }
  return(shapes)
}

# Reduces the resolution (and ev. noise) by averaging values over intervals
.avgIntervals <- function(data, interval){
  maxResolution <- max(sapply(data, FUN=function(x){ (max(x[,1])-min(x[,1]))/length(x[,1]) }),na.rm=T)
  if(interval > maxResolution){
    trange <- range(unlist(lapply(data, FUN=function(x) x[,1])))
    breaks <- seq(from=trange[1], to=trange[2], by=interval)
    data <- lapply(data,breaks=breaks,FUN=function(x, breaks){
      res <- sapply(1:(length(breaks)-1), d=x, breaks=breaks, FUN=function(i,d,breaks){
        if(i==(length(breaks)-1)){
          w <- which(d[,1]>=breaks[i] & d[,1]<=breaks[i+1])
        }else{
          w <- which(d[,1]>=breaks[i] & d[,1]<breaks[i+1])
        }
        if(length(w)==0)  return(NA)
        mean(d[w,2],na.rm=T)
      })
      data.frame(Time=(breaks[-1]+breaks[-length(breaks)])/2, Diameter=res)
    })
  }
  return(data)
}

# Get bin-level data
.getBinData <- function(data, bins){
  bins <- as.data.frame(bins)
  lapply(data, bins=bins, FUN=function(x,bins){
    bins$Diameter <- sapply(1:nrow(bins), bins=bins, d=x, FUN=function(i,bins,d){
      w <- which(d[,1]>=bins[i,1] & d[,1]<=bins[i,2])
      if(length(w)==0)  return(NA)
      mean(d[w,2],na.rm=T)
    })
    bins
  })
}