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
normDiameters <- function(dat, bins, tf=1, normalizeToBaseline=TRUE, normDrift="no",win, strength, cleanselected = FALSE){

  if(cleanselected){
    #samples are cleaned up
    ll <- cleanDat(dat, win, strength, cleanselected)
  }else{
    ll <- dat
  }
  
  normDrift <- match.arg(normDrift, c("no","global","specific"))
  
  ll <- lapply(ll, tf=tf, FUN=function(x,tf){
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

#Function to remove outliers from the data. Will detect outliers within a window (win) that are a percentage (strength) over the median, and sets them to the average of the previous and next non-outlier diameters
cleanDat <- function(dat, win, strength, selected = FALSE){
  if(!selected)return(dat)
  
  clean <- dat
  for(i in names(clean)){
    for(j in 1:length(clean[[i]]$Diameter)){
      if(is.na(clean[[i]]$Diameter[j])){
        print(clean[[i]]$Diameter[j])
        clean[[i]]$Outlier[j] <- TRUE
      }
      if(abs(log2(median(clean[[i]][((clean[[i]]$Time > (clean[[i]]$Time[j] - win)) & (clean[[i]]$Time < (clean[[i]]$Time[j] + win))), "Diameter"]) / clean[[i]]$Diameter[j])) > abs(log2(100/strength))){
        clean[[i]]$Outlier[j] <- TRUE
      }else{
        clean[[i]]$Outlier[j] <- FALSE
      }
    }
  }
  for(i in names(clean)){
    for(j in 1:length(clean[[i]]$Diameter)){
      if(clean[[i]]$Outlier[j]){
        nextlow <- max(clean[[i]][!clean[[i]]$Outlier & clean[[i]]$Time < clean[[i]]$Time[j],"Time"])
        nextup <- min(clean[[i]][!clean[[i]]$Outlier & clean[[i]]$Time > clean[[i]]$Time[j],"Time"])
        if(nextlow == -Inf){clean[[i]]$Diameter[j] <- clean[[i]][clean[[i]]$Time == nextup, "Diameter"] }
        else if(nextup == Inf){clean[[i]]$Diameter[j] <- clean[[i]][clean[[i]]$Time == nextlow, "Diameter"]}
        else{clean[[i]]$Diameter[j] <- mean(c(clean[[i]][clean[[i]]$Time == nextup, "Diameter"],clean[[i]][clean[[i]]$Time == nextlow, "Diameter"]))}
      }
    }
    clean[[i]]$Outlier <- NULL
  }
  return(clean)
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
    bins$MaxDiameter <- sapply(1:nrow(bins), bins=bins, d=x, FUN=function(i,bins,d){
      w <- which(d[,1]>=bins[i,1] & d[,1]<=bins[i,2])
      if(length(w)==0)  return(NA)
      max(d[w,2],na.rm=T)
    })
    bins
  })
}


#' getQualitativePalette
#'
#' Returns a qualitative color palette of the given size. If less than 23 colors are required,
#' the colors are based on Paul Tol's palettes. If more, the `randomcoloR` package is used.
#'
#' @param nbcolors number of colors (from 1 to 22)
#'
#' @return A vector of colors
#'
#' @export
getQualitativePalette <- function(nbcolors){
  nbcolors <- round(nbcolors)
  if(nbcolors>22){
    library(randomcoloR)
    return(distinctColorPalette(nbcolors))
  }
  switch(as.character(nbcolors),
         "1"=c("#4477AA"),
         "2"=c("#4477AA", "#CC6677"),
         "3"=c("#4477AA", "#DDCC77", "#CC6677"),
         "4"=c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
         "5"=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
         "6"=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
         "7"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
         "8"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
         "9"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
         "10"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
         "11"=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
         "12"=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"),
         "13"=c("#882E72", "#B178A6", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"),
         "14"=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"),
         "15"=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA"),
         "16"=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA", "black"),
         "17"=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
         "18"=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
         "19"=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788", "black"),
         "20"= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
         "21"= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
         "22"= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788", "black")
  )
}


.getRibbonData <- function(data, errortype, groups){
  data <- lapply(split(data,groups[as.character(names(data))]), FUN=function(x){
    if(length(x)==1) x[[1]]
    m <- rowMeans(sapply(x,FUN=function(x) x[,2]))
    SD <- apply(sapply(x,FUN=function(x) x[,2]),1,FUN=sd)
    if(errortype=="SE") SD <- SD/sqrt(length(x))
    if(errortype =="CI") SD <- qnorm(0.975)*SD/sqrt(length(x))
    data.frame(Time=x[[1]][,1], Diameter=m, Diameter_low=m-SD, Diameter_high=m+SD)
  })
}

TransformDLC <- function(data,header,likelihood_cutoff, completeness_cutoff, center_point,input_points){
  out <- list()
  pupil_points <- str_split(input_points,pattern = ",")[[1]]
  point <- t(data.frame(sapply(header[1,], as.character), stringsAsFactors=FALSE))
  coord <- t(data.frame(sapply(header[2,], as.character), stringsAsFactors=FALSE))
  
  #filter out points with to many bad values completely and interpolate any points that have to low likelihood
  for(j in pupil_points){
    if(sum(data[,point == j & coord == "likelihood"] >= likelihood_cutoff) < completeness_cutoff * dim(data)[1]){
      pupil_points <- pupil_points[!pupil_points %in% j]
      out$Report <- append(out$Report,paste("Dropping point ",j,": To many low likelihood values", sep = ""))
    }else{
      to_adjust <- data[,point == j & coord =="likelihood"] <= likelihood_cutoff
      data[to_adjust, point == j & coord == "x"] <- NA
      data[to_adjust, point == j & coord == "y"] <- NA
      data[,point == j & coord == "x"] <- InterpolateNA(data[,point == j & coord == "x"])
      data[,point == j & coord == "y"] <- InterpolateNA(data[,point == j & coord == "y"])
    }
  }
  #interploate center point. If to many center points are missing return error message
  to_adjust <- data[,point == center_point & coord =="likelihood"] <= likelihood_cutoff
  if(sum(!to_adjust) < completeness_cutoff* dim(data)[1]){
    out$Report <- append(out$Report,"WARNING: CENTER POINT HAS TO MANY LOW LIKELIHOOD VALUES. ANALYSIS NOT POSSIBLE!!! Dropping file")
    out$dat <- NULL
    return(out)
  }
  data[to_adjust, point == center_point & coord == "x"] <- NA
  data[to_adjust, point == center_point & coord == "y"] <- NA
  data[,point == center_point & coord == "x"] <- InterpolateNA(data[,point == center_point & coord == "x"])
  data[,point == center_point & coord == "y"] <- InterpolateNA(data[,point == center_point & coord == "y"])
  
  if(length(pupil_points) < 1){
    out$Report <- append(out$Report,"WARNING: ALL PUPIL POINTS HAVE TO MANY LOW LIKELIHOOD VALUES. ANALYSIS NOT POSSIBLE!!! Dropping file")
    out$dat <- NULL
    return(out)
  }
  
  
  #Calculate distance to center for each point
  dist_center <- NULL
  for(j in pupil_points){
    dist_center <- rbind(dist_center,distance_xy(data,j,point,coord,center_point))
  }
  #Take mean of distance for each point to determine final radius
  dist_center <- apply(dist_center,2,FUN = mean)
  out$dat <- data.frame(Time = data[,1],Diameter = dist_center)
  out$Report <- append(out$Report,"Radius calculation OK!")
  return(out)
}

distance_xy <- function(x,p,point,coord,center_point){
  sqrt((x[,point == p & coord == "x"] - x[,point == center_point & coord == "x"])^2 + (x[,point == p & coord == "y"] - x[,point == center_point & coord == "y"])^2)
}

#This function is copied from the package imputeTS. due to update issues the dependency was dropped and the code copied here.
InterpolateNA <- function(x)
{
  data <- x
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    for (i in 1:dim(data)[2]) {
      if (!anyNA(data[, i])) {
        next
      }
      tryCatch(data[, i] <- InterpolateNA(data[, i], 
                                             option, maxgap), error = function(cond) {
                                               warning(paste("imputeTS: No imputation performed for column", 
                                                             i, "because of this", cond), call. = FALSE)
                                             })
    }
    return(data)
  }
  else {
    missindx <- is.na(data)
    if (!anyNA(data)) {
      return(data)
    }
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }
    if (sum(!missindx) < 2) {
      stop("Input data needs at least 2 non-NA data point for applying na_interpolation")
    }
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      stop("Wrong input type for parameter x")
    }
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }
    if (!is.numeric(data)) {
      stop("Input x is not numeric")
    }
    n <- length(data)
    allindx <- 1:n
    indx <- allindx[!missindx]
    data_vec <- as.vector(data)
    interp <- stats::approx(indx, data_vec[indx], 1:n, 
                              rule = 2, ...)$y

    data[missindx] <- interp[missindx]
    if (is.finite(maxgap) && maxgap >= 0) {
      rlencoding <- rle(is.na(x))
      rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE
      en <- inverse.rle(rlencoding)
      data[en == TRUE] <- NA
    }
    if (!is.null(dim(x)[2])) {
      x[, 1] <- data
      return(x)
    }
    return(data)
  }
}