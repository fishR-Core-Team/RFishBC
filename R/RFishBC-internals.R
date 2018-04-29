#' @title Internal functions.
#'
#' @description Internal functions.
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach STOP WARN iHndlFilename iHndlID iHndlScalingFactor iGetImage  iSelectAnulli iFindTransect iShowTransect iSnap2Transect iProcessAnnuli iPlaceText


################################################################################
# Same as stop() and warning() but with call.=FALSE as default
################################################################################
STOP <- function(...,call.=FALSE,domain=NULL) stop(...,call.=call.,domain=domain)
WARN <- function(...,call.=FALSE,immediate.=FALSE,noBreaks.=FALSE,domain=NULL) {
  warning(...,call.=call.,immediate.=immediate.,noBreaks.=noBreaks.,domain=domain)
}


########################################################################
## Sends a start-up message to the console when the package is loaded.
########################################################################
.onAttach <- function(lib,pkg,...) {
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),
                   fields="Version")
  msg <- paste0("## RFishBC v",vers,". See vignettes at derekogle.com/RFishBC/.\n")
  packageStartupMessage(msg)
}



########################################################################
## Handles filenames
##
## Allows the user to choose a filename if none is given.
## Returns the originallly sent filename in givenm, the directory for
##   the file in dirname, and just the filename (no path) in basenm.
########################################################################
iHndlFilename <- function(givennm) {
  if (missing(givennm)) {
    givennm <- file.choose()
    if (missing(givennm)) STOP("A filename must be provided.")
  }
  if (length(givennm)>1) {
    ## If more than one name in fname, then just pass it through
    givennm
  } else {
    ## If just one name then handle directory etc.
    ## get directory name ... changed to working directory if =="."
    dirnm <- dirname(givennm)
    if (dirnm==".") dirnm <- getwd()
    ## return list with filename, directory name, and combined
    basenm <- basename(givennm)
    list(givennm=givennm,dirnm=dirnm,basenm=basenm)
  }
}


########################################################################
## Handles fish ID
##
## Allows the user to enter a fish ID if none was given. The ID can be
##   entered in a dialog box if using windows (will initially be
##   populated with the basenm in fname) or at the prompt. If an id was
##   given then it just passes that value through.
########################################################################
iHndlID <- function(id,fn,popID) {
  if (missing(id)) {
    if (grepl('w|W', .Platform$OS.type)) {
      ## we are on Windows
      ## use basename in fname as the default if popID=TRUE
      defID <- ifelse(popID,tools::file_path_sans_ext(fn$basenm),"")
      id <- utils::winDialogString("Enter a unique ID: ",defID)
    } else {
      ## Not on Windows so prompt in console if in interactive session
      if (interactive()) id <- readline(prompt="Enter a unique ID: ")
    }
    if (missing(id) | is.null(id)) STOP("You must provide a unique ID in 'id'.")
  }
  id
}



########################################################################
## Computes a scaling factor from a scale bar on the structure image.
##
########################################################################
iHndlScalingFactor <- function(scaleBar,knownLength,scalingFactor,
                               col,lwd,pixW2H) {
  if (scaleBar) {
    ## scaleBar is on the plot
    message("\n>> Find scaling factor from scale bar.\n",
            "   * Select endpoints on the scale bar.")
    tmp <- as.data.frame(graphics::locator(n=2,type="p",pch=3,col=col))
    if (nrow(tmp)<2) {
      WARN("Two endpoints were not selected for the scale bar;\n thus, no scaling factor was computed.")
      scalingFactor <- 1
    } else {
      graphics::lines(y~x,data=tmp,col=col,lwd=lwd)
      ## Find distances in x- and y- directions, corrected for pixel w to h ratio
      distx <- (tmp$x[2]-tmp$x[1])*pixW2H
      disty <- tmp$y[2]-tmp$y[1]
      ## Find distances between points
      maglength <- sqrt(distx^2+disty^2)
      scalingFactor <- knownLength/maglength
    }
    SF <- list(sfSource="ScaleBar",sbPts=tmp,sbLength=knownLength,
               scalingFactor=scalingFactor)
  } else {
    ## No scale bar on the plot ... using the scaling factor
    message("** Using the 'scalingFactor' provided.")
    SF <- list(sfSource="Provided",sbPts=NULL,sbLength=NULL,
               scalingFactor=scalingFactor)
  }
  SF
}





########################################################################
## Load and displays a structure image.
##
## Loads and plots any of the four bitmapped types of images as chosen
##   by the user. This uses readbitmap::read.bitmap() so that any of
##   PNG, JPG, BMP, or TIFF files will be automatically detected. This
##   function is styled off the unexported digitize::ReadImg().
########################################################################
iGetImage <- function(fname,id,sepWindow,windowSize,
                      showInfo,pos.info,cex.info,col.info) {
  ## Read the file
  img <- readbitmap::read.bitmap(fname,native=TRUE)
  ## Open separate window if asked to do so (avoids putting in RStudio pane)
  if (sepWindow) {
    ## Get window size so image displayed in its native aspect ratio.
    ## Only needed if windowSize contains one value.
    if (length(windowSize)==1) {
      cf <- dim(img)[2:1]
      windowSize <- windowSize*cf/max(cf) 
    }
    tmp <- grDevices::dev.cur()
    ## If a window is already open, close it as its aspect ratio may be wrong 
    if (tmp!=1 & names(tmp)!="RStudioGD") grDevices::dev.off()
    ## Open the new window
    grDevices::dev.new(rescale="fixed",noRStudioGD=TRUE,
                       width=windowSize[1],height=windowSize[2],
                       title=paste0("Image: ",basename(fname)))
  }
  ## Plot the image
  withr::local_par(list(mar=c(0,0,0,0),xaxs="i",yaxs="i"))
  graphics::plot.new()
  graphics::rasterImage(img,0,0,1,1)
  ## Add ID information to image if told to do so
  if (showInfo) iPlaceText(paste0("ID=",id),pos.info,cex=cex.info,col=col.info)
  ## Return information
  invisible(list(windowSize=windowSize,pixW2H=windowSize[1]/windowSize[2]))
}


########################################################################
## Interactively select annuli
##
## Allows the user to interactively select points on a plot by clicking
##   with the first mouse button on the image. When finished, the x- and
##   y-coordinates for each selected point will be returned.
########################################################################
iSelectAnnuli <- function(pch.pts,col.pts,cex.pts,
                          showTransect,snap2Transect,
                          col.transect,lwd.transect) {
  ## Deal with transect first
  trans <- iFindTransect(pch.pts=pch.pts,col.pts=col.pts,cex.pts=cex.pts)
  if (showTransect) iShowTransect(trans$ptsTransect,
                                  col.transect=col.transect,
                                  lwd.transect=lwd.transect)
  
  ## Deal with annuli second
  message("\n>> Select points that are annuli.\n",
          "   * When finished selecting points press\n",
          "       the second(right) mouse button and select 'Stop',\n",
          "       the 'Stop' button in Windows, or\n",
          "       the 'Finish' button in RStudio.")
  
  ## Initially populate pts and orig.pts with transect points
  pts <- orig.pts <- trans$ptsTransect
  
  ## Allow user to select one point at-a-time until locator stopped
  ## Selected points will be snapped to transect if snap2Transect==TRUE
  ## orig.pts are as selected by the user, pts may be on transect if
  ##   snap2Transect==TRUE but may not be if snap2Transect==FALSE
  repeat {
    tmp2 <- as.data.frame(graphics::locator(n=1,
                                            type=ifelse(snap2Transect,"n","p"),
                                            pch=pch.pts,col=col.pts,cex=cex.pts))
    if (!nrow(tmp2)>0) {
      ## no point was selected, user must have selected stop locator
      break
    } else {
      ## A point was selected
      orig.pts <- rbind(orig.pts,tmp2)
      if (!snap2Transect) {
        ## if not snapping points then pts=orig.pts
        pts <- orig.pts
      } else {
        ## snap points to the transect
        tmp2 <- iSnap2Transect(trans,tmp2)
        ## plot the snapped point
        graphics::points(y~x,data=tmp2,pch=pch.pts,col=col.pts,cex=cex.pts)
        ## and add snapped point to matrix of points
        pts <- rbind(pts,tmp2)
      }      
    }
  } # end repeat
  
  if (!nrow(pts)>2) STOP("No points were selected as annuli.")
  message("   * ",nrow(pts)," points were selected.\n")
  
  ## Re-order points by distance from the first point (the focus)
  pts <- iOrderPts(pts)
  orig.pts <- iOrderPts(orig.pts)
  
  ## Return the two types of selected points and info about the transect
  list(pts=pts,orig.pts=orig.pts,
       slpTransect=trans$slpTransect,intTransect=trans$intTransect)
}


########################################################################
## Finds a transect on an image
##
## Allows the user to interactively select a transect on the structure
##   image by selecting a point at the focus and margin. Coordinates
##   for those points and the slope, intercept, and slope of the line
##   perpendicular to the transection are returned.
########################################################################
iFindTransect <- function(pch.pts,col.pts,cex.pts) {
  ## Asks user to select two points at the structure focus and margin
  ## that will serve as the transect. Returns the coords of those points.
  message("\n>> Select transect endpoints.\n",
          "   * MUST select the focus of the structure FIRST.\n",
          "   * MUST select structure margin SECOND.")
  tmp1 <- as.data.frame(graphics::locator(n=2,type="p",pch=pch.pts,
                                          col=col.pts,cex=cex.pts))
  if (nrow(tmp1)<2) STOP("Either the focus or margin was not selected.")
  
  ## Calculate slope, intercept, and perpendicular slope to transect
  slpTransect <- diff(tmp1$y)/diff(tmp1$x)
  intTransect <- tmp1$y[1]-slpTransect*tmp1$x[1]
  slpPerpTransect <- -1/slpTransect
  
  ## Return all items in a list
  list(ptsTransect=tmp1,slpTransect=slpTransect,
       intTransect=intTransect,slpPerpTransect=slpPerpTransect)
}


########################################################################
## Shows a transect on the image
##
## Shows a previously defined transect on the  structure image.
########################################################################
iShowTransect <- function(obj,col.transect,lwd.transect) {
  graphics::lines(y~x,data=obj,lwd=lwd.transect,col=col.transect)
}


########################################################################
## Snaps selected points to the transect
##
## Perpendicularly "slides" a point to fall on the transect.
########################################################################
iSnap2Transect <- function(trans,pts) {
  ## Intercept of line perpendicular to transect through the point.
  intPerp <- pts$y-trans$slpPerpTransect*pts$x
  ## Intersection between transect and perpendicular line through the point
  intersectsX <- (intPerp-trans$intTransect)/
    (trans$slpTransect-trans$slpPerpTransect)
  intersectsY <- trans$intTransect+trans$slpTransect*intersectsX
  ## Return snapped coordinates
  data.frame(x=intersectsX,y=intersectsY)
}




########################################################################
## Processes selected points into data
##
## Processes the point locations to radial measurements, computes
##   an age-at-capture, and radius-at-capture. Combines all structure-
##   related information into a data.frame. Combines all process-related
##   and structure-related information into a list and writes that
##   information into an R object data file in the current working
##   directory.
########################################################################
iProcessAnnuli <- function(nms,dat,id,reading,suffix,description,
                           edgeIsAnnulus,scalingFactor,pixW2H) {
  ## Convert point locations to radial measurements, 
  n <- nrow(dat$pts)-1
  ## Find distances in x- and y- directions, corrected for pixel w to h ratio
  distx <- (dat$pts$x[2:(n+1)]-dat$pts$x[1])*pixW2H
  disty <- dat$pts$y[2:(n+1)]-dat$pts$y[1]
  ## Find distances between points
  distxy <- sqrt(distx^2+disty^2)
  ## Correct distances for scalingFactor ... and call a radius
  rad <- distxy*scalingFactor
  ## Sort the radii to be in increasing order (allows user to select the
  ##   points in any order)
  rad <- rad[order(rad)]
  ## create data.frame with radii information
  reading <- ifelse(is.null(reading),NA,reading)
  radii <- data.frame(id=as.character(rep(id,n)),
                      reading=as.character(rep(reading,n)),
                      agecap=ifelse(edgeIsAnnulus,n,n-1),
                      ann=seq_len(n),
                      rad=rad,radcap=max(rad),
                      stringsAsFactors=FALSE)
  ## Organize all results for later processing
  dat <- list(description=description,image=nms$givennm,
              basenm=nms$basenm,dirnm=nms$dirnm,
              datanm=paste0(tools::file_path_sans_ext(nms$basenm),
                            ifelse(!is.null(suffix),"_",""),
                            suffix,".RData"),
              edgeIsAnnulus=edgeIsAnnulus,
              slpTransect=dat$slpTransect,intTransect=dat$intTransect,
              pts=dat$pts,orig.pts=dat$orig.pts,radii=radii)
  dat
}



########################################################################
## Show annuli numbers on the showDigitizedImage() image
##
########################################################################
iShowAnnuliLabels <- function(dat,annuliLabels,col.ann,cex.ann) {
  ## Get points to plot
  pts <- dat$pts
  
  ## Find the degree of angle for the transect slope
  deg <- atan(dat$slpTransect)*180/pi
  #### Adjust for the quadrant in which the transect is in
  if (pts$x[nrow(pts)]<pts$x[1]) deg <- deg+180
  ## Convert absolute transect degress into a position for the text
  deg <- abs(deg)
  if (deg>=0 & deg<=45) pos <- 1        # below
  else if (deg>45 & deg<=90) pos <- 4   # right
  else if (deg>90 & deg<=135) pos <- 2  # left
  else if (deg>135 & deg>=180) pos <- 1 # below
  
  ## Put on text
  #### make labels from 1 to the number of points marked (-1 for the focus)
  lbls <- 1:(nrow(pts)-1)
  #### convert annuli not in annuliLabels to ""
  if (!is.null(annuliLabels)) lbls[!lbls %in% annuliLabels] <- ""
  #### add a "" for the focus
  lbls <- c("",lbls)
  #### remove the annuli number for the edge if it is not an annulus
  if (!dat$edgeIsAnnulus) lbls[length(lbls)] <- ""
  #### put the labels on the plot
  graphics::text(y~x,data=dat$pts,labels=lbls,font=2,
                 col=col.ann,cex=cex.ann,pos=pos)
}




########################################################################
## Orders a data.frame of x-y coordinates by distance from first point.
########################################################################
iOrderPts <- function(pts) {
  ## find a matrix of distances from the first point (in the first column
  ## returned by dist()), finds the order of those distances, and re-orders
  ## the original points by that order and returns the result
  pts[order(as.matrix(stats::dist(pts))[,1]),]
}



########################################################################
## Places text in txt= on an active plot at a position given in pos=
########################################################################
iPlaceText <- function(txt,pos,cex,col) {
  usr <- graphics::par("usr")
  if (pos=="topleft") {
    graphics::text(usr[1],usr[4],txt,adj=c(0,1),col=col,cex=cex)
  } else if (pos=="top") {
    graphics::text(mean(usr[1:2]),usr[4],txt,adj=c(0.5,1),col=col,cex=cex)
  } else if (pos=="topright") {
    graphics::text(usr[2],usr[4],txt,adj=c(1,1),col=col,cex=cex)
  } else if (pos=="right") {
    graphics::text(usr[2],mean(usr[3:4]),txt,adj=c(1,0.5),col=col,cex=cex)
  } else if (pos=="bottomright") {
    graphics::text(usr[2],usr[3],txt,adj=c(1,0),col=col,cex=cex)
  } else if (pos=="bottom") {
    graphics::text(mean(usr[1:2]),usr[3],txt,adj=c(0.5,0),col=col,cex=cex)
  } else if (pos=="bottomleft") {
    graphics::text(usr[1],usr[3],txt,adj=c(0,0),col=col,cex=cex)
  } else if (pos=="left") {
    graphics::text(usr[1],mean(usr[3:4]),txt,adj=c(0,0.5),col=col,cex=cex)
  }
}
