#' @title Internal functions.
#'
#' @description Internal functions.
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach STOP WARN iHndlFilenames iGetImage iPts2Rad iSnap2Transect iScalingFactorFromScaleBar iPlaceText


################################################################################
# Same as stop() and warning() but with call.=FALSE as default
################################################################################
STOP <- function(...,call.=FALSE,domain=NULL) stop(...,call.=call.,domain=domain)
WARN <- function(...,call.=FALSE,immediate.=FALSE,noBreaks.=FALSE,domain=NULL) {
  warning(...,call.=call.,immediate.=immediate.,noBreaks.=noBreaks.,domain=domain)
}



################################################################################
# Functions to allow symbols in the messages. Basically taken from fcuk package.
################################################################################
CATLINE <- function(...) cat(..., "\n", sep = "")
BULLET <- function(lines,bullet) CATLINE(paste0(bullet," ",lines))
DONE <- function(...,sep="") 
  BULLET(paste0(...,sep=sep),bullet=crayon::green(clisymbols::symbol$tick))
NOTE <- function(...,sep="") 
  BULLET(paste0(...,sep=sep),bullet=crayon::blue(clisymbols::symbol$checkbox_on))




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
## Handles processes related to a single file name.
########################################################################
iHndlFilenames <- function(nm,filter,multi=TRUE) {
  #### Allow user to select the image from a dialog box.
  if (missing(nm)) {
    if (grepl('w|W', .Platform$OS.type)) {
      RFBCFilters <- rbind(images=c("Bitmapped image files",
                                    "*.jpg;*.jpeg;*.png;*.tiff;*.tif;*.bmp"),
                           utils::Filters[c("RData","All"),])
      nm <- utils::choose.files(filter=RFBCFilters[c("All",filter),,drop=FALSE],
                                multi=multi,
                                caption=ifelse(multi,"Select files",
                                               "Select a file"))
    }
    if (missing(nm)| length(nm)==0)
      STOP("A filename must be provided in the first argument.")
  }
  #### Make sure that the file is in the current working directory
  dn <- dirname(nm[1])
  wd <- getwd()
  if (!dn %in% c(".",wd)) {
    message("The current working directory is ",wd)
    message("The directory with the chosen file is ",dn)
    STOP("The file MUST be in the current working directory.\n",
         "       Please use 'setwd()' to change the working directory\n",
         "       and then try the function again.")
  }
  #### Make sure just the filenames (no path info) is returned
  basename(nm)
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
## Convert selected x-y points to radial measurements
########################################################################
iPts2Rad <- function(pts,edgeIsAnnulus,scalingFactor,pixW2H,id,reading) {
  #### Number of radial measurements is one less than number of points selected
  n <- nrow(pts)-1
  #### Distances in x- and y- directions, corrected for pixel w to h ratio
  distx <- (pts$x[2:(n+1)]-pts$x[1])*pixW2H
  disty <- pts$y[2:(n+1)]-pts$y[1]
  #### Distances between points
  distxy <- sqrt(distx^2+disty^2)
  #### Correct distances for scalingFactor ... and call a radius
  rad <- distxy*scalingFactor
  #### Sort radii in increasing order (probably redundant)
  rad <- rad[order(rad)]
  #### create data.frame with radii information
  data.frame(id=as.character(rep(id,n)),
             reading=as.character(rep(ifelse(is.null(reading),NA,reading),n)),
             agecap=ifelse(edgeIsAnnulus,n,n-1),
             ann=seq_len(n),
             rad=rad,radcap=max(rad),
             stringsAsFactors=FALSE)
}



iScalingFactorFromScaleBar <- function(knownLength,pixW2H,
                                       col.scaleBar,lwd.scaleBar) {
  sbPts <- as.data.frame(graphics::locator(n=2,type="p",pch=3,col=col.scaleBar))
  if (nrow(sbPts)<2) {
    WARN("Two endpoints were not selected for the scale bar;\n","
           thus, a scaling factor of 1 will be used.")
    scalingFactor <- 1
  } else {
    ## Show the user-selected marking on the image
    graphics::lines(y~x,data=sbPts,col=col.scaleBar,lwd=lwd.scaleBar)
    ## Find distances in x- and y- directions,
    ##   corrected for pixel w to h ratio
    distx <- (sbPts$x[2]-sbPts$x[1])*pixW2H
    disty <- sbPts$y[2]-sbPts$y[1]
    ## Return a list (scaling factor is known / distance between points)
    list(sbPts=sbPts,scalingFactor=knownLength/sqrt(distx^2+disty^2))
  }
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
