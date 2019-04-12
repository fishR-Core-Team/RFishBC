#' @title Internal functions.
#'
#' @description Internal functions.
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach STOP WARN CATLINE BULLET DONE NOTE iHndlFilenames iGetImage iSelectPt iScalingFactorFromScaleBar iPlaceText isRData


########################################################################
## Sends a start-up message to the console when the package is loaded.
########################################################################
.onAttach <- function(lib,pkg,...) {                               # nocov start
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),
                   fields="Version")
  msg <- paste0("## RFishBC v",vers,". See vignettes at derekogle.com/RFishBC/.\n")
  packageStartupMessage(msg)
}                                                                  # nocov end



################################################################################
## Same as stop() and warning() but with call.=FALSE as default
################################################################################
STOP <- function(...,call.=FALSE,domain=NULL) stop(...,call.=call.,domain=domain)
WARN <- function(...,call.=FALSE,immediate.=FALSE,noBreaks.=FALSE,domain=NULL) {
  warning(...,call.=call.,immediate.=immediate.,noBreaks.=noBreaks.,domain=domain)
}



################################################################################
## Functions to allow symbols in the messages.
################################################################################
DONE <- function(...) 
  cli::cat_line(crayon::green(clisymbols::symbol$tick)," ",...)
DONE2 <- function(...) 
  cli::cat_line(crayon::red(clisymbols::symbol$cross)," ",...)
NOTE <- function(...) 
  cli::cat_line(crayon::blue(clisymbols::symbol$menu)," ",...)
RULE <- function(msg,line="=",line_col="green")
  cli::cat_rule(msg,line=line,line_col=line_col)



########################################################################
## Handles processes related to a single file name.
########################################################################
iHndlFilenames <- function(nm,filter,multi=TRUE) {
  #### Allow user to select the image from a dialog box.
  if (missing(nm)) {                                               # nocov start
    if (grepl('w|W', .Platform$OS.type)) {
      RFBCFilters <- rbind(images=c("Bitmapped image files",
                                    "*.jpg;*.jpeg;*.png;*.tiff;*.tif;*.bmp"),
                           RData=c("RData files","*.rds;*.RData;*.rda"),
                           All=utils::Filters["All",])
      nm <- utils::choose.files(filter=RFBCFilters[c("All",filter),,drop=FALSE],
                                multi=multi,
                                caption=ifelse(multi,"Select files",
                                               "Select a file"))
    }
    if (missing(nm) | length(nm)==0)
      STOP("A filename must be provided in the first argument.")
  }                                                                  # nocov end
  #### Make sure that the file is in the current working directory
  dn <- dirname(nm[1])
  wd <- getwd()
  if (!dn %in% c(".",wd)) {
    STOP("The file is in ",normalizePath(dn),", which is NOT\n",
         "       the current working directory of ",normalizePath(wd),".\n",
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
iGetImage <- function(fname,id,windowSize,deviceType,
                      showInfo,pos.info,cex.info,col.info) {
  ## Read the file
  img <- readbitmap::read.bitmap(fname,native=TRUE)
  ## Get window size so image displayed in its native aspect ratio.
  ## Only needed if windowSize contains one value.
  if (length(windowSize)==1) {                                     # nocov start
    cf <- dim(img)[2:1]
    windowSize <- windowSize*cf/max(cf) 
  }
  tmp <- grDevices::dev.cur()
  ## If a window is already open, close it as its aspect ratio may be wrong 
  if (tmp!=1 & names(tmp)!="RStudioGD") grDevices::dev.off()
  ## Open the new window (in the user's selected device)
  if (deviceType=="default")
    grDevices::dev.new(rescale="fixed",noRStudioGD=TRUE,
                       width=windowSize[1],height=windowSize[2],
                       title=paste0("Image: ",basename(fname)))
  if (deviceType=="X11")
    grDevices::X11(width=windowSize[1],height=windowSize[2],
                       title=paste0("Image: ",basename(fname)))
  ## Plot the image
  withr::local_par(list(mar=c(0,0,0,0),xaxs="i",yaxs="i"))
  graphics::plot.new()
  graphics::rasterImage(img,0,0,1,1)
  ## Add ID information to image if told to do so
  if (showInfo)
    iPlaceText(paste0("ID=",id),pos.info,cex=cex.info,col=col.info)  # nocov end
  ## Return information
  invisible(list(windowSize=windowSize,pixW2H=windowSize[1]/windowSize[2]))
}


########################################################################
## Allows the user to interactively select a point on the image. User
## can de-select a point with a key press and must select a key to
## identify that they are done selecting points.
########################################################################
iSelectPt <- function(numPts,msg1,msg2,
                      pch.sel,col.sel,cex.sel,
                      pch.del,col.del,
                      snap2Transect,trans.pts,
                      slpTransect,intTransect,slpPerpTransect) {   # nocov start
  ## Internal function for handling mouse down event
  mouseDown <- function(buttons,x,y) {
    tmp <- data.frame(x=graphics::grconvertX(x,"ndc","user"),
                      y=graphics::grconvertY(y,"ndc","user"))
    if (snap2Transect) 
      tmp <- iSnap2Transect(tmp,trans.pts,slpTransect,intTransect,slpPerpTransect)
    graphics::points(y~x,data=tmp,pch=pch.sel,col=col.sel,cex=cex.sel)
    dat <<- rbind(dat,tmp)
    NULL
  }
  ## Internal function for handling key press event
  keyPress <- function(key) {
    n <- nrow(dat)
    ### User requesting to be done with the process
    if (key=="f") {
      if (!is.null(numPts)) {
        ### Check to see if number of points is correct (if numPts given)
        if (n!=numPts) {
          tmpmsg <- paste("Must select exactly",numPts,"points. ")
          if (n<numPts) message(tmpmsg,"Please select ",numPts-n," more point(s).")
          if (n>numPts) message(tmpmsg,"Please de-select (press 'd' key) ",
                                n-numPts," point(s).")
        } else return(invisible(1))
      } else return(invisible(1))
    }
    ### User requesting to delete or remove a point
    if (key=="d") {
      if (n>=1) {
        graphics::points(y~x,data=dat[n,],pch=pch.del,col=col.del,cex=cex.sel)
        dat <<- dat[-n,]
      }
      NULL
    }
    ### User requesting to abort
    if (key=="q") {
      dat <<- "ABORT"
      return(invisible(1))
    }
    ### User requesting to start over with a clean slate
    if (key=="z") {
      dat <<- "RESTART"
      return(invisible(1))
    }
    ### User requesting to kill (same as abort for single image, gets out of loop if multiple images)
    if (key=="k") {
      dat <<- "KILLED"
      return(invisible(1))
    }
  }
  ## Main function
  dat <- data.frame(x=NULL,y=NULL)
  grDevices::getGraphicsEvent(paste0(msg1,msg2),consolePrompt="",
                              onMouseDown=mouseDown,onKeybd=keyPress)
  dat
}                                                                  # nocov end


########################################################################
## Finds the scaling factor from two points selected by the user.
########################################################################
iScalingFactorFromScaleBar <- function(msg2,knownLength,pixW2H,
                                       col.scaleBar,lwd.scaleBar,
                                       pch.sel,col.sel,cex.sel,
                                       pch.del,col.del) {          # nocov start
  ## Select the scale-bar
  sbPts <- iSelectPt(2,"Select ends of scale-bar:",msg2,
                     pch.sel=pch.sel,col.sel=col.sel,cex.sel=cex.sel,
                     pch.del=pch.del,col.del=col.del,
                     snap2Transect=FALSE,slpTransect=NULL,
                     intTransect=NULL,slpPerpTransect=NULL)
  if (is.data.frame(sbPts)) { # data.frame returned b/c not abort/restarted
    ## Show the user-selected marking on the image
    graphics::lines(y~x,data=sbPts,col=col.scaleBar,lwd=lwd.scaleBar)
    ## Find distances in x- and y- directions,
    ##   corrected for pixel w to h ratio
    distx <- (sbPts$x[2]-sbPts$x[1])*pixW2H
    disty <- sbPts$y[2]-sbPts$y[1]
    ## Return a list (scaling factor is known / distance between points)
    list(sbPts=sbPts,scalingFactor=knownLength/sqrt(distx^2+disty^2))
  } else { # data.frame not returned b/c abort/restarted
    sbPts  # just return the sbPts object
  }
}                                                                  # nocov end


########################################################################
## Places text in txt= on an active plot at a position given in pos=
########################################################################
iPlaceText <- function(txt,pos,cex,col) {                          # nocov start
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
}                                                                  # nocov end


########################################################################
## Uses magic numbers to determine if fn is an RData file as saved
## from digitizeRadii(). The magic numbers are from here ...
## https://www.loc.gov/preservation/digital/formats/fdd/fdd000470.shtml
########################################################################
isRData <- function(fn) {
  magic <- readBin(fn,what=0L,n=2,size=1L,signed=FALSE)
  ifelse(isTRUE(all.equal(magic,c(31,139))),TRUE,FALSE)
}


########################################################################
## Determines if the back-calculation method is a valid choice and
## converts the wordy name to a number
########################################################################
iGetBCMethod <- function(BCM) {
  BCMethodsNms <- c("DALE","FRALE","BI","LBI","BPH","LBPH","TVG","SPH",
                    "LSPH","AE","AESPH","AEBPH","MONA","MONA-BPH",
                    "MONA-SPH","WAKU","FRY","MF","ABI","FRY-BPH","ABPH",
                    "FRY-SPH","ASPH","QBPH","QSPH","PBPH","PSPH","EBPH",
                    "ESPH")
  BCMethodsNums <- c(1,2,3,3,4,4,5,6,6,7,7,8,9,10,11,12,13,14,14,15,15,
                     16,16,17,18,19,20,21,22)
  ## Do some checking
  if (missing(BCM)) STOP("A back-calculation function must be chosen with 'BCM'")
  if (length(BCM)>1) STOP("Only one value may be given to 'BCM'")
  if (is.numeric(BCM)) {
    ## Function declared numerically
    if (BCM<1 | BCM>22) STOP("BCM number must be between 1 and 22 inclusive.")
  } else {
    ## Function declared by name
    BCM <- toupper(BCM)
    if (!(BCM %in% BCMethodsNms)) {
      msg <- paste(strwrap(paste("'BCM' must be one of:",
                                 paste(BCMethodsNms,collapse=", ")),
                           width=62),collapse="\n")
      STOP(msg)
    } else {
      # All is good ... convert string to numeric
      BCM <- BCMethodsNums[which(BCMethodsNms %in% BCM)]
    }
  }  
  BCM
}
