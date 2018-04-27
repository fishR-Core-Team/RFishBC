#' @title Internal functions.
#'
#' @description Internal functions.
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach STOP WARN iHndlfname iHndlID iHndlScalingFactor iReadImage iProcessAnnuli iSelectAnulli iSnap2Transect


################################################################################
# same as stop() and warning() but with call.=FALSE as default
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
  msg <- paste0("## RFishBC v",vers,". See vignettes.\n")
  packageStartupMessage(msg)
}


########################################################################
## Allows the user to choose a filename if none is given.
## If the file is chosen, then the filename (basename) and the directory
##   for the file (dirname) is returned.
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
## Allows the user to enter a fish ID.
########################################################################
iHndlID <- function(id,fname,popID) {
  if (missing(id)) {
    if (grepl('w|W', .Platform$OS.type)) {
      ## we are on Windows
      ## use basename in fname as the default if popID=TRUE
      defID <- ifelse(popID,tools::file_path_sans_ext(fname$bn),"")
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
## Compute a scaling factor from a scale bar on the structure image.
########################################################################
iHndlScalingFactor <- function(scaleBar,knownLength,scalingFactor,
                               col,lwd,pixW2H) {
  if (scaleBar) {
    ## scaleBar is on the plot
    message("\n** Find scaling factor from scale bar.\n",
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
    message("\n** Using the 'scalingFactor' provided.")
    SF <- list(sfSource="Provided",sbPts=NULL,sbLength=NULL,
               scalingFactor=scalingFactor)
  }
  SF
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


########################################################################
## Loads and plots any of the three bitmapped types of images as chosen
## by the user. This uses readbitmap::read.bitmap() so that any of PNG,
## JPG, or BMP files will be automatically detected. This function is
## nearly exactly the unexported digitize::ReadImg(), except that I
## added the code for the dialog box for choosing the file and the use
## of withr.
########################################################################
iReadImage <- function(fname,id,sepWindow,windowSize,
                       showInfo,pos.info,cex.info,col.info) {
  img <- readbitmap::read.bitmap(fname)
  ## Get window size so image displayed in its native aspect ratio.
  ## Only needed if windowSize contains one value.
  if (length(windowSize)==1) {
    cf <- dim(img)[2:1]
    windowSize <- windowSize*cf/max(cf) 
  }
  if (sepWindow) {
    ## Hoping that this forces a new window that is device independent
    ## and does not get trapped into opening in the RStudio Plots pane.
    if (grDevices::dev.cur()==1) { 
      ## no window open and not sending to RStudio
      grDevices::dev.new(rescale="fixed",noRStudioGD=TRUE,
                         width=windowSize[1],height=windowSize[2],
                         title=paste0("Image: ",fname))
    } else if (names(grDevices::dev.cur())=="RStudioGD") {
      ## sending to RStudio, so try to avoid that
      grDevices::dev.new(rescale="fixed",noRStudioGD=TRUE,
                         width=windowSize[1],height=windowSize[2],
                         title=paste0("Image: ",fname))
    } else {
      ## sending to an already open window, close it because it might
      ## not be the correct window size for the images aspect ratio,
      ## and then open a new one
      grDevices::dev.off()
      grDevices::dev.new(rescale="fixed",noRStudioGD=TRUE,
                         width=windowSize[1],height=windowSize[2],
                         title=paste0("Image: ",fname))
    }
  }
  withr::local_par(list(mar=c(0,0,0,0),xaxs="i",yaxs="i"))
  graphics::plot.new()
  graphics::rasterImage(img,0,0,1,1)
  ## Add ID information if told to do so
  if (showInfo) iPlaceText(paste0("ID=",id),pos.info,cex=cex.info,col=col.info)
  ## Return information
  invisible(list(windowSize=windowSize,pixW2H=windowSize[1]/windowSize[2]))
}


########################################################################
## Processes the point locations to radial measurements, computes
## an age-at-capture, and radius-at-capture. Combines all structure-
## related information into a data.frame. Combines all process-related
## and structure-related information into a list and writes that
## information into an R object data file in the current working
## directory.
########################################################################
iProcessAnnuli <- function(nms,pts,id,reading,suffix,description,
                           edgeIsAnnulus,scalingFactor,pixW2H) {
  ## Convert point locations to radial measurements, 
  n <- nrow(pts)-1
  ## Find distances in x- and y- directions, corrected for pixel w to h ratio
  distx <- (pts$x[2:(n+1)]-pts$x[1])*pixW2H
  disty <- pts$y[2:(n+1)]-pts$y[1]
  ## Find distances between points
  distxy <- sqrt(distx^2+disty^2)
  ## Correct distances for scalingFactor ... and call a radius
  rad <- distxy*scalingFactor
  ## Sort the radii to be in increasing order (allows user to select
  ##   them in any order)
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
              pts=pts,radii=radii)
  dat
}


########################################################################
## Allows the user to interactively select points on a plot by clicking
## with the first mouse button on the image. When finished, the x- and
## y-coordinates for each selected point will be returned.
########################################################################
iSelectAnnuli <- function(pch.pts,col.pts,cex.pts,
                          addTransect,col.trans,lwd.trans) {
  message("\n** Select transect endpoints.\n",
          "   * MUST select the focus of the structure FIRST.\n",
          "   * MUST select structure margin SECOND.")
  tmp1 <- as.data.frame(graphics::locator(n=2,type="p",pch=pch.pts,
                                          col=col.pts,cex=cex.pts))
  if (nrow(tmp1)<2) STOP("Either the focus or margin was not selected.")
  if (addTransect) graphics::lines(y~x,data=tmp1,
                                   lwd=lwd.trans,col=col.trans)
  message("\n** Select points that are annuli.\n",
          "   * When finished selecting points press\n",
          "       the second(right) mouse button and select 'Stop',\n",
          "       the 'Stop' button in Windows, or\n",
          "       the 'Finish' button in RStudio.")
  tmp2 <- as.data.frame(graphics::locator(type="p",pch=pch.pts,
                                          col=col.pts,cex=cex.pts))
  if (nrow(tmp2)<1) STOP("No points were selected as annuli.")
  tmp <- rbind(tmp1,tmp2)
  message("   ",nrow(tmp)," points were selected.\n")
  tmp
}


########################################################################
## Will "slide" each point in the perpendicular direction to fall on
## the transect.
########################################################################
iSnap2Transect <- function(pts) {
  ## Isolate the two points that define the transect ... note that these
  ## are in the first two rows.
  transectPts <- pts[1:2,]
  ## Isolate the rest of the points that represent annuli
  annuliPts <- pts[-(1:2),]
  ## Find the slope & intercept of the transect line
  slpTransect <- diff(transectPts$y)/diff(transectPts$x)
  intTransect <- transectPts$y[1]-slpTransect*transectPts$x[1]
  ## Find the equation of the line perpendicular to the transect that
  ## passes through each annulus point.
  slpPerp <- -1/slpTransect
  intsPerp <- annuliPts$y-slpPerp*annuliPts$x
  ## Find the intersection points between the transect line and the
  ## perpendicular line through each annulus
  intersectsX <- (intsPerp-intTransect)/(slpTransect-slpPerp)
  intersectsY <- intTransect+slpTransect*intersectsX
  intersects <- cbind(x=intersectsX,y=intersectsY)
  ## Combine the data.frame of intersection points with the points that
  ## are now on the transect ... effectively moved each point
  ## perpendicularly over until it hit the transect.
  rbind(transectPts,intersects)
}
