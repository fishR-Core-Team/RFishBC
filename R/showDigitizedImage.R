#' @title Show points selected on a structure image and saved in an R data file
#' 
#' @description Show points selected on a structure image to represent annuli that were saved to an R data file using \code{\link{digitizeRadii}}. This allows the user to reexamine the selected points or overlay selected points from multiple readings of the structure.
#' 
#' @param nms A string (or vector of strings) that indicates the R data file(s) created with \code{\link{digitizeRadii}}. If missing the user will be provided a dialog box from which to choose the file(s). The file(s) must be in the current working directory (see \code{\link{getwd}} result). May also be a single \code{RFishBC} object created with \code{\link{digitizeRadii}}.
#' @param deviceType See details in \code{\link{RFBCoptions}}.
#' @param pch.show See details in \code{\link{RFBCoptions}}.
#' @param col.show See details in \code{\link{RFBCoptions}}.
#' @param cex.show See details in \code{\link{RFBCoptions}}.
#' @param connect See details in \code{\link{RFBCoptions}}.
#' @param col.connect See details in \code{\link{RFBCoptions}}.
#' @param lwd.connect See details in \code{\link{RFBCoptions}}.
#' @param col.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param lwd.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param showScaleBarLength See details in \code{\link{RFBCoptions}}.
#' @param cex.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param showAnnuliLabels See details in \code{\link{RFBCoptions}}.
#' @param annuliLabels See details in \code{\link{RFBCoptions}}.
#' @param col.ann See details in \code{\link{RFBCoptions}}.
#' @param cex.ann See details in \code{\link{RFBCoptions}}.
#' @param offset.ann See details in \code{\link{RFBCoptions}}.
#'
#' @return A list that contains the size of the window in \code{windowSize} and the pixel width to height ratio in \code{pixW2H}. In addition, an image is plotted with, at least, the selected points.
#' 
#' @details This function requires interaction from the user. A detailed description of its use is in \href{https://fishr-core-team.github.io/RFishBC/articles/seeRadiiData.html}{this vignette} on the \href{https://fishr-core-team.github.io/RFishBC/index.html}{RFishBC website}.
#'
#' @seealso \code{\link{digitizeRadii}} and \code{\link{RFBCoptions}}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @export
#'
#' @examples
#' ## None because this requires interaction from the user.
#' ## See the link to the extensive documentation in the Details.
#' 
showDigitizedImage <- function(nms,deviceType,
                               pch.show,col.show,cex.show,
                               connect,col.connect,lwd.connect,
                               col.scaleBar,lwd.scaleBar,
                               showScaleBarLength,cex.scaleBar,
                               showAnnuliLabels,annuliLabels,
                               col.ann,cex.ann,offset.ann) {
  ## handle options
  if (missing(deviceType)) deviceType <- iGetopt("deviceType")
  if (missing(pch.show)) pch.show <- iGetopt("pch.show")
  useArrows <- FALSE
  if (is.character(pch.show)) {
    if (length(pch.show)==1) {
      if (pch.show=="arrows") useArrows <- TRUE
    } else {
      if (any(pch.show=="arrows"))
        STOP("'arrows' cannot be used with other values in 'pch.show'.")
    }
  }
  if (missing(col.show)) col.show <- iGetopt("col.show")
  if (missing(cex.show)) cex.show <- iGetopt("cex.show")
  if (missing(connect)) connect <- iGetopt("connect")
  if (missing(col.connect)) col.connect <- iGetopt("col.connect")
  if (missing(lwd.connect)) lwd.connect <- iGetopt("lwd.connect")
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (length(col.scaleBar)>1) STOP("Can use only one color in 'col.scaleBar='.")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")
  if (length(lwd.scaleBar)>1) STOP("Can use only one value in 'lwd.scaleBar='.")
  if (missing(showScaleBarLength)) showScaleBarLength <- iGetopt("showScaleBarLength")
  if (missing(cex.scaleBar)) cex.scaleBar <- iGetopt("cex.scaleBar")
  if (missing(showAnnuliLabels)) showAnnuliLabels <- iGetopt("showAnnuliLabels")
  if (missing(annuliLabels)) annuliLabels <- iGetopt("annuliLabels")
  if (!showAnnuliLabels) if (!is.null(annuliLabels))
    STOP("'annuliLabels' not needed when 'showAnnuliLabels=FALSE'")
  if (missing(col.ann)) col.ann <- iGetopt("col.ann")
  if (missing(cex.ann)) cex.ann <- iGetopt("cex.ann")
  if (missing(offset.ann)) {
    # if using arrows and user did not set value then use slightly larger
    # offset.ann because arrows are larger relative to other pch.shows
    if (!useArrows) offset.ann <- iGetopt("offset.ann")
    else offset.ann <- 0.8
  }
  dat <- NULL # try to avoid "no visible binding" note
  
  ## Get image file names ######################################################
  ## If nms is missing then allow the user to choose a file or files
  if (missing(nms)) nms <- iHndlFilenames(nms,filter="RData",multi=TRUE) # nocov start
  ## If nms is an RFishBC object (and not a filename) then extract the 
  ##   filename otherwise process the filename(s)
  if (inherits(nms,"RFishBC")) nms <- nms$datanm                         # nocov end
    else nms <- iHndlFilenames(nms,filter="RData",multi=TRUE)
  
  ## Get number of readings ####################################################
  num2do <- length(nms)

  ## Show the reading(s) ... handle one and multiple readings differently ######
  if (num2do==1) {
    ### Make sure file is an RData file from digitizeRadii()
    if (!isRData(nms)) 
      STOP(nms," is not an RData file saved from 'digitizeRadii().")
    dat <- readRDS(nms)
    if (!inherits(dat,"RFishBC")) 
      STOP(nms," does not appear to be from 'digitizeRadii().")
    img <- iShowOneDigitizedImage(dat,deviceType,
                                  showScaleBarLength,col.scaleBar,
                                  lwd.scaleBar,cex.scaleBar,
                                  connect,col.connect,lwd.connect,
                                  useArrows,pch.show,col.show,cex.show,
                                  showAnnuliLabels,annuliLabels,
                                  col.ann,cex.ann,offset.ann)
  } else {
    tmp <- NULL
    for (i in seq_along(nms)) {
      ### Make sure each file is an RData file from digitizeRadii()
      if (!isRData(nms[i]))
        STOP(nms[i]," is not an RData file saved from 'digitizeRadii().")
      dat <- readRDS(nms[i])
      if (!inherits(dat,"RFishBC"))
        STOP(nms[i]," does not appear to be from 'digitizeRadii().")
      if (!is.null(tmp)) {
        if (dat$image!=tmp) {
          grDevices::dev.off()
          STOP("Files appear to derive from different structure images.")
        }
      }
      tmp <- dat$image
      
      ### expand pchs, colors, cexs, lwds to number of readings/transects
      pch.show <- rep(pch.show,ceiling(num2do/length(pch.show)))
      col.show <- rep(col.show,ceiling(num2do/length(col.show)))
      cex.show <- rep(cex.show,ceiling(num2do/length(cex.show)))
      col.connect <- rep(col.connect,ceiling(num2do/length(col.connect)))
      lwd.connect <- rep(lwd.connect,ceiling(num2do/length(lwd.connect)))
      
      ### Make or add to image
      if (i==1) 
        img <- iShowOneDigitizedImage(dat,deviceType,
                                      showScaleBarLength,col.scaleBar,
                                      lwd.scaleBar,cex.scaleBar,
                                      connect,col.connect[i],lwd.connect[i],
                                      useArrows,pch.show[i],col.show[i],cex.show[i],
                                      showAnnuliLabels=FALSE,annuliLabels="",
                                      col.ann[i],cex.ann[i],offset.ann[i])
      else {                                                       # nocov start
        ## Show connected points if asked to do so
        if (connect) graphics::lines(y~x,data=dat$pts,
                                     lwd=lwd.connect[i],col=col.connect[i],
                                     ljoin=1)
        ## Show points (only show points that were considered annuli)
        if (useArrows) {
          pos <- iFindLabelPos(dat)
          lbl <- intToUtf8(c(9650,9658,9660,9668)[pos])
          lbl <- Encoding(lbl)
          graphics::text(y~x,data=dat$pts[2:(nrow(dat$pts)-1),],labels=lbl,
                         col=col.show[i],cex=cex.show[i],pos=pos,offset=0)
          if (dat$edgeIsAnnulus)
            graphics::text(y~x,data=dat$pts[nrow(dat$pts),],labels=lbl,
                           col=col.show[i],cex=cex.show[i],
                           pos=pos,offset=0)
          
        } else {
          graphics::points(dat$pts[2:(nrow(dat$pts)-1),],
                           pch=pch.show[i],col=col.show[i],cex=cex.show[i])
          if (dat$edgeIsAnnulus)
            graphics::points(dat$pts[nrow(dat$pts),],
                             pch=pch.show[i],col=col.show[i],cex=cex.show[i])
        }
      }                                                            # nocov end
    }
  }
  invisible(img)
}

########################################################################
## Show the first digitized image.
########################################################################
iShowOneDigitizedImage <- function(dat,deviceType,
                                   showScaleBarLength,col.scaleBar,
                                   lwd.scaleBar,cex.scaleBar,
                                   connect,col.connect,lwd.connect,
                                   useArrows,pch.show,col.show,cex.show,
                                   showAnnuliLabels,annuliLabels,
                                   col.ann,cex.ann,offset.ann) {
  ## Get and display the image
  img <- iGetImage(dat$image,id=NULL,
                   windowSize=dat$windowSize,deviceType=deviceType,
                   showInfo=FALSE,pos.info=NULL,cex.info=NULL,col.info=NULL)
  ## Show scale-bar, if it was digitized, and scale-bar length if asked for
  if (!is.null(dat$sbPts)) {
    graphics::lines(y~x,data=dat$sbPts,col=col.scaleBar,lwd=lwd.scaleBar)
    if (showScaleBarLength) iShowScaleBarLength(dat,col.scaleBar,cex.scaleBar)
  }
  ## Connected points with a line if asked to do so
  if (connect) graphics::lines(y~x,data=dat$pts,
                               col=col.connect,lwd=lwd.connect,ljoin=1)
  ## Show points (only show points that were considered annuli)
  num.ann <- ifelse(dat$edgeIsAnnulus,nrow(dat$pts)-1,nrow(dat$pts)-2)
  if (length(col.show)>1 & length(col.show)<num.ann)
    WARN("'col.show' was recycled")
  if (length(cex.show)>1 & length(cex.show)<num.ann)
    WARN("'cex.show' was recycled")
  if (useArrows) {
    pos <- iFindLabelPos(dat)
    lbl <- intToUtf8(c(9650,9658,9660,9668)[pos])
    lbl <- Encoding(lbl)
    graphics::text(y~x,data=dat$pts[2:(nrow(dat$pts)-1),],labels=lbl,
                   col=col.show,cex=cex.show,pos=pos,offset=0)
    if (dat$edgeIsAnnulus)
      graphics::text(y~x,data=dat$pts[nrow(dat$pts),],labels=lbl,
                     col=col.show[length(col.show)],cex=cex.show[length(cex.show)],
                     pos=pos,offset=0)
  } else {
    if (length(pch.show)>1 & length(pch.show)<num.ann)
      WARN("'pch.show' was recycled")
    graphics::points(dat$pts[2:(nrow(dat$pts)-1),],pch=pch.show,col=col.show,cex=cex.show)
    if (dat$edgeIsAnnulus)
      graphics::points(dat$pts[nrow(dat$pts),],pch=pch.show[length(pch.show)],
                       col=col.show[length(col.show)],cex=cex.show[length(cex.show)])
  }
  #### Show annuli labels if asked to do so
  if (showAnnuliLabels)
    iShowAnnuliLabels(dat,annuliLabels=annuliLabels,
                      col.ann=col.ann,cex.ann=cex.ann,offset.ann=offset.ann)
  invisible(img)
}



########################################################################
## Show annuli numbers on the showDigitizedImage() image
########################################################################
iShowAnnuliLabels <- function(dat,annuliLabels,
                              col.ann,cex.ann,offset.ann) { # nocov start
  ## Get points to plot
  pts <- dat$pts
  ## Finda annuli list
  #### Use all annuli if annuliLabels not supplied by user
  if (is.null(annuliLabels)) annuliLabels <- 1:max(dat$radii$agecap)
  #### Make sure provided annuliLabels exist in the data
  annuliLabels <- annuliLabels[annuliLabels %in% rownames(pts)]
  #### Get just the points to be labelled
  pts <- pts[rownames(pts) %in% annuliLabels,]
  ## Check colors and cexs
  if (length(col.ann)>1 & length(col.ann)<length(annuliLabels))
    WARN("'col.ann' was recycled.")
  if (length(cex.ann)>1 & length(cex.ann)<length(annuliLabels))
    WARN("'cex.ann' was recycled.")
  ## Find a position relative to the point for the text based on transect slope
  pos <- iFindLabelPos(dat)
  ## put the labels on the plot
  graphics::text(y~x,data=pts,labels=annuliLabels,font=2,
                 col=col.ann,cex=cex.ann,pos=pos,offset=offset.ann)
}                                                                  # nocov end



########################################################################
## Find a position for the "arrow" or the annulus label based on the
##   slope of the transect or the first and last selected points.
########################################################################
iFindLabelPos <- function(dat) {
  ## Get slope of transect if used, otherwise find rough value from 1st/ last pts
  if (!is.null(dat$slpTransect)) slp <- dat$slpTransect
  else {
    tmp <- dat$pts[c(1,nrow(dat$pts)),]
    slp <- diff(tmp$y)/diff(tmp$x)
  }
  ## Convert slope to degrees
  deg <- atan(slp)*180/pi
  ## Adjust for the quadrant in which the transect is in
  if (dat$pts$x[nrow(dat$pts)]<dat$pts$x[1]) deg <- deg+180
  ## Convert absolute transect degrees into a position for the text/arrow
  deg <- abs(deg)
  if (deg>=0 & deg<=45) pos <- 1L        # below
  else if (deg>45 & deg<=90) pos <- 4L   # right
  else if (deg>90 & deg<=135) pos <- 2L  # left
  else if (deg>135 & deg<=180) pos <- 1L # below
  else if (deg>180 & deg<=225) pos <- 1L # below
  else if (deg>225 & deg<=270) pos <- 4L # right
  else if (deg>270 & deg<=315) pos <- 2L # left
  else if (deg>315 & deg<=360) pos <- 1L # below
  ## Return the position
  pos
}



########################################################################
## Show known length of scale-bar on the showDigitizedImage() image
########################################################################
iShowScaleBarLength <- function(dat,col.scaleBar,cex.scaleBar) {   # nocov start
  ## Get the scale-bar points, length, and units
  pts <- dat$sbPts
  len <- dat$sbLength
  if (!"sbUnits" %in% names(dat)) {
    ### Allows use with data created before sbUnits was added (v0.2.1)
    units <- "units"
  } else {
    ### Handles after sbUnits was added
    units <- ifelse(is.null(dat$sbUnits),"units",dat$sbUnits)
  }
  ### Change "units" to "unit" if the scale-bar length is 1
  if (units=="units" & isTRUE(all.equal(len,1))) units <- "unit"
  ## Find the degree of angle for the scale-bar slope
  deg <- atan(diff(pts$y)/diff(pts$x))*180/pi
  ## Convert absolute transect degrees into a position for the text
  pos <- ifelse(deg>=0 & deg<=45,1,4)
  ## Create a label
  lbl <- paste(len,units,sep=" ")
  ## Place text at the midpoint of the transect
  graphics::text(mean(pts$x),mean(pts$y),label=lbl,pos=pos,
                 col=col.scaleBar,cex=cex.scaleBar)
}                                                                  # nocov end
