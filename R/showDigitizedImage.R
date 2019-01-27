#' @title Show points selected on a structure image and saved in an R data file
#' 
#' @description Show points selected on a structure image to represent annuli that were saved to an R data file using \code{\link{digitizeRadii}}. This allows the user to reexaminine the selected points or overlay selected points from multiple readings of the structure.
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
#'
#' @return None, but an image is plotted with, at least, the selected points.
#' 
#' @details This function requires interaction from the user. A detailed description of its use is in \href{http://derekogle.com/RFishBC/articles/MeasureRadii/seeRadiiData.html}{this vignette} on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}.
#'
#' @seealso \code{\link{digitizeRadii}} and \code{\link{RFBCoptions}}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
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
                               showAnnuliLabels,annuliLabels,col.ann,cex.ann) {
  ## handle options
  if (missing(deviceType)) deviceType <- iGetopt("deviceType")
  if (missing(pch.show)) pch.show <- iGetopt("pch.show")
  if (missing(col.show)) col.show <- iGetopt("col.show")
  if (missing(cex.show)) cex.show <- iGetopt("cex.show")
  if (missing(connect)) connect <- iGetopt("connect")
  if (missing(col.connect)) col.connect <- iGetopt("col.connect")
  if (length(col.connect)>1) STOP("Can use only one color in 'col.connect='.")
  if (missing(lwd.connect)) lwd.connect <- iGetopt("lwd.connect")
  if (length(lwd.connect)>1) STOP("Can use only one value in 'lwd.connect='.")
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
  dat <- NULL # try to avoid "no visible binding" note
  
  ## Get image file names ######################################################
  ## If nms is missing then allow the user to choose a file or files
  if (missing(nms)) nms <- iHndlFilenames(nms,filter="RData",multi=TRUE) # nocov
  ## If nms is an RFishBC object (and not a filename) then extract the 
  ##   filename otherwise process the filename(s)
  if (inherits(nms,"RFishBC")) nms <- nms$datanm                         # nocov
    else nms <- iHndlFilenames(nms,filter="RData",multi=TRUE)
  ## Prepare for multiple readings #############################################
  # check that all were created with digitizeRadii() and from same image
  tmp <- NULL
  for (i in seq_along(nms)) {
    if (!isRData(nms[i]))
      STOP("File is not an RData file saved from 'digitizeRadii().")
    dat <- readRDS(nms[i])
    if (!inherits(dat,"RFishBC"))
      STOP("File does not appear to be from 'digitizeRadii().")
    tmp <- c(tmp,dat$image)
  }
  if (length(unique(tmp))>1)
    STOP("Files appear to derive from different structure images.")
  # expand pchs, colors, cexs, lwds to number of transects
  num2do <- length(nms)
  pch.show <- rep(pch.show,ceiling(num2do/length(pch.show)))
  col.show <- rep(col.show,ceiling(num2do/length(col.show)))
  cex.show <- rep(cex.show,ceiling(num2do/length(cex.show)))
  col.connect <- rep(col.connect,ceiling(num2do/length(col.connect)))
  lwd.connect <- rep(lwd.connect,ceiling(num2do/length(lwd.connect)))

  ## Display results ###########################################################
  for (i in seq_along(nms)) {
    dat <- readRDS(nms[i])
    #### If first then show the image
    if (i==1) {
      iGetImage(dat$image,id=NULL,
                windowSize=dat$windowSize,deviceType=deviceType,
                showInfo=FALSE,pos.info=NULL,cex.info=NULL,col.info=NULL)
      origImage <- dat$image
      #### Show scale-bar, if it was digitized, and scale-bar length if asked
      #### for (but only for the first reading)
      if (!is.null(dat$sbPts)) {
        graphics::lines(y~x,data=dat$sbPts,col=col.scaleBar,lwd=lwd.scaleBar)
        if (showScaleBarLength) iShowScaleBarLength(dat,col.scaleBar,cex.scaleBar)
      }
    }
    #### Show connected points if asked to do so
    if (connect) graphics::lines(y~x,data=dat$pts,
                                 lwd=lwd.connect[i],col=col.connect[i])
    #### Show points (only show points that were considered annuli)
    graphics::points(dat$pts[2:(nrow(dat$pts)-1),],
                     pch=pch.show[i],col=col.show[i],cex=cex.show[i])
    if (dat$edgeIsAnnulus) ## add on edge if considered an annulus
      graphics::points(dat$pts[nrow(dat$pts),],
                       pch=pch.show[i],col=col.show[i],cex=cex.show[i])
    #### Show annuli labels if asked to do so AND only if one set of readings
    if (num2do==1) {
      #### Show annuli labels if asked to do so
      if (showAnnuliLabels) iShowAnnuliLabels(dat,annuliLabels=annuliLabels,
                                              col.ann=col.ann,cex.ann=cex.ann)
    }
  }
}




########################################################################
## Show annuli numbers on the showDigitizedImage() image
########################################################################
iShowAnnuliLabels <- function(dat,annuliLabels,col.ann,cex.ann) { # nocov start
  ## Get points to plot
  pts <- dat$pts
  
  ## Find the degree of angle for the transect slope
  ### If given the use it, otherwise find rough value from first and last points
  if (!is.null(dat$slpTransect)) deg <- atan(dat$slpTransect)*180/pi
  else {
    tmp <- dat$pts[c(1,nrow(dat$pts)),]
    deg <- atan(diff(tmp$y)/diff(tmp$x))*180/pi
  }
  #### Adjust for the quadrant in which the transect is in
  if (pts$x[nrow(pts)]<pts$x[1]) deg <- deg+180
  ## Convert absolute transect degrees into a position for the text
  deg <- abs(deg)
  if (deg>=0 & deg<=45) pos <- 1        # below
  else if (deg>45 & deg<=90) pos <- 4   # right
  else if (deg>90 & deg<=135) pos <- 2  # left
  else if (deg>135 & deg<=180) pos <- 1 # below
  else if (deg>180 & deg<=225) pos <- 1 # below
  else if (deg>225 & deg<=270) pos <- 4 # right
  else if (deg>270 & deg<=315) pos <- 2 # left
  else if (deg>315 & deg<=360) pos <- 1 # below
  
  ## Put on text
  #### Use all annuli if annuliLabels not supplied by user
  if (is.null(annuliLabels)) annuliLabels <- 1:max(dat$radii$agecap)
  #### Make sure provided annuliLabels exist in the data
  annuliLabels <- annuliLabels[annuliLabels %in% rownames(pts)]
  #### Get just the points to be labelled
  pts <- pts[rownames(pts) %in% annuliLabels,]
  ## Check colors
  if (length(col.ann)>1 & length(col.ann)<length(annuliLabels))
    WARN("Colors in 'col.ann' will be recylced.")
  ## Check cexs
  if (length(cex.ann)>1 & length(cex.ann)<length(annuliLabels))
    WARN("Values in 'cex.ann' will be recylced.")
  #### put the labels on the plot
  graphics::text(y~x,data=pts,labels=annuliLabels,font=2,
                 col=col.ann,cex=cex.ann,pos=pos)
} # nocov end

########################################################################
## Show known length of scale-bar on the showDigitizedImage() image
########################################################################
iShowScaleBarLength <- function(dat,col.scaleBar,cex.scaleBar) { # nocov start
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
} # nocov end
