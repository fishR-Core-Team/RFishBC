#' @title Show points selected on a structure image and saved in an R data file
#' 
#' @description Show points selected on a structure image to represent annuli that were saved to an R data file using \code{\link{digitizeRadii}}. This allows the user to reexaminine the selected points or overlay selected points from multiple readings of the structure.
#' 
#' @param nm A string that indicates the R data file created with \code{\link{digitizeRadii}}. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file. Either way the file must be in the current working directory.
#' @param sepWindow See details in \code{\link{RFBCoptions}}.
#' @param pch.show See details in \code{\link{RFBCoptions}}.
#' @param col.show See details in \code{\link{RFBCoptions}}.
#' @param cex.show See details in \code{\link{RFBCoptions}}.
#' @param showTransect See details in \code{\link{RFBCoptions}}.
#' @param col.transect See details in \code{\link{RFBCoptions}}.
#' @param lwd.transect See details in \code{\link{RFBCoptions}}.
#' @param col.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param lwd.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param showAnnuliLabels See details in \code{\link{RFBCoptions}}.
#' @param annuliLabels See details in \code{\link{RFBCoptions}}.
#' @param col.ann See details in \code{\link{RFBCoptions}}.
#' @param cex.ann See details in \code{\link{RFBCoptions}}.
#'
#' @details This function requires interaction from the user. A detailed description of its use is in \href{http://derekogle.com/RFishBC/articles/MeasureRadii/seeRadiiData.html}{this vignette} on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}.
#'
#' @seealso \code{\link{digitizeRadii}} and \code{\link{RFBCoptions}}.
#' 
#' @return None, but an image is plotted with, at least, the selected points.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None because this requires interaction from the user.
#' ## See the link to the extensive documentation in the Details.
#' 
showDigitizedImage <- function(nm,sepWindow,
                               pch.show,col.show,cex.show,
                               showTransect,col.transect,lwd.transect,
                               col.scaleBar,lwd.scaleBar,
                               showAnnuliLabels,annuliLabels,col.ann,cex.ann) {
  ## handle options
  if (missing(sepWindow)) sepWindow <- iGetopt("sepWindow")
  if (missing(pch.show)) pch.show <- iGetopt("pch.show")
  if (missing(col.show)) col.show <- iGetopt("col.show")
  if (missing(cex.show)) cex.show <- iGetopt("cex.show")
  if (missing(showTransect)) showTransect <- iGetopt("showTransect")
  if (missing(col.transect)) col.transect <- iGetopt("col.transect")
  if (missing(lwd.transect)) lwd.transect <- iGetopt("lwd.transect")
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")
  if (missing(showAnnuliLabels)) showAnnuliLabels <- iGetopt("showAnnuliLabels")
  if (missing(annuliLabels)) annuliLabels <- iGetopt("annuliLabels")
  if (!showAnnuliLabels) if (!is.null(annuliLabels))
    STOP("'annuliLabels' not needed when 'showAnnuliLabels=FALSE'")
  if (missing(col.ann)) col.ann <- iGetopt("col.ann")
  if (missing(cex.ann)) cex.ann <- iGetopt("cex.ann")
  dat <- NULL # try to avoid "no visible binding" note
  
  ## Get image file names ######################################################
  nm <- iHndlFilenames(nm,filter="RData",multi=TRUE)
  ## Prepare for multiple readings #############################################
  num2do <- length(nm)
  # expand pchs, colors, cexs, lwds to number of transects
  pch.show <- rep(pch.show,ceiling(num2do/length(pch.show)))
  col.show <- rep(col.show,ceiling(num2do/length(col.show)))
  cex.show <- rep(cex.show,ceiling(num2do/length(cex.show)))
  col.transect <- rep(col.transect,ceiling(num2do/length(col.transect)))
  lwd.transect <- rep(lwd.transect,ceiling(num2do/length(lwd.transect)))

  ## Display results ###########################################################
  for (i in seq_along(nm)) {
    if (!isRData(nm[i]))
      STOP("File is not an RData file saved from 'digitizeRadii().")
    dat <- readRDS(nm[i])
    if (!inherits(dat,"RFishBC"))
      STOP("File does not appear to be from 'digitizeRadii().")
    #### If first then show the image
    if (i==1) {
      iGetImage(dat$image,id=NULL,sepWindow=sepWindow,
                windowSize=dat$windowSize,showInfo=FALSE,
                pos.info=NULL,cex.info=NULL,col.info=NULL)
      origImage <- dat$image
    }
    if (origImage!=dat$image)
      STOP("Files appear to derive from different structure images.")
    #### Show transect if asked ... assumes that the focus is in the first row
    #### and the margin is in the last row (should be from digitizeRadii)
    if (showTransect) graphics::lines(y~x,data=dat$pts[c(1,nrow(dat$pts)),],
                                      lwd=lwd.transect[i],col=col.transect[i])
    #### Show scale-bar, if it was digitized
    if (!is.null(dat$sbPts)) graphics::lines(y~x,data=dat$sbPts,
                                             col=col.scaleBar,lwd=lwd.scaleBar)
    #### Show points
    graphics::points(dat$pts,pch=pch.show[i],col=col.show[i],cex=cex.show[i])
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
  else if (deg>135 & deg<=180) pos <- 1 # below
  
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

