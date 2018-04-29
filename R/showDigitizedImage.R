#' @title Show points selected on a structure image and saved in an RData file
#' 
#' @description Show points selected on a structure image to represent annuli that were saved to an RData file using \code{\link{digitizeRadii}}. This allows the user to reexaminine the selected points or overlay selected points from multiple readings of the structure.
#' 
#' @param nm A string that indicates the RData file that contains the R object created with  created with \code{\link{digitizeRadii}} (the loaded object will contain the image and the selected points as). By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (file must be in the current working directory unless a fully pathed name is given).
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
#' @param col.ann See details in \code{\link{RFBCoptions}}.
#' @param cex.ann See details in \code{\link{RFBCoptions}}.
#' @param showOrigPts A logical that indicates whether the original user-selected points (i.e., not snapped to the transect) should be shown or not. If the original selections were not snapped to the transect then this will be ignored.
#' @param pch.show2 The plotting character for the original points (i.e., not snapped to the transect) selected by the user. Defaults to same as \code{pch.show}.
#' @param col.show2 The color for the original points (i.e., not snapped to the transect) selected by the user. Defaults to same as \code{pch.show}.
#' @param cex.show2 The character expansion value for the original points (i.e., not snapped to the transect) selected by the user. Defaults to same as \code{pch.show}.

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
                               showAnnuliLabels,col.ann,cex.ann,
                               showOrigPts=FALSE,
                               pch.show2,col.show2,cex.show2) {
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
  if (missing(col.ann)) col.ann <- iGetopt("col.ann")
  if (missing(cex.ann)) cex.ann <- iGetopt("cex.ann")
  if (missing(pch.show2)) pch.show2 <- pch.show
  if (missing(col.show2)) col.show2 <- col.show
  if (missing(cex.show2)) cex.show2 <- cex.show
  fnames <- iHndlFilename(nm)
  dat <- NULL # try to avoid "no visible binding" note
  # Must handle filenames differently if one or multiple are given
  if (is.list(fnames)) {
    ## Only one image will be shown ... load the data object
    load(fnames$givennm)
    num2do <- 1
  } else {
    ## One image with multiple points will be shown ... load first data object
    load(fnames[1])
    num2do <- length(fnames)
  }
  ## Show the image
  iGetImage(dat$image,NULL,sepWindow,dat$windowSize,FALSE,NULL,NULL,NULL)
  ## Show the putative transect ... assumes that the focus is in the first row
  ## and the margin is in the last row (as they should be from digitizeRadii)
  if (showTransect) iShowTransect(dat$pts[c(1,nrow(dat$pts)),],
                                  lwd.transect=lwd.transect[1],
                                  col.transect=col.transect[1])
  ## Show scale-bar, if it was digitized
  if (!is.null(dat$sbPts)) {
    graphics::lines(y~x,data=dat$sbPts,col=col.scaleBar,lwd=lwd.scaleBar)
  }
  ## Show points
  graphics::points(dat$pts,pch=pch.show[1],col=col.show[1],cex=cex.show[1])
  ## Show annuli labels if asked to do so
  if (showAnnuliLabels & num2do==1) iShowAnnuliLabels(dat,col.ann=col.ann,
                                                      cex.ann=cex.ann)
  ## Show original points if asked and if snapped to transect
  if (showOrigPts & dat$snap2Transect)
    graphics::points(dat$orig.pts,pch=pch.show2[1],col=col.show2[1],
                     cex=cex.show2[1])
  ## Add other results
  if (num2do>1) {
    # expand colors
    pch.show <- rep(pch.show,ceiling(num2do/length(pch.show)))
    col.show <- rep(col.show,ceiling(num2do/length(col.show)))
    cex.show <- rep(cex.show,ceiling(num2do/length(cex.show)))
    col.transect <- rep(col.transect,ceiling(num2do/length(col.transect)))
    lwd.transect <- rep(lwd.transect,ceiling(num2do/length(lwd.transect)))
    pch.show2 <- rep(pch.show2,ceiling(num2do/length(pch.show2)))
    col.show2 <- rep(col.show2,ceiling(num2do/length(col.show2)))
    cex.show2 <- rep(cex.show2,ceiling(num2do/length(cex.show2)))
    for (i in 2:num2do) {
      load(fnames[i])
      if (showTransect) iShowTransect(dat$pts[c(1,nrow(dat$pts)),],
                                      lwd.transect=lwd.transect[i],
                                      col.transect=col.transect[i])
      graphics::points(dat$pts,pch=pch.show[i],col=col.show[i],
                       cex=cex.show[i])
      if (showOrigPts & dat$snap2Transect)
        graphics::points(dat$orig.pts,pch=pch.show2[i],col=col.show2[i],
                         cex=cex.show2[i])
    }
  }
}
