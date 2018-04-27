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
                               col.scaleBar,lwd.scaleBar) {
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
  fnames <- iHndlFilename(nm)
  dat <- NULL # try to avoid "no visible binding" note
  # Must handle filenames different if one or multiple are given
  if (is.list(fnames)) {
    ## Only one image will be shown
    ## Load the data object
    load(fnames$givennm)
    ## Show first image
    iReadImage(dat$image,NULL,sepWindow,dat$windowSize,FALSE,NULL,NULL,NULL)
  } else {
    ## One image with multiple points will be shown
    ## Load the data object
    load(fnames[1])
    ## Show first image
    iReadImage(dat$image,NULL,sepWindow,dat$windowSize,FALSE,NULL,NULL,NULL)
  }
  ## Show the putative transect ... assumes that the focus and margin
  ## are in the first two rows of dat$pts (as they should be)
  if (showTransect) 
    graphics::lines(dat$pts[1:2,],
                    lwd=lwd.transect[1],col=col.transect[1])
  ## Show points
  graphics::points(dat$pts,
                   pch=pch.show[1],col=col.show[1],cex=cex.show[1])
  ## Show scale-bar, if it was digitized
  if (!is.null(dat$sbPts)) {
    graphics::lines(y~x,data=dat$sbPts,col=col.scaleBar,lwd=lwd.scaleBar)
  }
  ## Add other results
  if (!is.list(fnames) & length(fnames)>1) {
    num <- length(fnames)
    # expand colors
    pch.show <- rep(pch.show,ceiling(num/length(pch.show)))
    col.show <- rep(col.show,ceiling(num/length(col.show)))
    cex.show <- rep(cex.show,ceiling(num/length(cex.show)))
    col.transect <- rep(col.transect,ceiling(num/length(col.transect)))
    lwd.transect <- rep(lwd.transect,ceiling(num/length(lwd.transect)))
    for (i in 2:num) {
      load(fnames[i])
      if (showTransect) 
        graphics::lines(dat$pts[1:2,],lwd=lwd.transect[i],
                        col=col.transect[i])
      graphics::points(dat$pts,pch=pch.show[i],col=col.show[i],
                       cex=cex.show[i])
    }
  }
}
