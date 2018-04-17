#' @title Show the points that were selected at some previous time and saved to the R object file.
#' 
#' @description Show the points that were selected at some previous time on the structure and save to the R object file. This is useful for reexamining the selected points at a later time or overlaying selected points from multiple reads of the structure.
#' 
#' @param fname A string that indicates the R object file to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
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
#' @details None yet
#'
#' @return None, but an image is plotted with the selected points, and possibly a putative transect, overlaid.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet
#' 
showDigitizedImage <- function(fname,sepWindow,
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
  
  ## Load the data object
  fname <- iHndlfname(fname)
  dat <- NULL # try to avoid "no visible binding" note
  load(fname[1])
  ## Show first image
  iReadImage(dat$image,sepWindow,dat$windowSize)
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
  num <- length(fname)
  if (num>1) {
    # expand colors
    pch.show <- rep(pch.show,ceiling(num/length(pch.show)))
    col.show <- rep(col.show,ceiling(num/length(col.show)))
    cex.show <- rep(cex.show,ceiling(num/length(cex.show)))
    col.transect <- rep(col.transect,ceiling(num/length(col.transect)))
    lwd.transect <- rep(lwd.transect,ceiling(num/length(lwd.transect)))
    for (i in 2:num) {
      load(fname[i])
      if (showTransect) 
        graphics::lines(dat$pts[1:2,],lwd=lwd.transect[i],
                        col=col.transect[i])
      graphics::points(dat$pts,pch=pch.show[i],col=col.show[i],
                       cex=cex.show[i])
    }
  }
}
