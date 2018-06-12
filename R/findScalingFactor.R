#' @title Find scaling factor from object of known length
#' 
#' @description This computes a scaling factor (i.e., a multiplier that is used to convert image lengths to actual lengths) from user selected endpoints of an object of known length on the image given in \code{img} and the actual known length given in \code{knownLength}. See Details.
#' 
#' @param img A string that indicates the image (must be PNG, JPG, BMP, or TIFF) to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory).
#' @param knownLength See details in \code{\link{RFBCoptions}}.
#' @param sepWindow See details in \code{\link{RFBCoptions}}.
#' @param windowSize See details in \code{\link{RFBCoptions}}.
#' @param closeWindow See details in \code{\link{RFBCoptions}}.
#' @param col.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param lwd.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param pch.sel See details in \code{\link{RFBCoptions}}.
#' @param col.sel See details in \code{\link{RFBCoptions}}.
#' @param cex.sel See details in \code{\link{RFBCoptions}}.
#' @param pch.del See details in \code{\link{RFBCoptions}}.
#' @param col.del See details in \code{\link{RFBCoptions}}.
#' 
#' @details To apply the scaling factor determined with this function to images opened in \code{\link{digitizeRadii}} is is important that the images were created with the EXACT same magnification, are saved with the EXACT same dimensions (and aspect ratio), and the EXACT same value for \code{windowSize=} is used.
#'
#' @return A single numeric that is the scaling factor (a multiplier that is used to convert image lengths to actual lengths).
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet

findScalingFactor <- function(img,knownLength,
                              sepWindow,windowSize,closeWindow,
                              col.scaleBar,lwd.scaleBar,
                              pch.sel,col.sel,cex.sel,
                              pch.del,col.del) {
  ## Handle options
  if (missing(knownLength)) STOP("Must provide a 'knownLength'.")
  if (knownLength<=0) STOP("'knownLength' must be positive.")
  if (missing(sepWindow)) sepWindow <- iGetopt("sepWindow")
  if (missing(windowSize)) windowSize <- iGetopt("windowSize")
  if (missing(closeWindow)) closeWindow <- iGetopt("closeWindow")
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")
  if (missing(pch.sel)) pch.sel <- iGetopt("pch.sel")
  if (missing(col.sel)) col.sel <- iGetopt("col.sel")
  if (missing(cex.sel)) cex.sel <- iGetopt("cex.sel")
  if (missing(pch.del)) pch.del <- iGetopt("pch.del")
  if (missing(col.del)) col.del <- iGetopt("col.del")
  
  ## Handle the filename
  img <- iHndlFilenames(img,filter="images",multi=FALSE)
  
  ## Read the image
  windowInfo <- iGetImage(img,id=NULL,sepWindow=sepWindow,
                          windowSize=windowSize,showInfo=FALSE,
                          pos.info=NULL,cex.info=NULL,col.info=NULL)
  NOTE("Select the endpoints of the scale-bar.")
  msg2 <- "     Press 'f' when finished, 'd' to delete selection."
  SF <- iScalingFactorFromScaleBar(msg2,knownLength,windowInfo$pixW2H,
                                   col.scaleBar=col.scaleBar,
                                   lwd.scaleBar=lwd.scaleBar,
                                   pch.sel,col.sel,cex.sel,
                                   pch.del,col.del)
  if (sepWindow & closeWindow) grDevices::dev.off()
  SF$scalingFactor
}

