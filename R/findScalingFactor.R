#' @title Find scaling factor from object of known length
#' 
#' @description This computes a scaling factor (i.e., a multiplier that is used to convert image lengths to actual lengths) from user selected endpoints of an object of known length on the image given in \code{fname} and the actual known length given in \code{knownLength}. 
#' 
#' @param fname A string that indicates the image (must be PNG, JPG, or BMP) to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
#' @param knownLength See details in \code{\link{RFBCoptions}}.
#' @param sepWindow See details in \code{\link{RFBCoptions}}.
#' @param windowSize See details in \code{\link{RFBCoptions}}.
#' @param col.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param lwd.scaleBar See details in \code{\link{RFBCoptions}}.
#' 
#' @details None yet.
#'
#' @return A single numeric that is the scaling factor (a multiplier that is used to convert image lengths to actual lengths).
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet

findScalingFactor <- function(fname,knownLength,
                              sepWindow,windowSize,
                              col.scaleBar,lwd.scaleBar) {
  ## handle options
  fname <- iHndlFilename(fname)
  if (missing(knownLength)) STOP("Must provide a 'knownLength'.")
  if (missing(sepWindow)) sepWindow <- iGetopt("sepWindow")
  if (missing(windowSize)) windowSize <- iGetopt("windowSize")
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")
  ## Read the image
  iGetImage(fname,sepWindow,windowSize)
  SF <- iHndlScalingFactor(TRUE,knownLength,NULL,col.scaleBar,lwd.scaleBar)
  SF$scalingFactor
}

