#' @title Combines radii data from multiple files into one data.frame
#' 
#' @description Combines radial measurements made on calcified structures and saved to an RData file with \code{\link{digitizeRadii}} into a single data.frame that can then be post-processed (e.g., back-calculate length at a previous age).
#'  
#' @param nms A character vector of RData file names created with \code{\link{digitizeRadii}}. The files must be in the current working directory (see \code{\link{getwd}} result).
#'
#' @details A detailed description of its use is in \href{http://derekogle.com/RFishBC/articles/MeasureRadii/collectRadiiData.html}{this vignette} on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}. The list of RData file names may be efficiently created with \code{\link{listFiles}} as described in that vignette. The RData file names may also be selected from a dialog box if using Windows.
#' 
#' @return A data.frame that contains the radii data created with \code{\link{digitizeRadii}} for all files given in \code{nms}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples 
#' ## See the link to the extensive documentation in the Details.
#' 

combineData <- function(nms) {
  d <- dat <- NULL # try to avoid "no visible binding" note
  ## Get files
  nms <- iHndlFilenames(nms,filter="RData",multi=TRUE)
  ## Row-bind radii data.frames from dat object loaded from the RData files
  for (i in seq_along(nms)) {
    if (!isRData(nms[i])) STOP("File is not an RData file saved from 'digitizeRadii().")
    dat <- readRDS(nms[i])
    d <- rbind(d,dat$radii)
  }
  d
}
