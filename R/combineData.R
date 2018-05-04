#' @title Combines radii data from multiple files into
#' 
#' @description Combines radial measurements made on calcified structures and saved to an RData file with \code{\link{digitizeRadii}} into a single data.frame can then be post-processed (e.g., back-calculate length at a previous age).
#'  
#' @param nms A character vector of RData file names that were created with  \code{\link{digitizeRadii}}. The files must be in the current working directory (see \code{\link{getwd}} result).
#'
#' @details A detailed description of its use is in \href{http://derekogle.com/RFishBC/articles/MeasureRadii/collectRadiiData.html}{this vignette} on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}. The list of RData file names may be efficiently created with \code{\link{listFiles}} as described in that vignette.
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
  ## Get files
  nms <- iHndlFilenames(nms,filter="RData",multi=TRUE)
  ## Row-bind together the radii data.frames from teh dat object loaded
  ## from the RData files
  d <- dat <- NULL # try to avoid "no visible binding" note
  for (i in seq_along(nms)) {
    load(nms[i])
    if (!"radii" %in% names(dat)) STOP("RData file does not contain a 'radii' object.")
    d <- rbind(d,dat$radii)
  }
  d
}
