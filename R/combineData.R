#' @title Combines radii data from multiple files into one data.frame
#' 
#' @description Combines radial measurements made on calcified structures and saved to an R data file with \code{\link{digitizeRadii}} into a single data.frame that can then be post-processed (e.g., back-calculate length at a previous age).
#'  
#' @param nms A character vector of R data file names created with \code{\link{digitizeRadii}}. The files must be in the current working directory (see \code{\link{getwd}} result).
#' @param outFormat A string that indicates the output format for the combined data. The \code{"wide"} (DEFAULT) format has one-radius-per-line (i.e., each radial measurement for a fish in on a separate row), whereas the \code{"long"} format has one-fish-per-line (i.e., each radial measurement for a fish is in a separate column).
#' @param deletePlusGrowth A logical that indicates whether the radial measurement that corresponds to \dQuote{plus-growth} should be deleted from the returned data.frame (\code{TRUE}; DEFAULT) or not (\code{FALSE}).
#'
#' @details A detailed description of its use is in \href{http://derekogle.com/RFishBC/articles/MeasureRadii/collectRadiiData.html}{this vignette} on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}. The list of R data file names may be efficiently created with \code{\link{listFiles}} as described in that vignette. The R data file names may also be selected from a dialog box if using Windows.
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

combineData <- function(nms,outFormat=c("long","wide"),deletePlusGrowth=TRUE) {
  outFormat <- match.arg(outFormat)
  d <- NULL
  ann <- rad <- NULL  ## Trying to avoid no visible binding note
  ## Get files
  nms <- iHndlFilenames(nms,filter="RData",multi=TRUE)
  ## Row-bind radii data.frames from dat object loaded from the RData files
  for (i in seq_along(nms)) {
    if (!isRData(nms[i])) STOP("File is not an RData file saved from 'digitizeRadii().")
    dat <- readRDS(nms[i])
    if (!inherits(dat,"RFishBC")) STOP("File does not appear to be from 'digitizeRadii().")
    d <- rbind(d,dat$radii)
  }
  ## Remove radial measurement related to plus-growth (same as radcap anyways)
  if (deletePlusGrowth) d <- d[d$ann<=d$agecap,]
  ## Convert to wide (one-fish-per-line) format
  if (outFormat=="wide") {
    d <- tidyr::spread(d,key=ann,value=rad,sep="rad")
    ## Remove "ann" from variable names
    names(d) <- gsub("ann","",names(d))
  }
  d
}
