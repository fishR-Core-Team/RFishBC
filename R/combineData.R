#' @title Combines radii data from multiple files into one data.frame
#' 
#' @description Combines radial measurements made on calcified structures and saved to an R data file with \code{\link{digitizeRadii}} into a single data.frame that can then be post-processed (e.g., back-calculate length at a previous age).
#'  
#' @param nms A string (or vector of strings) that indicates the R data file(s) created with \code{\link{digitizeRadii}}. If missing the user will be provided a dialog box from which to choose the file(s). The file(s) must be in the current working directory (see \code{\link{getwd}} result). May also be a single \code{RFishBC} object created with \code{\link{digitizeRadii}}.
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
  d <- ann <- rad <- NULL  ## Trying to avoid no visible binding note
  ## If nms is missing then allow the user to choose a file
  if (missing(nms)) nms <- iHndlFilenames(nms,filter="RData",multi=TRUE) # nocov
  ## If nms is an RFishBC object (and not a filename) then extract the radii
  ##   otherwise extract the radii from the file(s)
  if (inherits(nms,"RFishBC")) {
    d <- nms$radii                                                       # nocov
  } else {
    ## Row-bind radii data.frames from dat object loaded from the RData files
    for (i in seq_along(nms)) {
      if (!isRData(nms[i]))
        STOP("File is not an RData file saved from 'digitizeRadii().")
      dat <- readRDS(nms[i])
      if (!inherits(dat,"RFishBC"))
        STOP("File does not appear to be from 'digitizeRadii().")
      d <- rbind(d,dat$radii)
    }
  }
  ## Remove radial measurement related to plus-growth (same as radcap anyways)
  if (deletePlusGrowth) {
    ## Get all age-0 fish and include no annulus or radius, but the radcap
    d1 <- d[d$agecap==0,]
    d1$ann <- d1$rad <- NA
    ## Get all >age-0 fish and then remove the plus-growth
    d2 <- d[d$agecap>0,]
    d2 <- d2[d2$ann<=d2$agecap,]
    ## Put the age-0 and >age-0 fish back together, sort by ID and ann
    d <- rbind(d1,d2)
    d <- d[order(d$id,d$ann),]
  }
  ## Convert to wide (one-fish-per-line) format
  if (outFormat=="wide") {
    d <- tidyr::spread(d,key=ann,value=rad,sep="rad")
    ## Remove "ann" from variable names
    names(d) <- gsub("ann","",names(d))
  }
  d
}
