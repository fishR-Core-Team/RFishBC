#' @title Combines radii data from multiple files into
#' 
#' @description Combines radial measurements made on calcified structures and saved to an RData file with \code{\link{digitizeRadii}} into a single data.frame can then be post-processed (e.g., back-calculate length at a previous age).
#'  
#' @param nms A character vector of RData file names (without the path) created with  \code{\link{digitizeRadii}} from which the radii data will be extracted and combined to form one synthetic data.frame. The files should all be in one folder/directory as given in \code{path}.
#' @param path A string that contains the full path name for the folder/directory the files in \code{nms}. Defaults to the current working directory (see \code{\link{getwd}} result).
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

combineData <- function(nms,path='.') {
  ## Some checks
  if (length(path)!=1) STOP("'path' can take only one string.")
  ## Append path to file names
  if (path==".") path <- getwd()
  nms <- file.path(path,nms)
  ## Start the resultant data.frame with the data from the first file
  dat <- NULL # try to avoid "no visible binding" note
  load(nms[1])
  d <- dat$radii
  ## Now rbind to this with data from the remaining files, if more than
  ## one file was given in fnms
  if (length(nms)>1) {
    for (i in 2:length(nms)) {
      load(nms[i])
      d <- rbind(d,dat$radii)
    }
  }
  ## Return the result
  d
}
