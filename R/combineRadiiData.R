#' @title Combines radii data from multiple files into one data.frame
#' 
#' @description Combines the radii data that were created with \code{processPoints} and saved to an R object file from multiple R object files into one data.frame. This data.frame can then be post-processed.
#'  
#' @param fnames A character vector of file names (without the path) from which the radii data should be extracted and combined to form one synthetic data.frame. The files should all be in one folder/directory as given in \code{path}.
#' @param path A string that contains the full path name for the folder/directory for which to list files. Defaults to the current working directory (see \code{\link{getwd}} result).
#'
#' @details None yet.
#' 
#' @return A data.frame that contains the radii data created with \code{processPoints} for all files given in \code{fnames}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples 
#' ## None yet
#' 
combineRadiiData <- function(fnames,path='.') {
  ## Some checks
  if (length(path)!=1) stop("'path' can take only one string.",
                            call.=FALSE)
  ## Append path to file names
  if (path==".") path <- getwd()
  fnames <- file.path(path,fnames)
  ## Start the resultant data.frame with the data from the first file
  dat <- NULL # try to avoid "no visile binding" note
  load(fnames[1])
  d <- dat$radii
  ## Now rbind to this with data from the remaining files, if more than
  ## one file was given in fnms
  if (length(fnames)>1) {
    for (i in 2:length(fnames)) {
      load(fnames[i])
      d <- rbind(d,dat$radii)
    }
  }
  ## Return the result
  d
}
