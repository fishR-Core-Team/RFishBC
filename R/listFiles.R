#' @title List files with a specific extension in a folder/directory
#' 
#' @description This returns a vector with all file names with the \code{ext} extension in the \code{path} folder/directory. In \pkg{RfishBC} this is used primarily to create a list of image file names for use in \code{\link{digitizeRadii}} or RData file names created with \code{\link{digitizeRadii}} and to be given to \code{\link{combineData}}. 
#' 
#' @param ext A single string that contains the file extension pattern to match.
#' @param other Other strings to match in file names that were already matched by the extension in \code{ext}.
#' @param path A single string that contains the full path name for the folder/directory for which to list files. Defaults to the current working directory (see \code{\link{getwd}} result).
#' @param ignore.case A logical for whether pattern matching should be case sensitive (\code{=FALSE}) or not (\code{TRUE}; DEFAULT).
#' @param \dots Parameters to be given to \code{\link{list.files}}.
#' 
#' @seealso \code{\link{digitizeRadii}} and \code{\link{combineData}}; and \code{\link{list.files}} in base R.
#'
#' @details An example of using this function is in \href{https://fishr-core-team.github.io/RFishBC/articles/collectRadiiData.html}{this vignette} and \href{https://fishr-core-team.github.io/RFishBC/articles/seeRadiiData.html}{this vignette} on the \href{https://fishr-core-team.github.io/RFishBC/index.html}{RFishBC website}.
#'
#' @return Character vector.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## See the link to the extensive documentation in the Details.
#' 
listFiles <- function(ext,other=NULL,path=".",ignore.case=TRUE,...) {
  ## Some checks
  if (length(path)!=1) STOP("'path' can take only one string.")
  if (length(ext)!=1) STOP("'ext' can take only one string.")
  ## Add a dot to the extension if one does not exist
  if (!grepl("\\.",ext)) ext <- paste0(".",ext)
  ## Get the list of files in path that have the ext extension
  tmp <- list.files(path=path,pattern=paste0("\\",ext,"$"),
                    ignore.case=ignore.case,...)
  if (length(tmp)<1) STOP("No files have a ",ext," extension.")
  ## Potentially reduce that list to those that match strings in other
  if (!is.null(other)) {
    for (i in seq_along(other)) tmp <- tmp[grepl(other[i],tmp,ignore.case=ignore.case)]
    if (length(tmp)<1) STOP("No files with ",ext," extension contain the patterns given in 'other'.")
    tmp <- unique(tmp)
  }
  tmp
}
