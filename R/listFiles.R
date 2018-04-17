#' @title List files with a specific extension in a folder/directory
#' 
#' @description This returns a vector with all file names with the \code{ext} extension in the \code{path} folder/directory. In \pkg{RfishBC} this is used primarily to create a list of image file names over which the user will loop or R data object names that can be given to \code{combineFiles}. 
#' 
#' @param ext A string that contains the file extension pattern to match.
#' @param other Other strings to match int eh file names that match the file extensin in \code{ext}.
#' @param path A string that contains the full path name for the folder/directory for which to list files. Defaults to the current working directory (see \code{\link{getwd}} result).
#' @param ignore.case A logical for whether pattern matching should be case sensitive (\code{=FALSE}) or not (\code{TRUE}; DEFAULT).
#' @param \dots Parameters to be given to \code{\link{list.files}}.
#' 
#' @seealso \code{\link{list.files}} in base R.
#'
#' @details None yet.
#'
#' @return Character vector.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet
#' 
listFiles <- function(ext,other=NULL,path=".",ignore.case=TRUE,...) {
  ## Some checks
  if (length(path)!=1) stop("'path' can take only one string.",
                            call.=FALSE)
  if (length(ext)!=1) stop("'ext' can take only one string.",
                           call.=FALSE)
  ## Add a dot to the extension if one does not exist
  if (!grepl("\\.",ext)) ext <- paste0(".",ext)
  ## Get the list of files in path that have the ext extension
  tmp <- list.files(path=path,pattern=paste0("\\",ext,"$"),
                    ignore.case=ignore.case,...)
  if (length(tmp)<1) stop("No files have a ",ext," extension.",call.=FALSE)
  ## Potentially reduce that list to those that match strings in other
  if (!is.null(other)) {
    for (i in seq_along(other)) {
      if (ignore.case) tmp <- tmp[grepl(tolower(other[i]),tolower(tmp))]
      else tmp <- tmp[grepl(tolower(other[i]),tolower(tmp))]
    }
    if (length(tmp)<1) stop("No files with ",ext," extension contain the patterns given in 'other'.",call.=FALSE)
    tmp <- unique(tmp)
  }
  tmp
}
