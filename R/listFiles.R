#' @title List files with a specific extension in a folder/directory
#' 
#' @description This returns a vector with all file names with the \code{ext} extension in the \code{path} folder/directory. In \pkg{RfishBC} this is used primarily to create a list of image file names over which the user will loop or R data object names that can be given to \code{combineFiles}. 
#' 
#' @param path A string that contains the full path name for the folder/directory for which to list files. Defaults to the current working directory (see \code{\link{getwd}} result).
#' @param ext A string that contains the file extension pattern to match. The string should include the period.
#' @param ignore.case A logical for where pattern matching should be case sensitive (\code{=FALSE}) or not (\code{TRUE}; DEFAULT).
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
listFiles <- function(path=".",ext=".RData",
                      ignore.case=TRUE,...) {
  ## Some checks
  if (length(path)!=1) stop("'path' can take only one string.",
                            call.=FALSE)
  if (length(ext)!=1) stop("'ext' can take only one string.",
                            call.=FALSE)
  ## Add a dot to the extension if one does not exist
  if (!grepl("\\.",ext)) ext <- paste0(".",ext)
  ## Get the list of files in path that have the ext extension
  list.files(path=path,pattern=paste0("\\",ext,"$"),
             ignore.case=ignore.case,...)
}