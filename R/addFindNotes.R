#' @title Add a note to an existing RData file
#' 
#' @description Use this to add a note to a saved R data file that was created with \code{\link{digitizeRadii}}. Can be used to flag the reading for some reason.
#'  
#' @param nms A string that indicates the R data file created with \code{\link{digitizeRadii}} (in the current directory) to which the note should be added.
#' @param note The note (as a string) to be added to the R data file. If missing the user will be provided a dialog box in which to enter the note.
#'
#' @return None, but the \code{note} object in the R Data file(s) given in \code{nms} will be modified.
#' 
#' @details A detailed description of its use is in the "Other Features" vignette on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples 
#' ## See the link to the extensive documentation in the Details.
#' 

addNote <- function(nms,note) {
  ## Get the RDS file to add note to
  if (missing(nms)) nms <- iHndlFilenames(nms,filter="RData") # nocov end
  if (!isRData(nms)) 
    STOP(nms," is not an RData file saved from 'digitizeRadii().")
  dat <- readRDS(nms)
  if (!inherits(dat,"RFishBC")) 
    STOP(nms," does not appear to be from 'digitizeRadii().")
  
  ## Add the note and resave the RDS file
  dat$note <- ifelse(missing(note),iGetNote(note),note)
  saveRDS(dat,file=nms)
}


#' @title Returns the notes from the chosen R Data files
#' 
#' @description Returns the notes from the chosen R data files created with \code{\link{digitizeRadii}}.
#'  
#' @param nms A string (or vector of strings) that indicates the R data file(s) created with \code{\link{digitizeRadii}}. If missing the user will be provided a dialog box from which to choose the file(s). The file(s) must be in the current working directory (see \code{\link{getwd}} result). May also be a single \code{RFishBC} object created with \code{\link{digitizeRadii}}.
#'
#' @return A data.frame that contains the \code{id}, \code{reading}, and \code{notes} from the chosen R Data files.
#' 
#' @details A detailed description of its use is in the "Other Features" vignette on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples 
#' ## See the link to the extensive documentation in the Details.
#' 
findNotes <- function(nms) {
  if (inherits(nms,"RFishBC")) {
    tmp <- ifelse("note" %in% names(nms),nms$note,"")
    d <- cbind(nms$radii[1,c("id","reading")],note=tmp)
  } else {
    ## Row-bind radii data.frames from dat object loaded from the RData files
    d <- NULL
    for (i in seq_along(nms)) {
      if (!isRData(nms[i]))
        STOP("File is not an RData file saved from 'digitizeRadii().")
      dat <- readRDS(nms[i])
      if (!inherits(dat,"RFishBC"))
        STOP("File does not appear to be from 'digitizeRadii().")
      tmp <- ifelse("note" %in% names(dat),dat$note,"")
      d <- rbind(d,cbind(dat$radii[1,c("id","reading")],note=tmp))
    }
  }
  as.data.frame(d)
}


## =============================================================================
iGetNote <- function(x) {
  msg <- "Enter note about image: "
  if (grepl('w|W', .Platform$OS.type)) {
    ## we are on Windows ... use a windows dialog box
    x <- utils::winDialogString(msg,ifelse(missing(x),"",x))
  } else {
    ## Not on Windows ... use prompt in console if in interactive session
    if (interactive()) x <- readline(prompt=msg)
  }
  x
}
