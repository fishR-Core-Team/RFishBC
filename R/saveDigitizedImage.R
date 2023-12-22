#' @title Save to a file the structure image saved in an R data file with selected points.
#' 
#' @description Save to a file the structure image with points to represent annuli that were saved to an R data file using \code{\link{digitizeRadii}}. This allows the user to create a file of their selections that could be printed.
#' 
#' @param nms A string (or vector of strings) that indicates the R data file(s) created with \code{\link{digitizeRadii}}. If missing the user will be provided a dialog box from which to choose the file(s). The file(s) must be in the current working directory (see \code{\link{getwd}} result). May also be a single \code{RFishBC} object created with \code{\link{digitizeRadii}}.
#' @param fileType Choose file type to be \code{jpeg}, \code{png}, or \code{pdf}.
#' @param suffix A string that will be appended to each saved filename prior to the extension. Defaults to \dQuote{_marked}.
#' @param res Device (for jpeg and png) resolution. Defaults to 72.
#' @param pch.show See details in \code{\link{RFBCoptions}}.
#' @param col.show See details in \code{\link{RFBCoptions}}.
#' @param cex.show See details in \code{\link{RFBCoptions}}.
#' @param connect See details in \code{\link{RFBCoptions}}.
#' @param col.connect See details in \code{\link{RFBCoptions}}.
#' @param lwd.connect See details in \code{\link{RFBCoptions}}.
#' @param col.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param lwd.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param showScaleBarLength See details in \code{\link{RFBCoptions}}.
#' @param cex.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param showAnnuliLabels See details in \code{\link{RFBCoptions}}.
#' @param annuliLabels See details in \code{\link{RFBCoptions}}.
#' @param col.ann See details in \code{\link{RFBCoptions}}.
#' @param cex.ann See details in \code{\link{RFBCoptions}}.
#' @param offset.ann See details in \code{\link{RFBCoptions}}.
#'
#' @return None, but a file is created in the working directory.
#' 
#' @details None.
#'
#' @seealso \code{\link{showDigitizedImage}}, \code{\link{digitizeRadii}}, \code{\link{RFBCoptions}}, and \code{\link{jpeg}}, \code{\link{png}}, and \code{\link{pdf}}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @export
#'
#' @examples
#' ## None because this requires interaction from the user.
#' ## See the link to the extensive documentation in the Details.
#' 
saveDigitizedImage <- function(nms,fileType=c("jpeg","png","pdf"),
                               suffix="_marked",res=72,
                               pch.show,col.show,cex.show,
                               connect,col.connect,lwd.connect,
                               col.scaleBar,lwd.scaleBar,
                               showScaleBarLength,cex.scaleBar,
                               showAnnuliLabels,annuliLabels,
                               col.ann,cex.ann,offset.ann) {
  ## handle options
  fileType <- match.arg(fileType)
  
  ## Get image file names ######################################################
  ## If nms is missing then allow the user to choose a file or files
  if (missing(nms)) nms <- iHndlFilenames(nms,filter="RData",multi=TRUE) # nocov start
  ## If nms is an RFishBC object (and not a filename) then extract the 
  ##   filename otherwise process the filename(s)
  if (inherits(nms,"RFishBC")) nms <- nms$datanm                         # nocov end
    else nms <- iHndlFilenames(nms,filter="RData",multi=TRUE)

  ## Make sure files are from digitizeRadii
  nms <- iCheckFiles(nms)
    
  ## Cycle through files #######################################################
  for (i in nms) {
    # start to make the filename
    nm <- paste0(tools::file_path_sans_ext(i),suffix)
    # display image ...
    d <- showDigitizedImage(i,"default",
                            pch.show,col.show,cex.show,
                            connect,col.connect,lwd.connect,
                            col.scaleBar,lwd.scaleBar,
                            showScaleBarLength,cex.scaleBar,
                            showAnnuliLabels,annuliLabels,
                            col.ann,cex.ann,offset.ann)
    # ... and then send to file
    if (fileType=="jpeg") {
      grDevices::dev.copy(grDevices::jpeg,paste0(nm,".jpg"),
                          width=d$windowSize[1],height=d$windowSize[2],
                          units="in",res=res)
    } else if (fileType=="png") {
      grDevices::dev.copy(grDevices::png,paste0(nm,".png"),
                          width=d$windowSize[1],height=d$windowSize[2],
                          units="in",res=res)
    } else {
      grDevices::dev.copy(grDevices::pdf,paste0(nm,".pdf"),
                          width=d$windowSize[1],height=d$windowSize[2])
    }
    grDevices::dev.off()
  }
  # close device if one is left open
  if (!is.null(grDevices::dev.list())) invisible(grDevices::dev.off())
}
