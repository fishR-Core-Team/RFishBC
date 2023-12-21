#' @title Allows user to efficiently view images with selected points saved in multiple R data files.
#' 
#' @description Allows user to efficiently view images with points to represent annuli that were saved to multiple R data files using \code{\link{digitizeRadii}}. The user can press keyboard buttons to move between (forward and backward) images.
#' 
#' @param nms A string (or vector of strings) that indicates the R data file(s) created with \code{\link{digitizeRadii}}. If missing the user will be provided a dialog box from which to choose the file(s). The file(s) must be in the current working directory (see \code{\link{getwd}} result). May also be a single \code{RFishBC} object created with \code{\link{digitizeRadii}}.
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
#' @seealso \code{\link{showDigitizedImage}}, \code{\link{saveDigitizedImage}}, \code{\link{digitizeRadii}}, \code{\link{RFBCoptions}}, and \code{\link{jpeg}}, \code{\link{png}}, and \code{\link{pdf}}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @export
#'
#' @examples
#' ## None because this requires interaction from the user.
#' ## See the link to the extensive documentation in the Details.
#' 
reviewDigitizedImages <- function(nms,
                                  pch.show,col.show,cex.show,
                                  connect,col.connect,lwd.connect,
                                  col.scaleBar,lwd.scaleBar,
                                  showScaleBarLength,cex.scaleBar,
                                  showAnnuliLabels,annuliLabels,
                                  col.ann,cex.ann,offset.ann) {

  ## Internal function for handling key press event
  keyPress <- function(key) ifelse(key %in% c("f","q"),"DONE",
                                   ifelse(key %in% c("n","Right","Up"),"Next",
                                          ifelse(key %in% c("p","Left","Down"),"Prev",
                                                 "NONE")))
  
  ## Internal function for showing image
  iReviewDigitizedImage <- function(nm,msg1,
                                    pch.show,col.show,cex.show,
                                    connect,col.connect,lwd.connect,
                                    col.scaleBar,lwd.scaleBar,
                                    showScaleBarLength,cex.scaleBar,
                                    showAnnuliLabels,annuliLabels,
                                    col.ann,cex.ann,offset.ann) {
    cat("Showing ",msg1,".",sep="")
    showDigitizedImage(nm,"default",
                       pch.show,col.show,cex.show,
                       connect,col.connect,lwd.connect,
                       col.scaleBar,lwd.scaleBar,
                       showScaleBarLength,cex.scaleBar,
                       showAnnuliLabels,annuliLabels,
                       col.ann,cex.ann,offset.ann)
  }
  
  # MAIN FUNCTION
  ## Get image file names ######################################################
  ## If nms is missing then allow the user to choose a file or files
  if (missing(nms)) nms <- iHndlFilenames(nms,filter="RData",multi=TRUE) # nocov start
  ## If nms is an RFishBC object (and not a filename) then extract the 
  ##   filename otherwise process the filename(s)
  if (inherits(nms,"RFishBC")) nms <- nms$datanm
    else nms <- iHndlFilenames(nms,filter="RData",multi=TRUE)
  
  ## Setup for the loop ########################################################
  num_imgs <- length(nms)
  i <- 0
  act <- "Next"
  msg2 <- ": 'f'/'q'=DONE, 'n'/'>'=Next, 'p'/'<'=Previous"
  cat("Use",msg2,"\n",sep="")
  
  ## Access images until users says "Done" #####################################
  ### The use if iReviewDigitizedImage() looks redundant below, but doing this
  ###   means that the image will not be redisplayed when trying to go to next
  ###   or previous when next or previous does not exist.
  while (act!="DONE") {
     if (act=="Next") {
      if (i<num_imgs) {
        i <- i+1
        msg1 <- tools::file_path_sans_ext(nms[i])
        iReviewDigitizedImage(nms[i],msg1,
                              pch.show,col.show,cex.show,
                              connect,col.connect,lwd.connect,
                              col.scaleBar,lwd.scaleBar,
                              showScaleBarLength,cex.scaleBar,
                              showAnnuliLabels,annuliLabels,
                              col.ann,cex.ann,offset.ann)
      } else {
        cat("There is no 'Next' image. ")
        i <- i
      }
    } else if (act=="Prev") {
      if (i>1) {
        i <- i-1
        msg1 <- tools::file_path_sans_ext(nms[i])
        iReviewDigitizedImage(nms[i],msg1,
                              pch.show,col.show,cex.show,
                              connect,col.connect,lwd.connect,
                              col.scaleBar,lwd.scaleBar,
                              showScaleBarLength,cex.scaleBar,
                              showAnnuliLabels,annuliLabels,
                              col.ann,cex.ann,offset.ann)
      } else {
        cat("There is no 'Prev'ious image. ")
        i <- i
      }
    } else cat("Must use",msg2,sep="")
    act <- grDevices::getGraphicsEvent(paste0(msg1,msg2),
                                       consolePrompt="",
                                       onKeybd=keyPress,onMouseDown="")
  }
  grDevices::dev.off()
  cat("Done.")                                                       # nocov end
}
