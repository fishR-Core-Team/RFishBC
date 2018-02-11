#' @title Show the points that were selected at some previous time and saved to the R object file.
#' 
#' @description Show the points that were selected at some previous time on the structure and save to the R object file. This is useful for reexamining the selected points at a later time or overlaying selected points from multiple reads of the structure (i.e., using \code{add=TRUE}).
#' 
#' @param fname A string that indicates the R object file to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
#' @param pch Plotting character for the selected points.
#' @param col.pts Color for the selected points.
#' @param cex.pts Character expansion factor for the selected points.
#' @param col.transect The color of the transect line.
#' @param lwd.transect The line width of the transect line.
#' @param lty.transect The line type of the transect line.
#' @param show.transect A logical that indicates whether a putative transect line should be drawn (\code{=TRUE}; DEFAULT) or not (\code{=FALSE}). This transect line is NOT the transect found with \code{addTransect}; rather it simply connects the first and last points.
#' @param add A logical that indicates whether the the transect and points should be added to the active image. Generally, use \code{add=FALSE} when plotting the points for the first time. Use \code{add=TRUE} to create an image that has the transects and points from multiple reads of the same image.
#'
#' @details None yet
#'
#' @return None, but an image is plotted with the selected points, and possibly a putative transect, overlaid.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet
#' 
showPoints <- function(fname=file.choose(),
                       pch=3,col.pts="red",cex.pts=1.25,
                       col.transect="blue",lwd.transect=1,
                       lty.transect=1,show.transect=TRUE,
                       add=FALSE) {
  ## Load the data object
  dat <- NULL # try to avoid "no visile binding" note
  load(fname)
  ## Show image
  if (!add) readImage(dat$image)
  ## Show the putative transect
  if (show.transect) 
    graphics::lines(dat$pts[c(1,nrow(dat$pts)),],
                    col=col.transect,lwd=lwd.transect,lty=lty.transect)
  ## Show points
  graphics::points(dat$pts,pch=pch,col=col.pts,cex=cex.pts)
}
