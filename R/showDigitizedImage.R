#' @title Show the points that were selected at some previous time and saved to the R object file.
#' 
#' @description Show the points that were selected at some previous time on the structure and save to the R object file. This is useful for reexamining the selected points at a later time or overlaying selected points from multiple reads of the structure (i.e., using \code{add=TRUE}).
#' 
#' @param fname A string that indicates the R object file to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
#' @param pch.pts Plotting character for the selected points.
#' @param col.pts Color for the selected points.
#' @param cex.pts Character expansion factor for the selected points.
#' @param showTransect A logical that indicates whether a putative transect line should be drawn (\code{=TRUE}; DEFAULT) or not (\code{=FALSE}). This transect line is NOT the transect found with \code{addTransect}; rather it simply connects the first and last points.
#' @param col.transect The color of the transect line.
#' @param lwd.transect The line width of the transect line.
#' @param add A logical that indicates whether the the transect and points should be added to the active image. Generally, use \code{add=FALSE} when plotting the points for the first time. Use \code{add=TRUE} to create an image that has the transects and points from multiple reads of the same image.
#' @param sepWindow A logical that indicates whether the structure image should be opened in a new separate window (\code{=TRUE}) or not (\code{=FALSE}).
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
showDigitizedImage <- function(fname=file.choose(),
                               pch.pts=3,col.pts="red",cex.pts=1,
                               showTransect=TRUE,col.transect="yellow",
                               lwd.transect=2,
                               add=FALSE,sepWindow=FALSE) {
  ## Load the data object
  dat <- NULL # try to avoid "no visible binding" note
  load(fname)
  ## Show image
  if (!add) iReadImage(dat$image,sepWindow,ID=dat$radii$ID[1],
                       reading=NULL,description=NULL)
  ## Show the putative transect ... assumes that the focuse and margin
  ## are in the first two rows of dat$pts (as they should be)
  if (showTransect) 
    graphics::lines(dat$pts[1:2,],lwd=lwd.transect,col=col.transect)
  ## Show points
  graphics::points(dat$pts,pch=pch.pts,col=col.pts,cex=cex.pts)
}
