#' @title Interactively select points on an image.
#' 
#' @description The user can interactively select points on a plot by clicking with the first mouse button on the image. When finished, the x- and y-coordinates for each selected point will be returned.
#' 
#' @param pch Plotting character for the selected points.
#' @param col.pts Color for the selected points.
#' @param cex.pts Character expansion factor for the selected points.
#'
#' @details This uses \code{\link[graphics]{locator}} which will suspend activity in the console until the user indicates that they are done selecting points. The user indicates that they are done selecting points by pressing the ESCape key or right-clicking on the image and selecting Stop in Windows, pressing any mouse button other than the first (left) in X11 devices, and pressing the ESCape key in Quartz or OS X devices.
#'
#' @return A list of x- and y-coordinates for the selected points.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet
#' 
selectPoints <- function(pch=3,col.pts="red",cex.pts=1.25) {
  message("Remember to select the focus of the structure first.\n\n")
  message("Remember to indicated that you are done selecting points\nwhen you have selected the structure edge.\nFinish selecting points by pressing the ESCape key in Windows or OS X.\n")
  tmp <- as.data.frame(graphics::locator(type="p",pch=pch,
                       col=col.pts,cex=cex.pts))
  message(nrow(tmp)," points were selected.\n")
  tmp
}
