#' @title Add a transect to an image by selecting two points.
#' 
#' @description Add a transect to an image by selecting a point at the focus and a point at the edge of the calcified structure. This is not required for any analyses, but it is useful for providing a landmark for identifying annuli with \code{selectPoints}.
#' 
#' @param col.transect The color of the transect line.
#' @param lwd.transect The line width of the transect line.
#' @param lty.transect The line type of the transect line.
#'
#' @return A list of x- and y- coordinates for the two selected points.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet
#' 
addTransect <- function(col.transect="blue",lwd.transect=1,
                         lty.transect=1) {
  invisible(graphics::locator(n=2,type="l",col=col.transect,
                              lwd=lwd.transect,lty=lty.transect))
}
