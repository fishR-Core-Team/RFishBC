#' @title Collect radial measurements from a calcified structure by interactively selecting annuli.
#' 
#' @description Computes radial measurements from selected points and writes all to an R data object
#' 
#' The user can interactively select points on a plot by clicking with the first mouse button on the image. When finished, the x- and y-coordinates for each selected point will be returned.
#' 
#' @param fname A string that indicates the image (must be PNG, JPG, or BMP) to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
#' @param ID A unique identifier for the fish or structure being examined. Will be coerced to a character.
#' @param reading Identifies the reading for this structure. Typically this can simply be set to \sQuote{1} if the structure will not be read multiple times. If the structure is read multiple times then this should be used to identify which time (which read) this one is. Will be coerced to a character.
#' @param description A string that possibly contains a short description for the read of this structure.
#' @param suffix A string that will be added to the R object data file (see Details). This defaults to the same string (possibly coerced) in \code{reading}.
#' @param addTransect A logical that indicates whether the user will be prompted to add a linear transect to the structure image by selecting a point at the focus and a point on the margin of the structure. Adding a transect is not required, but may be useful when identifying annuli on the structure.
#' @param col.transect The color of the transect line if \code{addTransect=TRUE}.
#' @param lwd.transect The line width of the transect line if \code{addTransect=TRUE}.
#' @param lty.transect The line type of the transect line if \code{addTransect=TRUE}.
#' @param useScaleBar XXX
#' @param magnification XXX
#' @param pch.annuli The plotting character of points for selected annuli.
#' @param col.annuli The color of points for selected annuli.
#' @param cex.annuli The character expansion value of points for selected annuli.
#' @param edgeIsAnnulus A logical that indicates whether the point at the structure margin should be considered as an annulus (\code{TRUE}) or not (\code{FALSE}). Use \code{FALSE} if the last selected point represents an incomplete year's worth of growth (i.e., \sQuote{plus-growth}).
#'
#' @details This uses \code{\link[graphics]{locator}} which will suspend activity in the console until the user indicates that they are done selecting points. The user indicates that they are done selecting points by pressing the ESCape key or right-clicking on the image and selecting Stop in Windows, pressing any mouse button other than the first (left) in X11 devices, and pressing the ESCape key in Quartz or OS X devices.
#'
#' @return A string that contains the filename (with path) of an R data object (i.e., .RData file) written to the same directory/folder as the file in \code{fname} that contains the following items in a list:
#' \itemize{
#'   \item description: The description given in \code{description}.
#'   \item ID: The unique identifier given in \code{ID}.
#'   \item image: The filename for the structure image given in \code{fname}.
#'   \item pts: The data.frame of x- and y-coordinates for the points (as returned directly from selecting points on the structure image).
#'   \item radii: A data.frame that contains the radial measurements (i.e., from the focus to the selected point) for the structure. Specifically, it contains the following items:
#'   \itemize{
#'     \item XXX
#'   }
#' }.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}.
#' 
#' @export
#'
#' @examples
#' ## None yet
#' 
collectRadiiData <- function(fname=file.choose(),ID,reading,
                             suffix=reading,description=NULL,
                             magnification=NULL,useScaleBar=FALSE,
                             addTransect=TRUE,col.transect="yellow",
                             lwd.transect=2,lty.transect=1,
                             pch.annuli=3,col.annuli="red",
                             cex.annuli=1.25,edgeIsAnnulus=FALSE) {
  ## Some checks
  if (missing(ID)) stop("You must enter a unique identifier in 'ID'.",
                        call.=FALSE)
  if (missing(reading)) stop("You must enter an identifier in 'reading'.",
                             call.=FALSE)
  ## Loads image given in fname
  fn <- iReadImage(fname)
  ## Allows the user to select a scaling bar to get a magnification

  ## Allows user to add a transect to the image if desired
  if (addTransect) iAddTransect(col=col.transect,lwd=lwd.transect,
                                lty=lty.transect)
  ## User selects annuli on the image
  pts <- iSelectAnnuli(pch=pch.annuli,col=col.annuli,cex=cex.annuli)
  ## Converts the selected points to radial measurements and writes out
  ## an R object to the working directory. R object filename is returned.
  fn2 <- iProcessAnnuli(fn,pts,ID,reading,suffix,
                        description,edgeIsAnnulus)
  ## Invisibly return the filename
  invisible(fn2)
}