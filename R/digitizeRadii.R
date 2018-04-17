#' @title Collect radial measurements from a calcified structure by interactively selecting annuli.
#' 
#' @description Computes radial measurements from selected points and writes all to an R data object
#' 
#' The user can interactively select points on a plot by clicking with the first mouse button on the image. When finished, the x- and y-coordinates for each selected point will be returned.
#' 
#' @param fname A string that indicates the image (must be PNG, JPG, or BMP) to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
#' @param id A unique identifier for the fish or structure being examined. Will be coerced to a character.
#' @param reading See details in \code{\link{RFBCoptions}}.
#' @param description See details in \code{\link{RFBCoptions}}.
#' @param suffix See details in \code{\link{RFBCoptions}}.
#' @param edgeIsAnnulus See details in \code{\link{RFBCoptions}}.
#' @param sepWindow See details in \code{\link{RFBCoptions}}.
#' @param windowSize See details in \code{\link{RFBCoptions}}.
#' @param scaleBar See details in \code{\link{RFBCoptions}}.
#' @param scaleBarLength See details in \code{\link{RFBCoptions}}.
#' @param col.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param lwd.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param scalingFactor See details in \code{\link{RFBCoptions}}.
#' @param addTransect See details in \code{\link{RFBCoptions}}.
#' @param snap2Transect See details in \code{\link{RFBCoptions}}.
#' @param col.transect See details in \code{\link{RFBCoptions}}.
#' @param lwd.transect See details in \code{\link{RFBCoptions}}.
#' @param pch.sel See details in \code{\link{RFBCoptions}}.
#' @param col.sel See details in \code{\link{RFBCoptions}}.
#' @param cex.sel See details in \code{\link{RFBCoptions}}.
#'
#' @details This uses \code{\link[graphics]{locator}} which will suspend activity in the console until the user indicates that they are done selecting points. The user indicates that they are done selecting points by right-clicking on the image and selecting Stop or selecting Stop in the window's menu.
#'
#' @return A string that contains the filename (with path) of an R data object (i.e., .RData file) written to the same directory/folder as the file in \code{fname} that contains the following items in a list:
#' \itemize{
#'   \item description: The description given in \code{description}.
#'   \item id: The unique identifier given in \code{id}.
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
digitizeRadii <- function(fname,id,reading,suffix,
                          description,edgeIsAnnulus,
                          sepWindow,windowSize,scaleBar,
                          scaleBarLength,col.scaleBar,lwd.scaleBar,
                          scalingFactor,addTransect,snap2Transect,
                          col.transect,lwd.transect,
                          pch.sel,col.sel,cex.sel) {
  ## Process argument defaults
  fname <- iHndlfname(fname)
  id <- iHndlID(id)
  if (missing(reading)) reading <- iGetopt("reading")
  if (missing(description)) description <- iGetopt("description")
  if (missing(suffix)) suffix <- iGetopt("suffix")
  if (is.null(suffix) & !is.null(reading)) suffix <- reading
  if (missing(edgeIsAnnulus)) edgeIsAnnulus <- iGetopt("edgeIsAnnulus")
  if (!is.logical(edgeIsAnnulus)) STOP("'edgeIsAnnulus' must be TRUE or FALSE")
  if (missing(scaleBar)) scaleBar <- iGetopt("scaleBar")
  if (missing(scaleBarLength)) scaleBarLength <- iGetopt("scaleBarLength")
  if (scaleBar & is.null(scaleBarLength)) STOP("Must provide a 'scaleBarLength' when 'scaleBar=TRUE'.")
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")
  if (missing(scalingFactor)) scalingFactor <- iGetopt("scalingFactor")
  if (missing(addTransect)) addTransect<- iGetopt("addTransect")
  if (missing(snap2Transect)) snap2Transect<- iGetopt("snap2Transect")
  if (missing(col.transect)) col.transect <- iGetopt("col.transect")
  if (missing(lwd.transect)) lwd.transect <- iGetopt("lwd.transect")
  if (missing(pch.sel)) pch.sel <- iGetopt("pch.sel")
  if (missing(col.sel)) col.sel <- iGetopt("col.sel")
  if (missing(cex.sel)) cex.sel <- iGetopt("cex.sel")
  if (missing(sepWindow)) sepWindow <- iGetopt("sepWindow")
  if (missing(windowSize)) windowSize <- iGetopt("windowSize")

  ## Loads image given in fname
  windowSize <- iReadImage(fname,sepWindow,windowSize)
  message("1. Loaded the ",fname," image.")
  ## Allows the user to select a scaling bar to get a scaling factor
  SF <- iHndlScalingFactor(scaleBar,scaleBarLength,scalingFactor,
                           col.scaleBar,lwd.scaleBar)
  ## User selects annuli on the image
  pts <- iSelectAnnuli(pch.pts=pch.sel,col.pts=col.sel,
                       cex.pts=cex.sel,addTransect=addTransect,
                       col.trans=col.transect,lwd.trans=lwd.transect)
  ## If asked, "snap" the selected points to the transect
  pts <- iSnap2Transect(pts)
  ## Converts the selected points to radial measurements
  dat <- iProcessAnnuli(fname,pts,id,reading,suffix,description,
                        edgeIsAnnulus,SF$scalingFactor)
  ## Add windowSize and scaling factor information to dat list
  dat <- c(dat,SF,windowSize)
  ## Write the dat object to R object filename in the working directory.
  save(dat,file=dat$datobj)
  message("4. All results written to ",dat$datobj)
  ## Invisibly return the R object
  invisible(dat)
}
