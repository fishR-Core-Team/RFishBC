#' @title Collect radial measurements from a calcified structure by interactively selecting annuli
#' 
#' @description The user interactively select points wit the first moust button on an image of a calcified structure. When finished, radial measurements (from the sturture focus to the selected points) are calculated (either with arbitrary units or actual units if a scale-bar is included on the image) and written to an external file for later use.
#' 
#' @param img A string that indicates the image (must be PNG, JPG, BMP, or TIFF) to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (file must be in the current working directory unless a fully pathed name is given).
#' @param id A unique identifier for the fish or structure being examined. Will be coerced to a character. If missing you will be prompted to enter a value.
#' @param reading See details in \code{\link{RFBCoptions}}.
#' @param description See details in \code{\link{RFBCoptions}}.
#' @param suffix See details in \code{\link{RFBCoptions}}.
#' @param edgeIsAnnulus See details in \code{\link{RFBCoptions}}.
#' @param sepWindow See details in \code{\link{RFBCoptions}}.
#' @param windowSize See details in \code{\link{RFBCoptions}}.
#' @param popID See details in \code{\link{RFBCoptions}}.
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
#' @param showInfo See details in \code{\link{RFBCoptions}}.
#' @param pos.info See details in \code{\link{RFBCoptions}}.
#' @param cex.info See details in \code{\link{RFBCoptions}}.
#' @param col.info See details in \code{\link{RFBCoptions}}.
#'
#' @details This function requires interaction from the user. A detailed description of its use is in the vignettes on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}.
#' 
#' @note This uses \code{\link[graphics]{locator}} which will suspend activity in the console until the user indicates that they are done selecting points by right-clicking on the image and selecting Stop or selecting Stop in the window's menu.
#'
#' @seealso \code{\link{showDigitizedImage}} and \code{\link{RFBCoptions}}.
#'
#' @return A list that contains the following:
#' \itemize{
#'   \item{\code{description}: }{The description given in \code{description}.}
#'   \item{\code{image}: }{The full filename given in \code{img}.}
#'   \item{\code{basenm}: }{The filename given in \code{img} without the path.}
#'   \item{\code{dirnm}: }{The directory in which \code{img} was located.}
#'   \item{\code{datanm}: }{The filename that contains the RData object.}
#'   \item{\code{pts}: }{A data.frame that contains the \code{x} and \code{y} coordinates on the image for the selected annuli.}
#'   \item{\code{radii}: }{A data.frome that contains the unique \code{id}, the \code{reading} code, the age-at-capture in \code{agecap}, the annulus number in \code{ann}, the radial measurements in \code{rad}, and the radial measurement at capture in \code{radcap}.}
#'   \item{\code{sfSource}: }{A character string that identifies whether the scaling factor was \code{"Provided"} through the \code{scalingFactor} argument or derived from a \code{"scaleBar"}.}
#'   \item{\code{sbPts}: }{A data.frame of \code{x} and \code{y} coordinates for the endpoints of the scale-bar if the scaling factor was derived from a scale-bar.}
#'   \item{\code{sbLength}: }{A single numeric that is the known (actual) length of the scale-bar if the scaling factor was derived from a scale-bar.}
#'   \item{\code{scalingFactor}: }{A single numeric used to convert measurements on the structure image to actual measurements on the structure. Measurements on the structure image were multiplied by this value.}
#'   \item{\code{windowSize}: }{A numeric of length two that contains the width and height of the window used to display the structure image. One of these units was set by the given \code{windowSize} value.}
#'   \item{\code{pixW2H}: }{The ratio of pixel width to height. This is used to correct measurements for when an image is not square.}
#' }.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}.
#' 
#' @export
#'
#' @examples
#' ## None because this requires interaction from the user.
#' ## See the link to the extensive documentation in the Details.
#' 
digitizeRadii <- function(img,id,reading,suffix,
                          description,edgeIsAnnulus,popID,
                          sepWindow,windowSize,scaleBar,
                          scaleBarLength,col.scaleBar,lwd.scaleBar,
                          scalingFactor,addTransect,snap2Transect,
                          col.transect,lwd.transect,
                          pch.sel,col.sel,cex.sel,
                          showInfo,pos.info,cex.info,col.info) {
  ## Process argument defaults
  if (missing(reading)) reading <- iGetopt("reading")
  if (missing(description)) description <- iGetopt("description")
  if (missing(suffix)) suffix <- iGetopt("suffix")
  if (is.null(suffix) & !is.null(reading)) suffix <- reading
  if (missing(edgeIsAnnulus)) edgeIsAnnulus <- iGetopt("edgeIsAnnulus")
  if (!is.logical(edgeIsAnnulus)) STOP("'edgeIsAnnulus' must be TRUE or FALSE")
  if (missing(popID)) popID <- iGetopt("popID")
  if (!is.logical(popID)) STOP("'popID' must be TRUE or FALSE")
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
  if (missing(showInfo)) showInfo <- iGetopt("showInfo")
  if (missing(pos.info)) pos.info <- iGetopt("pos.info")
  if (missing(cex.info)) cex.info <- iGetopt("cex.info")
  if (missing(col.info)) col.info <- iGetopt("col.info")
  inames <- iHndlFilename(img)
  id <- iHndlID(id,inames,popID)

  ## Loads image given in img
  windowInfo <- iReadImage(inames$givennm,id,sepWindow,windowSize,
                           showInfo,pos.info,cex.info,col.info)
  message("** Loaded the ",inames$basenm," image.")
  ## Allows the user to select a scaling bar to get a scaling factor
  SF <- iHndlScalingFactor(scaleBar,scaleBarLength,scalingFactor,
                           col.scaleBar,lwd.scaleBar,
                           pixW2H=windowInfo$pixW2H)
  ## User selects annuli on the image
  pts <- iSelectAnnuli(pch.pts=pch.sel,col.pts=col.sel,
                       cex.pts=cex.sel,addTransect=addTransect,
                       col.trans=col.transect,lwd.trans=lwd.transect)
  ## If asked, "snap" the selected points to the transect
  pts <- iSnap2Transect(pts)
  ## Converts the selected points to radial measurements
  dat <- iProcessAnnuli(inames,pts,id,reading,suffix,description,
                        edgeIsAnnulus,SF$scalingFactor,
                        pixW2H=windowInfo$pixW2H)
  ## Add windowSize and scaling factor information to dat list
  dat <- c(dat,SF,windowInfo)
  ## Write the dat object to R object filename in the working directory.
  save(dat,file=paste0(inames$dirnm,"/",dat$datanm))
  message("** All results written to ",dat$datanm)
  ## Invisibly return the R object
  invisible(dat)
}
