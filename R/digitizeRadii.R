#' @title Collect radial measurements from a calcified structure by interactively selecting annuli
#' 
#' @description The user interactively select points wit the first moust button on an image of a calcified structure. When finished, radial measurements (from the sturture focus to the selected points) are calculated (either with arbitrary units or actual units if a scale-bar is included on the image) and written to an external file for later use.
#' 
#' @param img A string that indicates the image (must be PNG, JPG, BMP, or TIFF) to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file. Either way the file must be in the current working directory.
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
#' @param showTransect See details in \code{\link{RFBCoptions}}.
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
#'   \item{\code{image}: }{The full filename given in \code{img}.}
#'   \item{\code{datanm}: }{The filename that contains the RData object.}
#'   \item{\code{description}: }{The description given in \code{description}.}
#'   \item{\code{edgeIsAnnulus}: }{The logical given in \code{edgeIsAnnulus}.}
#'   \item{\code{snap2Transect}: }{The logical from \code{snap2Transect} that identified whether the selected points were \dQuote{"snapped"} to the transect or not.}
#'   \item{\code{scalingFactor}: }{A single numeric used to convert measurements on the structure image to actual measurements on the structure. Measurements on the structure image were multiplied by this value.}
#'   \item{\code{sfSource}: }{A character string that identifies whether the scaling factor was \code{"Provided"} through the \code{scalingFactor} argument or derived from a \code{"scaleBar"}.}
#'   \item{\code{sbPts}: }{A data.frame of \code{x} and \code{y} coordinates for the endpoints of the scale-bar if the scaling factor was derived from a scale-bar.}
#'   \item{\code{sbLength}: }{A single numeric that is the known (actual) length of the scale-bar if the scaling factor was derived from a scale-bar.}
#'   \item{\code{slpTransect}: }{The slope of the transect.}
#'   \item{\code{intTransect}: }{The intercept of the transect.}
#'   \item{\code{slpPerpTransect}: }{The slope of the line perpendicular to the transect.}
#'   \item{\code{windowSize}: }{A numeric of length two that contains the width and height of the window used to display the structure image. One of these units was set by the given \code{windowSize} value.}
#'   \item{\code{pixW2H}: }{The ratio of pixel width to height. This is used to correct measurements for when an image is not square.}
#'   \item{\code{orig.pts}: }{A data.frame that contains the \dQuote{original} \code{x} and \code{y} coordinates on the image for the selected annuli. If \code{snap2Transect=FALSE} then these are the same as the points in \code{pts}. If \code{snap2Transect=TRUE} then these are the originally selected points and will be different then the points in \code{pts}.}
#'   \item{\code{pts}: }{A data.frame that contains the \code{x} and \code{y} coordinates on the image for the selected annuli. These points may be \dQuote{snapped} to the transect if \code{snap2Transect==TRUE}.}
#'   \item{\code{radii}: }{A data.frome that contains the unique \code{id}, the \code{reading} code, the age-at-capture in \code{agecap}, the annulus number in \code{ann}, the radial measurements in \code{rad}, and the radial measurement at capture in \code{radcap}.}
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
                          scalingFactor,showTransect,snap2Transect,
                          col.transect,lwd.transect,
                          pch.sel,col.sel,cex.sel,
                          showInfo,pos.info,cex.info,col.info) {
  ## Process argument defaults
  if (missing(reading)) reading <- iGetopt("reading")
  if (missing(description)) description <- iGetopt("description")
  if (missing(suffix)) suffix <- iGetopt("suffix")
  if (is.null(suffix) & !is.null(reading)) suffix <- reading
  if (missing(edgeIsAnnulus)) edgeIsAnnulus <- iGetopt("edgeIsAnnulus")
  if (!is.logical(edgeIsAnnulus))
    STOP("'edgeIsAnnulus' must be TRUE or FALSE.")
  if (missing(popID)) popID <- iGetopt("popID")
  if (missing(scaleBar)) scaleBar <- iGetopt("scaleBar")
  if (missing(scaleBarLength)) scaleBarLength <- iGetopt("scaleBarLength")
  if (scaleBar & is.null(scaleBarLength))
    STOP("Must provide a 'scaleBarLength' when 'scaleBar=TRUE'.")
  if (!is.null(scaleBarLength) & !is.numeric(scaleBarLength))
    STOP("'scaleBarLength' must be numeric.")
  if (!is.null(scaleBarLength))
    if (scaleBarLength<=0) STOP("'scaleBarLength' must be positive.")
  if (!scaleBar & !is.null(scaleBarLength)) 
    STOP("Can not use 'scaleBarLength=' with 'scaleBar=FALSE'.")
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")
  if (missing(scalingFactor)) scalingFactor <- iGetopt("scalingFactor")
  if (!is.null(scalingFactor))
    if (!is.numeric(scalingFactor)) STOP("'scalingFactor' must be numeric.")
  if (!is.null(scalingFactor) & scalingFactor<=0)
    STOP("'scalingFactor' must be positive.")
  if (scaleBar & !is.null(scalingFactor)) 
    STOP("Can not use both 'scalingFactor' and 'scaleBar=TRUE'.")
  if (missing(showTransect)) showTransect<- iGetopt("showTransect")
  if (missing(snap2Transect)) snap2Transect<- iGetopt("snap2Transect")
  if (missing(col.transect)) col.transect <- iGetopt("col.transect")
  if (missing(lwd.transect)) lwd.transect <- iGetopt("lwd.transect")
  if (missing(pch.sel)) pch.sel <- iGetopt("pch.sel")
  if (missing(col.sel)) col.sel <- iGetopt("col.sel")
  if (missing(cex.sel)) cex.sel <- iGetopt("cex.sel")
  if (missing(sepWindow)) sepWindow <- iGetopt("sepWindow")
  if (missing(windowSize)) windowSize <- iGetopt("windowSize")
  if (!is.numeric(windowSize)) STOP("'windowSize' must be numeric.")
  if (windowSize<=0) STOP("'windowSize' must be positive.")
  if (missing(showInfo)) showInfo <- iGetopt("showInfo")
  if (missing(pos.info)) pos.info <- iGetopt("pos.info")
  if (missing(cex.info)) cex.info <- iGetopt("cex.info")
  if (missing(col.info)) col.info <- iGetopt("col.info")
  
  ## Handle getting the image filename =========================================
  img <- iHndlFilenames(img,filter="images",multi=FALSE)

  ## Handle the ID =============================================================
  if (missing(id)) {
    if (grepl('w|W', .Platform$OS.type)) {
      ## we are on Windows ... use a windows dialog box
      ## use img name as the default if popID=TRUE
      id <- utils::winDialogString("Enter a unique ID: ",
                                   ifelse(popID,tools::file_path_sans_ext(img),""))
    } else {
      ## Not on Windows ... use prompt in console if in interactive session
      if (interactive()) id <- readline(prompt="Enter a unique ID: ")
    }
    if (missing(id) | is.null(id)) STOP("You must provide a unique ID in 'id'.")
  }
  
  ## Loads image given in img ==================================================
  windowInfo <- iGetImage(img,id,sepWindow,windowSize,
                          showInfo,pos.info,cex.info,col.info)
  message("\n\n** Loaded the ",img," image.")
  
  ## Allows user to select a scaling bar to get a scaling factor ===============
  if (scaleBar) { ## scaleBar is on the plot
    message("\n>> Find scaling factor from scale bar.\n",
            "   * Select endpoints on the scale bar.")
    sfSource <- "scaleBar"
    sbInfo <- iScalingFactorFromScaleBar(scaleBarLength,windowInfo$pixW2H,
                                         col.scaleBar=col.scaleBar,
                                         lwd.scaleBar=lwd.scaleBar)
    sbPts <- sbInfo$sbPts
    scalingFactor <- sbInfo$scalingFactor
  } else { ## No scale bar on the plot ... using the scaling factor
    message("** Using the 'scalingFactor' provided.")
    sbPts <- NULL
    scaleBarLength <- NULL
    sfSource <- "Provided"
  }

  ## User selects a transect on the image ======================================
  message("\n>> Select transect endpoints.\n",
          "   * MUST select the focus of the structure FIRST.\n",
          "   * MUST select structure margin SECOND.")
  #### Ask user to select two points at the structure focus and margin
  #### that will serve as the transect. Returns the coords of those points.
  trans.pts <- as.data.frame(graphics::locator(n=2,type="p",pch=pch.sel,
                                               col=col.sel,cex=cex.sel))
  if (nrow(trans.pts)<2) STOP("Either the focus or margin was not selected.")
  
  #### Calculate slope, intercept, and perpendicular slope to transect
  slpTransect <- diff(trans.pts$y)/diff(trans.pts$x)
  intTransect <- trans.pts$y[1]-slpTransect*trans.pts$x[1]
  slpPerpTransect <- -1/slpTransect
  #### Show the transect if asked to
  if (showTransect) graphics::lines(y~x,data=trans.pts,
                                    lwd=lwd.transect,col=col.transect)
  
  ## User selects annuli on the image ==========================================
  message("\n>> Select points that are annuli.\n",
          "   * When finished selecting points press\n",
          "       the second(right) mouse button and select 'Stop',\n",
          "       the 'Stop' button in Windows, or\n",
          "       the 'Finish' button in RStudio.")
  #### Initially populate pts and orig.pts with transect points
  pts <- orig.pts <- trans.pts
  #### Allow user to select one point at-a-time until locator stopped
  #### Selected points will be snapped to transect if snap2Transect==TRUE
  #### orig.pts are as selected by the user, pts may be on transect if
  ####   snap2Transect==TRUE but may not be if snap2Transect==FALSE
  repeat {
    tmp2 <- as.data.frame(graphics::locator(n=1,
                                            type=ifelse(snap2Transect,"n","p"),
                                            pch=pch.sel,col=col.sel,cex=cex.sel))
    if (!nrow(tmp2)>0) {
      ## no point was selected, user must have selected stop locator
      break
    } else {
      ## A point was selected
      orig.pts <- rbind(orig.pts,tmp2)
      if (!snap2Transect) {
        ## if not snapping points then pts=orig.pts
        pts <- orig.pts
      } else {
        ## snap points to the transect
        tmp2 <- iSnap2Transect(tmp2,slpTransect=slpTransect,
                               intTransect=intTransect,
                               slpPerpTransect=slpPerpTransect)
        ## plot the snapped point
        graphics::points(y~x,data=tmp2,pch=pch.sel,col=col.sel,cex=cex.sel)
        ## and add snapped point to matrix of points
        pts <- rbind(pts,tmp2)
      }      
    }
  } # end repeat
  #### Make sure some points were selected
  if (!nrow(pts)>2) STOP("No points were selected as annuli.")
  #### Re-order points by distance from the first point (the focus)
  pts <- iOrderPts(pts)
  orig.pts <- iOrderPts(orig.pts)
  #### Tell the user how many points were selected
  message("   * ",nrow(pts)," points were selected.\n")
  
  ## Converts selected points to radial measurements ===========================
  radii <- iPts2Rad(pts,edgeIsAnnulus=edgeIsAnnulus,scalingFactor=scalingFactor,
                    pixW2H=windowInfo$pixW2H,id=id,reading=reading)

  ## Create a master data object and write to RData file in working directory ==
  #### Name of RData file
  datanm <- paste0(tools::file_path_sans_ext(img),
                   ifelse(!is.null(suffix),"_",""),
                   suffix,".RData")
  #### Master data object
  dat <- list(image=img,datanm=datanm,description=description,
              edgeIsAnnulus=edgeIsAnnulus,snap2Transect=snap2Transect,
              scalingFactor=scalingFactor,sfSource=sfSource,
              sbPts=sbPts,sbLength=scaleBarLength,
              slpTransect=slpTransect,intTransect=intTransect,
              slpPerpTransect=slpPerpTransect,
              windowSize=windowInfo$windowSize,
              pixW2H=windowInfo$pixW2H,
              orig.pts=orig.pts,pts=pts,radii=radii)
  #### Write the RData file
  save(dat,file=datanm)
  #### Tell user what happend and invisibly return the R object
  message("** All results written to ",datanm)
  invisible(dat)
}



########################################################################
## Snaps selected points to the transect
##
## Perpendicularly "slides" a point to fall on the transect.
########################################################################
iSnap2Transect <- function(pts,slpTransect,intTransect,slpPerpTransect) {
  ## Intercept of line perpendicular to transect through the point.
  intPerp <- pts$y-slpPerpTransect*pts$x
  ## Intersection between transect and perpendicular line through the point
  intersectsX <- (intPerp-intTransect)/(slpTransect-slpPerpTransect)
  intersectsY <- intTransect+slpTransect*intersectsX
  ## Return snapped coordinates
  data.frame(x=intersectsX,y=intersectsY)
}


########################################################################
## Orders a data.frame of x-y coordinates by distance from first point.
########################################################################
iOrderPts <- function(pts) {
  ## find a matrix of distances from the first point (in the first column
  ## returned by dist()), finds the order of those distances, and re-orders
  ## the original points by that order and returns the result
  pts[order(as.matrix(stats::dist(pts))[,1]),]
}

