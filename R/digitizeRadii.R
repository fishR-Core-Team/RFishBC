#' @title Collect radial measurements from a calcified structure by interactively selecting annuli
#' 
#' @description The user interactively select points on an image of a calcified structure. When finished, radial measurements (from the structure focus to the selected points) are calculated (either with arbitrary units or actual units if a scale-bar is included on the image) and written to an external file for later use.
#' 
#' @param img A vector of strings that indicates the image (must be PNG, JPG, BMP, or TIFF) to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file(s). Alternatively the user can supply the name(s) of the file(s). Either way the file(s) must be in the current working directory.
#' @param id A vector of unique identifiers for the fish or structure(s) being examined. Will be coerced to a character. If length of \code{img} is greater than 1, then the length of \code{id} must be the same. If missing then you will be prompted to enter a value.
#' @param reading See details in \code{\link{RFBCoptions}}.
#' @param description See details in \code{\link{RFBCoptions}}.
#' @param suffix See details in \code{\link{RFBCoptions}}.
#' @param edgeIsAnnulus See details in \code{\link{RFBCoptions}}.
#' @param windowSize See details in \code{\link{RFBCoptions}}.
#' @param deviceType See details in \code{\link{RFBCoptions}}.
#' @param closeWindow See details in \code{\link{RFBCoptions}}.
#' @param popID See details in \code{\link{RFBCoptions}}.
#' @param IDpattern See details in \code{\link{RFBCoptions}}.
#' @param IDreplace See details in \code{\link{RFBCoptions}}.
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
#' @param pch.del See details in \code{\link{RFBCoptions}}.
#' @param col.del See details in \code{\link{RFBCoptions}}.
#' @param showInfo See details in \code{\link{RFBCoptions}}.
#' @param pos.info See details in \code{\link{RFBCoptions}}.
#' @param cex.info See details in \code{\link{RFBCoptions}}.
#' @param col.info See details in \code{\link{RFBCoptions}}.
#'
#' @return \code{NULL} if more than one file was given in \code{img} or, if only one file was given, a list that contains the following:
#' \itemize{
#'   \item{\code{image}: }{The full filename given in \code{img}.}
#'   \item{\code{datanm}: }{The R data filename.}
#'   \item{\code{description}: }{The description given in \code{description}.}
#'   \item{\code{edgeIsAnnulus}: }{The logical given in \code{edgeIsAnnulus} that identified whether the structure edge/margin should be considered as an annulus.}
#'   \item{\code{snap2Transect}: }{The logical from \code{snap2Transect} that identified whether the selected points were \dQuote{snapped} to the transect or not.}
#'   \item{\code{scalingFactor}: }{A single numeric used to convert measurements on the structure image to actual measurements on the structure. Measurements on the structure image were multiplied by this value.}
#'   \item{\code{sfSource}: }{A character string that identifies whether the scaling factor was \code{"Provided"} through the \code{scalingFactor} argument or derived from a \code{"scaleBar"}.}
#'   \item{\code{sbPts}: }{A data.frame of \code{x} and \code{y} coordinates for the endpoints of the scale-bar if the scaling factor was derived from a scale-bar.}
#'   \item{\code{sbLength}: }{A single numeric that is the known (actual) length of the scale-bar if the scaling factor was derived from a scale-bar.}
#'   \item{\code{slpTransect}: }{The slope of the transect.}
#'   \item{\code{intTransect}: }{The intercept of the transect.}
#'   \item{\code{slpPerpTransect}: }{The slope of the line perpendicular to the transect.}
#'   \item{\code{windowSize}: }{A numeric of length two that contains the width and height of the window used to display the structure image. One of these units was set by the given \code{windowSize} value.}
#'   \item{\code{pixW2H}: }{The ratio of pixel width to height. This is used to correct measurements for when an image is not square.}
#'   \item{\code{pts}: }{A data.frame that contains the \code{x} and \code{y} coordinates on the image for the selected annuli. These points may have been \dQuote{snapped} to the transect if \code{snap2Transect==TRUE}.}
#'   \item{\code{radii}: }{A data.frame that contains the unique \code{id}, the \code{reading} code, the age-at-capture in \code{agecap}, the annulus number in \code{ann}, the radial measurements in \code{rad}, and the radial measurement at capture in \code{radcap}.}
#' }.
#' 
#' @details This function requires interaction from the user. A detailed description of its use is in the vignettes on the \href{http://derekogle.com/RFishBC/index.html}{RFishBC website}.
#' 
#' @seealso \code{\link{showDigitizedImage}} and \code{\link{RFBCoptions}}.
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
                          description,edgeIsAnnulus,popID,IDpattern,IDreplace,
                          windowSize,deviceType,closeWindow,
                          scaleBar,scaleBarLength,col.scaleBar,lwd.scaleBar,
                          scalingFactor,showTransect,snap2Transect,
                          col.transect,lwd.transect,
                          pch.sel,col.sel,cex.sel,
                          pch.del,col.del,
                          showInfo,pos.info,cex.info,col.info) {
  ## Process argument defaults =================================================
  if (missing(reading)) reading <- iGetopt("reading")
  if (missing(description)) description <- iGetopt("description")
  if (missing(suffix)) suffix <- iGetopt("suffix")
  if (is.null(suffix) & !is.null(reading)) suffix <- reading
  if (missing(edgeIsAnnulus)) edgeIsAnnulus <- iGetopt("edgeIsAnnulus")
  if (!is.logical(edgeIsAnnulus))
    STOP("'edgeIsAnnulus' must be TRUE or FALSE.")
  if (missing(popID)) popID <- iGetopt("popID")
  if (missing(IDpattern)) IDpattern <- iGetopt("IDpattern")
  if (missing(IDreplace)) IDreplace <- iGetopt("IDreplace")
  if (missing(scaleBar)) scaleBar <- iGetopt("scaleBar")
  if (missing(scaleBarLength)) scaleBarLength <- iGetopt("scaleBarLength")
  if (missing(scalingFactor)) scalingFactor <- iGetopt("scalingFactor")
  if (scaleBar & is.null(scaleBarLength))
    STOP("Must provide a 'scaleBarLength' when 'scaleBar=TRUE'.")
  if (!is.null(scaleBarLength)) {
    if (!is.numeric(scaleBarLength)) STOP("'scaleBarLength' must be numeric.")
    if (scaleBarLength<=0) STOP("'scaleBarLength' must be positive.")
    if (scalingFactor!=RFBCoptions()$scalingFactor)
      STOP("Can not set both 'scaleBarLength' and 'scalingFactor'.")
  }
  if (!scaleBar & !is.null(scaleBarLength)) 
    STOP("Can not use 'scaleBarLength=' with 'scaleBar=FALSE'.")
  if (!is.null(scalingFactor)) {
    if (!is.numeric(scalingFactor)) STOP("'scalingFactor' must be numeric.")
    if (scalingFactor<=0) STOP("'scalingFactor' must be positive.")
  }
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")
  if (missing(showTransect)) showTransect<- iGetopt("showTransect")
  if (missing(snap2Transect)) snap2Transect<- iGetopt("snap2Transect")
  if (snap2Transect & !showTransect) {
    cat("\n!! NOTE that points will be 'snapped' to a transect that is not shown\n",
         "!!      because 'showTtransect=FALSE' and 'snap2Transect=TRUE'.\n\n",
        sep="")
  }
  if (missing(col.transect)) col.transect <- iGetopt("col.transect")
  if (missing(lwd.transect)) lwd.transect <- iGetopt("lwd.transect")
  if (missing(pch.sel)) pch.sel <- iGetopt("pch.sel")
  if (missing(col.sel)) col.sel <- iGetopt("col.sel")
  if (missing(cex.sel)) cex.sel <- iGetopt("cex.sel")
  if (missing(pch.del)) pch.del <- iGetopt("pch.del")
  if (missing(col.del)) col.del <- iGetopt("col.del")
  if (missing(windowSize)) windowSize <- iGetopt("windowSize")
  if (!is.numeric(windowSize)) STOP("'windowSize' must be numeric.")
  if (windowSize<=0) STOP("'windowSize' must be positive.")
  if (missing(deviceType)) deviceType <- iGetopt("deviceType")
  if (missing(closeWindow)) closeWindow <- iGetopt("closeWindow")
  if (missing(showInfo)) showInfo <- iGetopt("showInfo")
  if (missing(pos.info)) pos.info <- iGetopt("pos.info")
  if (missing(cex.info)) cex.info <- iGetopt("cex.info")
  if (missing(col.info)) col.info <- iGetopt("col.info")

  ## Handle getting the image filename =========================================
  img <- iHndlFilenames(img,filter="images",multi=TRUE)

  ## Handle the ID =============================================================
  if (missing(id)) {                                               # nocov start
    ## Guess IDs from image file names
    initID <- tryCatch(getID(img,IDpattern,IDreplace),
                       error=function(e) tools::file_path_sans_ext(img))
    ## If only one image then ask user to enter ID,
    if (length(img)==1) {
      if (grepl('w|W', .Platform$OS.type)) {
        ## we are on Windows ... use a windows dialog box
        ## use img name as the default if popID=TRUE
        id <- utils::winDialogString("Enter a unique ID: ",
                                     ifelse(popID,initID,""))
      } else {
        ## Not on Windows ... use prompt in console if in interactive session
        if (interactive()) id <- readline(prompt="Enter a unique ID: ")
      }
    } else {
      ## Set ID to the initial guesses at IDs when multiple images given
      id <- initID
    }                                                             # nocov end
  } else {
    ## Make sure that img and id have the same length
    if (length(img)!=length(id))
      STOP("Lengths of image file names and IDs must be equal.")
  }  
  if (missing(id) | is.null(id)) STOP("You must provide a unique ID in 'id'.")
  
  ## ===========================================================================
  if (length(img)>1) {
    ## More than one image to process
    for (i in seq_along(img)) {
      digitizeRadii(img[i],id=id[i],reading,suffix,
                    description,edgeIsAnnulus,popID,IDpattern,IDreplace,
                    windowSize,deviceType,closeWindow,
                    scaleBar,scaleBarLength,col.scaleBar,lwd.scaleBar,
                    scalingFactor,showTransect,snap2Transect,
                    col.transect,lwd.transect,
                    pch.sel,col.sel,cex.sel,
                    pch.del,col.del,
                    showInfo,pos.info,cex.info,col.info)
    }
    dat <- NULL
  } else {
    ## Only one image to process
    dat <- iDigitizeRadii1(img,id,reading,suffix,
                           description,edgeIsAnnulus,popID,IDpattern,IDreplace,
                           windowSize,deviceType,
                           scaleBar,scaleBarLength,col.scaleBar,lwd.scaleBar,
                           scalingFactor,showTransect,snap2Transect,
                           col.transect,lwd.transect,
                           pch.sel,col.sel,cex.sel,
                           pch.del,col.del,
                           showInfo,pos.info,cex.info,col.info)
  }
  if (closeWindow) grDevices::dev.off()
  invisible(dat)
}



########################################################################
## =====================================================================
## INTERNAL FUNCTIONS specific to digitizeRadii()
##   others shared with other functions in RFishBC-internals
## =====================================================================
########################################################################

########################################################################
## Digitize one image
########################################################################
iDigitizeRadii1 <- function(img,id,reading,suffix,
                            description,edgeIsAnnulus,popID,IDpattern,IDreplace,
                            windowSize,deviceType,
                            scaleBar,scaleBarLength,col.scaleBar,lwd.scaleBar,
                            scalingFactor,showTransect,snap2Transect,
                            col.transect,lwd.transect,
                            pch.sel,col.sel,cex.sel,
                            pch.del,col.del,
                            showInfo,pos.info,cex.info,col.info) {

  ## Setup logicals that allow an abort or a restart ===========================
  abort <- restart <- FALSE
  
  ## Setup a message ===========================================================
  msg2 <- "  'f'=finished, 'd'=delete, 'q'=abort, 'z'=restart"

  ## Loads image given in img ==================================================
  windowInfo <- iGetImage(img,id,windowSize,deviceType,
                          showInfo,pos.info,cex.info,col.info)
  DONE("Loaded ",img,".\n")
  
  ## Allows user to select a scaling bar to get a scaling factor ===============
  if (scaleBar) { ## scaleBar is on the plot
    RULE("Select endpoints of scale-bar.")
    RULE(msg2,line="-")
    sfSource <- "scaleBar"
    sbInfo <- iScalingFactorFromScaleBar(msg2,scaleBarLength,windowInfo$pixW2H,
                                         col.scaleBar=col.scaleBar,
                                         lwd.scaleBar=lwd.scaleBar,
                                         pch.sel=pch.sel,col.sel=col.sel,
                                         cex.sel=cex.sel,
                                         pch.del=pch.del,col.del=col.del)
    if (is.list(sbInfo)) { # returned a list b/c not abort/restarted
      sbPts <- sbInfo$sbPts
      scalingFactor <- sbInfo$scalingFactor
      DONE("Found scaling factor from selected scale-bar.\n")
    } else { # no list returned b/c abort/restarted
      if (sbInfo=="ABORT") abort <- TRUE
      else if (sbInfo=="RESTART") restart <- TRUE
    }
  } else { ## No scale bar on the plot ... using the scaling factor
    DONE("Using scaling factor provided in 'scalingFactor'.\n")
    sbPts <- NULL
    scaleBarLength <- NULL
    sfSource <- "Provided"
  }
  
  ## User selects a transect on the image ======================================
  if (!abort & !restart) {
    RULE("Select FOCUS (center) and MARGIN (edge) of the structure.")
    RULE(msg2,line="-")
    trans.pts <- iSelectPt(2,"Select FOCUS and MARGIN:",msg2,
                           pch.sel=pch.sel,col.sel=col.sel,cex.sel=cex.sel,
                           pch.del=pch.del,col.del=col.del,
                           snap2Transect=FALSE,trans.pts=NULL,slpTransect=NULL,
                           intTransect=NULL,slpPerpTransect=NULL)
    if (is.data.frame(trans.pts)) { # returned data.frame b/c not abort/restarted
      #### Calculate slope, intercept, and perpendicular slope to transect
      slpTransect <- diff(trans.pts$y)/diff(trans.pts$x)
      intTransect <- trans.pts$y[1]-slpTransect*trans.pts$x[1]
      slpPerpTransect <- -1/slpTransect
      #### Show the transect if asked to
      if (showTransect) {
        graphics::lines(y~x,data=trans.pts,lwd=lwd.transect,col=col.transect)
        DONE("Transect selected and shown on image.\n")
      } else {
        DONE("Transect selected.\n")
      }
    } else { # no data.frame returned b/c abort/restarted
      if (trans.pts=="ABORT") abort <- TRUE
      else if (trans.pts=="RESTART") restart <- TRUE
    }
  }
  
  ## User selects annuli on the image ==========================================
  if (!abort & !restart) {
    RULE("Select points that are annuli.")
    RULE(msg2,line="-")
    pts <- iSelectPt(NULL,"Select ANNULI:",msg2,
                     pch.sel=pch.sel,col.sel=col.sel,cex.sel=cex.sel,
                     pch.del=pch.del,col.del=col.del,
                     snap2Transect=snap2Transect,trans.pts=trans.pts,
                     slpTransect=slpTransect,intTransect=intTransect,
                     slpPerpTransect=slpPerpTransect)
    if (is.data.frame(pts)) { # data.frame returned b/c not abort/restarted
      #### Add transect (focus and margin) to the points
      pts <- rbind(trans.pts,pts)
      #### Re-order points by distance from the first point (the focus)
      pts <- iOrderPts(pts)
      numAnn <- nrow(pts)-2
      if (edgeIsAnnulus) numAnn <- numAnn+1
      #### Tell the user how many points were selected
      if (numAnn==1) DONE("1 point was selected as an annulus.\n")
      else DONE(numAnn," points were selected as annuli.\n")
    } else { # data.frame not returned because abort/restarted
      if (pts=="ABORT") abort <- TRUE
      else if (pts=="RESTART") restart <- TRUE
    }
  }
  
  ## Converts selected points to radial measurements ===========================
  ##    as long as not aborted or asked to restart =============================
  if (!abort & !restart) {
    radii <- iPts2Rad(pts,edgeIsAnnulus=edgeIsAnnulus,scalingFactor=scalingFactor,
                      pixW2H=windowInfo$pixW2H,id=id,reading=reading)
  }

  ## Finish up =================================================================
  if (abort) {
    cat("\n\n")
    DONE("Processing was ABORTED by user! No file written for ",img,".\n")
  } else if (restart) {
    cat("\n\n")
    DONE("Processing is being RESTARTED as requested by user.",
         " No file written for ",img,".\n\n")
    iDigitizeRadii1(img,id,reading,suffix,description,edgeIsAnnulus,popID,
                    IDpattern,IDreplace,windowSize,deviceType,scaleBar,
                    scaleBarLength,col.scaleBar,lwd.scaleBar,scalingFactor,
                    showTransect,snap2Transect,col.transect,lwd.transect,
                    pch.sel,col.sel,cex.sel,pch.del,col.del,showInfo,pos.info,
                    cex.info,col.info)
  } else { ### process results because not abort/restarted
    ### Create a master data object and write to RData file in working directory
    #### Name of RData file
    datanm <- paste0(tools::file_path_sans_ext(img),
                     ifelse(!is.null(suffix),"_",""),
                     suffix,".rds")
    #### Master data object
    dat <- list(image=img,datanm=datanm,description=description,
                edgeIsAnnulus=edgeIsAnnulus,snap2Transect=snap2Transect,
                scalingFactor=scalingFactor,sfSource=sfSource,
                sbPts=sbPts,sbLength=scaleBarLength,
                slpTransect=slpTransect,intTransect=intTransect,
                slpPerpTransect=slpPerpTransect,
                windowSize=windowInfo$windowSize,
                pixW2H=windowInfo$pixW2H,
                pts=pts,radii=radii)
    class(dat) <- "RFishBC"
    #### Write the RData file
    saveRDS(dat,file=datanm)
    #### Tell user what happend and invisibly return the R object
    DONE("Results written to ",datanm,".\n\n")
    invisible(dat)    
  }
}


########################################################################
## Convert selected x-y points to radial measurements
########################################################################
iPts2Rad <- function(pts,edgeIsAnnulus,scalingFactor,pixW2H,id,reading) {
  #### Number of radial measurements is one less than number of points selected
  n <- nrow(pts)-1
  #### Distances in x- and y- directions, corrected for pixel w to h ratio
  distx <- (pts$x[2:(n+1)]-pts$x[1])*pixW2H
  disty <- pts$y[2:(n+1)]-pts$y[1]
  #### Distances between points
  distxy <- sqrt(distx^2+disty^2)
  #### Correct distances for scalingFactor ... and call a radius
  rad <- distxy*scalingFactor
  #### Sort radii in increasing order (probably redundant)
  rad <- rad[order(rad)]
  #### create data.frame with radii information
  data.frame(id=as.character(rep(id,n)),
             reading=as.character(rep(ifelse(is.null(reading),NA,reading),n)),
             agecap=ifelse(edgeIsAnnulus,n,n-1),
             ann=seq_len(n),
             rad=rad,radcap=max(rad),
             stringsAsFactors=FALSE)
}



########################################################################
## Snaps selected points to the transect
##
## Perpendicularly "slides" a point to fall on the transect.
########################################################################
iSnap2Transect <- function(pts,trans.pts,
                           slpTransect,intTransect,slpPerpTransect) {
  if (is.infinite(slpTransect)) {
    ## Transect is perfectly vertical
    ### x-value of point of intercept with transect is same as x on transect
    intersectsX <- trans.pts$x
    ### y-value of point of intercept with transect is same as y of point
    intersectsY <- pts$y
  } else if (isTRUE(all.equal(slpTransect,0))) {
    ## Transect is perfectly horizontal
    ### x-value of point of intercept with transect is same as x of point
    intersectsX <- pts$x
    ### y-value of point of intercept with transect is same as y on transect
    intersectsY <- trans.pts$y
  } else {
    ## Transect is not perfectly vertical, so must be careful with geometry
    ### Intercept of line perpendicular to transect through the point.
    intPerp <- pts$y-slpPerpTransect*pts$x
    ### Intersection between transect and perpendicular line through the point
    intersectsX <- (intPerp-intTransect)/(slpTransect-slpPerpTransect)
    intersectsY <- intTransect+slpTransect*intersectsX
  }
  ### Return snapped coordinates
  data.frame(x=intersectsX,y=intersectsY)
}



########################################################################
## Orders a data.frame of x-y coordinates by distance from first point.
########################################################################
iOrderPts <- function(pts) {
  ## find a matrix of distances from the first point (in the first column
  ## returned by dist()), finds the order of those distances, and re-orders
  ## the original points by that order and returns the result
  pts <- pts[order(as.matrix(stats::dist(pts))[,1]),]
  ## change rownames
  if (nrow(pts)==2) rownames(pts) <- c("center","edge")
  else rownames(pts) <- c("center",1:(nrow(pts)-2),"edge")
  ## Return data.frame
  pts
}
