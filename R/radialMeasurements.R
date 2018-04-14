#' @title Collect radial measurements from a calcified structure by interactively selecting annuli.
#' 
#' @description Computes radial measurements from selected points and writes all to an R data object
#' 
#' The user can interactively select points on a plot by clicking with the first mouse button on the image. When finished, the x- and y-coordinates for each selected point will be returned.
#' 
#' @param fname A string that indicates the image (must be PNG, JPG, or BMP) to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
#' @param ID A unique identifier for the fish or structure being examined. Will be coerced to a character.
#' @param reading See details in \code{\link{RFBCoptions}}.
#' @param description See details in \code{\link{RFBCoptions}}.
#' @param suffix See details in \code{\link{RFBCoptions}}.
#' @param edgeIsAnnulus See details in \code{\link{RFBCoptions}}.
#' @param scaleBar See details in \code{\link{RFBCoptions}}.
#' @param scaleBarLength See details in \code{\link{RFBCoptions}}.
#' @param col.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param lwd.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param scalingFactor See details in \code{\link{RFBCoptions}}.
#' @param addTransect See details in \code{\link{RFBCoptions}}.
#' @param col.transect See details in \code{\link{RFBCoptions}}.
#' @param lwd.transect See details in \code{\link{RFBCoptions}}.
#' @param pch.sel See details in \code{\link{RFBCoptions}}.
#' @param col.sel See details in \code{\link{RFBCoptions}}.
#' @param cex.sel See details in \code{\link{RFBCoptions}}.
#' @param sepWindow See details in \code{\link{RFBCoptions}}.
#' @param windowSize See details in \code{\link{RFBCoptions}}.
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
digitizeRadii <- function(fname=file.choose(),ID,reading,suffix,
                          description,edgeIsAnnulus,scaleBar,
                          scaleBarLength,col.scaleBar,lwd.scaleBar,
                          scalingFactor,addTransect,col.transect,
                          lwd.transect,pch.sel,col.sel,cex.sel,
                          sepWindow,windowSize) {
  ## Process arguments
  if (missing(ID)) stop("You must enter a unique identifier in 'ID'.",
                        call.=FALSE)
  if (missing(reading)) reading <- iGetopt("reading")
  if (missing(description)) description <- iGetopt("description")
  if (missing(suffix)) suffix <- iGetopt("suffix")
  if (is.null(suffix) & !is.null(reading)) suffix <- reading
  if (missing(edgeIsAnnulus)) edgeIsAnnulus <- iGetopt("edgeIsAnnulus")
  if (!is.logical(edgeIsAnnulus)) stop("'edgeIsAnnulus' must be TRUE or FALSE",call.=FALSE)
  if (missing(scaleBar)) scaleBar <- iGetopt("scaleBar")
  if (missing(scaleBarLength)) scaleBarLength <- iGetopt("scaleBarLength")
  if (scaleBar & is.null(scaleBarLength)) stop("Must provide a 'scaleBarLength' when 'scaleBar=TRUE'.",call.=FALSE)
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")
  if (missing(scalingFactor)) scalingFactor <- iGetopt("scalingFactor")
  if (missing(addTransect)) addTransect<- iGetopt("addTransect")
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
  ## Allows the user to select a scaling bar to get scaling factor
  if (scaleBar) {
    message("\n2. Find scaling factor from scale bar.\n",
            "   * Select endpoints on the scale bar.")
    SF <- iScaleBar(scaleBarLength,col=col.scaleBar,lwd=lwd.scaleBar)
  } else {
    message("\n2. Using the `scalingFactor` provided.")
    SF <- list(sbSource="Provided",sbPts=NULL,sbLength=NULL,
               scalingFactor=scalingFactor)
  }
  ## User selects annuli on the image
  pts <- iSelectAnnuli(pch.pts=pch.sel,col.pts=col.sel,
                       cex.pts=cex.sel,addTransect=addTransect,
                       col.trans=col.transect,lwd.trans=lwd.transect)
  ## Converts the selected points to radial measurements
  dat <- iProcessAnnuli(fname,pts,ID,reading,suffix,description,
                        edgeIsAnnulus,SF$scalingFactor)
  ## Add windowSize and scaling factor information to dat list
  dat <- c(dat,SF,windowSize)
  ## Write the dat object to R object filename in the working directory.
  save(dat,file=dat$datobj)
  message("4. All results written to ",dat$datobj)
  ## Invisibly return the R object
  invisible(dat)
}




#' @title Show the points that were selected at some previous time and saved to the R object file.
#' 
#' @description Show the points that were selected at some previous time on the structure and save to the R object file. This is useful for reexamining the selected points at a later time or overlaying selected points from multiple reads of the structure.
#' 
#' @param fname A string that indicates the R object file to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
#' @param sepWindow See details in \code{\link{RFBCoptions}}.
#' @param pch.show See details in \code{\link{RFBCoptions}}.
#' @param col.show See details in \code{\link{RFBCoptions}}.
#' @param cex.show See details in \code{\link{RFBCoptions}}.
#' @param showTransect See details in \code{\link{RFBCoptions}}.
#' @param col.transect See details in \code{\link{RFBCoptions}}.
#' @param lwd.transect See details in \code{\link{RFBCoptions}}.
#' @param col.scaleBar See details in \code{\link{RFBCoptions}}.
#' @param lwd.scaleBar See details in \code{\link{RFBCoptions}}.
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
showDigitizedImage <- function(fname=file.choose(),sepWindow,
                               pch.show,col.show,cex.show,
                               showTransect,col.transect,lwd.transect,
                               col.scaleBar,lwd.scaleBar) {
  ## handle options
  if (missing(sepWindow)) sepWindow <- iGetopt("sepWindow")
  if (missing(pch.show)) pch.show <- iGetopt("pch.show")
  if (missing(col.show)) col.show <- iGetopt("col.show")
  if (missing(cex.show)) cex.show <- iGetopt("cex.show")
  if (missing(showTransect)) showTransect <- iGetopt("showTransect")
  if (missing(col.transect)) col.transect <- iGetopt("col.transect")
  if (missing(lwd.transect)) lwd.transect <- iGetopt("lwd.transect")
  if (missing(col.scaleBar)) col.scaleBar <- iGetopt("col.scaleBar")
  if (missing(lwd.scaleBar)) lwd.scaleBar <- iGetopt("lwd.scaleBar")

  ## Load the data object
  if (missing(fname)) stop("A filename must be provided.",call.=FALSE)
  dat <- NULL # try to avoid "no visible binding" note
  load(fname[1])
  ## Show first image
  iReadImage(dat$image,sepWindow,dat$windowSize)
  ## Show the putative transect ... assumes that the focus and margin
  ## are in the first two rows of dat$pts (as they should be)
  if (showTransect) 
    graphics::lines(dat$pts[1:2,],
                    lwd=lwd.transect[1],col=col.transect[1])
  ## Show points
  graphics::points(dat$pts,
                   pch=pch.show[1],col=col.show[1],cex=cex.show[1])
  ## Show scale-bar, if it was digitized
  if (!is.null(dat$sbPts)) {
    graphics::lines(y~x,data=dat$sbPts,col=col.scaleBar,lwd=lwd.scaleBar)
  }
  ## Add other results
  num <- length(fname)
  if (num>1) {
    # expand colors
    pch.show <- rep(pch.show,ceiling(num/length(pch.show)))
    col.show <- rep(col.show,ceiling(num/length(col.show)))
    cex.show <- rep(cex.show,ceiling(num/length(cex.show)))
    col.transect <- rep(col.transect,ceiling(num/length(col.transect)))
    lwd.transect <- rep(lwd.transect,ceiling(num/length(lwd.transect)))
    for (i in 2:num) {
      load(fname[i])
      if (showTransect) 
        graphics::lines(dat$pts[1:2,],lwd=lwd.transect[i],
                        col=col.transect[i])
      graphics::points(dat$pts,pch=pch.show[i],col=col.show[i],
                       cex=cex.show[i])
    }
  }
}



#' @title Combines radii data from multiple files into one data.frame
#' 
#' @description Combines the radii data that were created with \code{processPoints} and saved to an R object file from multiple R object files into one data.frame. This data.frame can then be post-processed.
#'  
#' @param fnames A character vector of file names (without the path) from which the radii data should be extracted and combined to form one synthetic data.frame. The files should all be in one folder/directory as given in \code{path}.
#' @param path A string that contains the full path name for the folder/directory for which to list files. Defaults to the current working directory (see \code{\link{getwd}} result).
#'
#' @details None yet.
#' 
#' @return A data.frame that contains the radii data created with \code{processPoints} for all files given in \code{fnames}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples 
#' ## None yet
#' 
combineData <- function(fnames,path='.') {
  ## Some checks
  if (length(path)!=1) stop("'path' can take only one string.",
                            call.=FALSE)
  ## Append path to file names
  if (path==".") path <- getwd()
  fnames <- file.path(path,fnames)
  ## Start the resultant data.frame with the data from the first file
  dat <- NULL # try to avoid "no visible binding" note
  load(fnames[1])
  d <- dat$radii
  ## Now rbind to this with data from the remaining files, if more than
  ## one file was given in fnms
  if (length(fnames)>1) {
    for (i in 2:length(fnames)) {
      load(fnames[i])
      d <- rbind(d,dat$radii)
    }
  }
  ## Return the result
  d
}



#' @title List files with a specific extension in a folder/directory
#' 
#' @description This returns a vector with all file names with the \code{ext} extension in the \code{path} folder/directory. In \pkg{RfishBC} this is used primarily to create a list of image file names over which the user will loop or R data object names that can be given to \code{combineFiles}. 
#' 
#' @param ext A string that contains the file extension pattern to match.
#' @param other Other strings to match int eh file names that match the file extensin in \code{ext}.
#' @param path A string that contains the full path name for the folder/directory for which to list files. Defaults to the current working directory (see \code{\link{getwd}} result).
#' @param ignore.case A logical for whether pattern matching should be case sensitive (\code{=FALSE}) or not (\code{TRUE}; DEFAULT).
#' @param \dots Parameters to be given to \code{\link{list.files}}.
#' 
#' @seealso \code{\link{list.files}} in base R.
#'
#' @details None yet.
#'
#' @return Character vector.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet
#' 
listFiles <- function(ext,other=NULL,path=".",ignore.case=TRUE,...) {
  ## Some checks
  if (length(path)!=1) stop("'path' can take only one string.",
                            call.=FALSE)
  if (length(ext)!=1) stop("'ext' can take only one string.",
                           call.=FALSE)
  ## Add a dot to the extension if one does not exist
  if (!grepl("\\.",ext)) ext <- paste0(".",ext)
  ## Get the list of files in path that have the ext extension
  tmp <- list.files(path=path,pattern=paste0("\\",ext,"$"),
                    ignore.case=ignore.case,...)
  if (length(tmp)<1) stop("No files have a ",ext," extension.",call.=FALSE)
  ## Potentially reduce that list to those that match strings in other
  if (!is.null(other)) {
    for (i in seq_along(other)) {
      if (ignore.case) tmp <- tmp[grepl(tolower(other[i]),tolower(tmp))]
      else tmp <- tmp[grepl(tolower(other[i]),tolower(tmp))]
    }
    if (length(tmp)<1) stop("No files with ",ext," extension contain the patterns given in 'other'.",call.=FALSE)
    tmp <- unique(tmp)
  }
  tmp
}


#' @title See or set arguments for functions that handle radii measurements.
#' 
#' @description Allows the user to see or set values for many of the arguments in \code{\link{digitizeRadii}} and \code{\link{showDigitizedImage}}. These values will be used in these functions unless the user changes them with this function or within \code{\link{digitizeRadii}} or \code{\link{showDigitizedImage}}
#' 
#' @param reset A logical that will reset the values to their factory fresh defaults if \code{TRUE}.
#' @param \dots An arbitrary number of \code{argument}=\code{value} pairs where \code{argument} is one of the argument names and \code{value} is the new value for this argument. See details and examples.
#' 
#' @details The arguments that can be set with this function are:
#' \itemize{
#' \item{\code{reading}: }{A single character string (or an object that can be coerced to a character) that identifies the reading for this structure. If the structure will be read multiple times, then this is used to identify which time (which reading) this one is. Defaults to \code{NULL}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{description}: }{A single character string that contains a short but more detailed description for the reading of the structure. Defaults to \code{NULL}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{suffix}: }{A single character string that will be added to the R object data file name (see Details). If \code{NULL} and \code{reading} is not \code{NULL} then this will be replaced with the value in \code{reading}. Defaults to \code{NULL}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{edgeIsAnnulus}: }{A single logical that indicates whether the point at the structure margin should be considered an annulus (\code{TRUE}) or not (\code{FALSE}). Use \code{FALSE} if the last selected point represents an incomplete year's worth of growth (i.e., \sQuote{plus-growth}). Defaults to \code{NULL}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{sepWindow}: }{A single logical that indicates whether the structure image should be opened in a separate window (\code{=TRUE}) or not (\code{=FALSE}). Defaults to \code{TRUE}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{windowSize}: }{A single numeric that is used to set the size of the largest dimension for the window in which the structure image is opened if \code{sepWindow=TRUE}. This size will be the width for wider images and the height for taller images. The other dimension will be set relative to this so that the image is displayed in its native aspect ration. Defaults to 7 inches. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{scalingFactor}: }{A single numeric that is used to convert measurements on the structure image to actual measurements on the structure. Measurements on the structure image will be multiplied by this value. Ignored if \code{scaleBar=TRUE}. Defaults to \code{1}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{scaleBar}: }{A single logical that indicates whether the user will be prompted to select the endpoints of a \sQuote{scale-bar} on the structure image. If \code{TRUE}, then must also use \code{scaleBarLength}. If \code{FALSE}, then consider using \code{scalingFactor}. Defaults to \code{FALSE}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{scaleBarLength}: }{A single numeric that represents the actual length of the \sQuote{scale-bar}. Ignored if \code{scaleBar=FALSE}. Defaults to \code{NULL}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{col.scaleBar}: }{The color of the scale bar line if \code{scalebar=TRUE}. Defaults to \code{"red"}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{lwd.scaleBar}: }{The line width of the scale bar line if \code{scalebar=TRUE}. Defaults to \code{2}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{addTransect}: }{A single logical that indicates whether the points selected at the structure center and margin should be connected to form a linear transect to mark annuli. Defaults to \code{TRUE}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{showTransect}: }{ Defaults to \code{TRUE}. Used in \code{\link{showDigitizedImage}}.}
#' \item{\code{col.transect}: }{The color of the transect line if \code{addTransect=TRUE} in \code{\link{digitizeRadii}} or \code{showTransect=TRUE} in \code{\link{showDigitizedImage}}. Defaults to \code{"yellow"}.}
#' \item{\code{lwd.transect}: }{The width of the transect line if \code{addTransect=TRUE} in \code{\link{digitizeRadii}} or \code{showTransect=TRUE} in \code{\link{showDigitizedImage}}. Defaults to \code{1}. Used in \code{\link{digitizeRadii}} and \code{\link{showDigitizedImage}}.}
#' \item{\code{pch.sel}: }{The plotting character of points for selected annuli. Defaults to \code{3} (a "cross hairs"). Used in \code{\link{digitizeRadii}}.}
#' \item{\code{col.sel}: }{The color of points for selected annuli. Defaults to \code{"red"}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{cex.sel}: }{The character expansion value of points for selected annuli. Defaults to \code{1}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{pch.show}: }{The plotting character for points shown. Defaults to \code{19} (a solid dot). Used in \code{\link{showDigitizedImage}}.}
#' \item{\code{col.show}: }{The color of points shown. Defaults to \code{"red"}. Used in \code{\link{showDigitizedImage}}.}
#' \item{\code{cex.show}: }{The character expansion value of points shown. Defaults to \code{1}. Used in \code{\link{showDigitizedImage}}.}
#' } 
#' 
#' The user will likely use this function to change arguments at the start of a script, so that those values will be used throughout the analyses in the script. If the values for the arguments need to be changed in any instance of \code{\link{digitizeRadii}} or \code{\link{showDigitizedImage}}, then it is more efficient to change the argument within the call to those functions.
#' 
#' The argument values can be reset to the original defaults by using \code{reset=TRUE}. See examples.
#'
#' @return None, but the list in \code{RFBCoptions} will be modified.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' RFBCoptions()
#' RFBCoptions()$addTransect
#' RFBCoptions(addTransect=FALSE)
#' RFBCoptions()$addTransect
#' RFBCoptions(reset=TRUE)
#' RFBCoptions()$addTransect
RFBCoptions <- function(reset=FALSE,...) {
  if (reset) settings::reset(iRFBCopts)
  else iRFBCopts(...) 
}


# This is global to the package's namespace and does not need to be
# documented. However, this sets default options for the selecting and
# showing of the radii.
iRFBCopts <- settings::options_manager(reading=NULL,description=NULL,
                        suffix=NULL,edgeIsAnnulus=NULL,
                        sepWindow=TRUE,windowSize=7,
                        scalingFactor=1,scaleBar=FALSE,scaleBarLength=NULL,
                        col.scaleBar="red",lwd.scaleBar=2,
                        addTransect=TRUE,showTransect=TRUE,
                        col.transect="yellow",lwd.transect=1,
                        pch.sel=3,col.sel="red",cex.sel=1,
                        pch.show=19,col.show="red",cex.show=1,
                      .allowed=list(
                        sepWindow=settings::inlist(TRUE,FALSE),
                        windowSize=settings::inrange(min=1,max=30),
                        scalingFactor=settings::inrange(min=1e-10,max=Inf),
                        scaleBar=settings::inlist(TRUE,FALSE),
                        lwd.scaleBar=settings::inrange(min=1,max=10),
                        addTransect=settings::inlist(TRUE,FALSE),
                        lwd.transect=settings::inrange(min=1,max=10),
                        cex.sel=settings::inrange(min=0.1,max=10),
                        cex.show=settings::inrange(min=0.1,max=10)
                      )
)



########################################################################
#=======================================================================
#-----------------------------------------------------------------------
#
# INTERNAL FUNCTIONS
#
#-----------------------------------------------------------------------
#=======================================================================


########################################################################
## Used to get an option value out of RFBCoptions.
########################################################################
iGetopt <- function(opt) {
  tmp <- RFBCoptions()
  if (!opt %in% names(tmp)) stop("'opt' is not the name of an option.",
                                 call.=FALSE)
  tmp[[opt]]
}

########################################################################
## Loads and plots any of the three bitmapped types of images as chosen
## by the user. This uses readbitmap::read.bitmap() so that any of PNG,
## JPG, or BMP files will be automatically detected. This function is
## nearly exactly the unexported digitize::ReadImg(), except that I
## added the code for the dialog box for choosing the file and the use
## of withr.
########################################################################
iReadImage <- function(fname,sepWindow,windowSize) {
  img <- readbitmap::read.bitmap(fname)
  ## Get window size so image displayed in its native aspect ratio.
  ## Only needed if windowSize contains one value.
  if (length(windowSize)==1) {
    cf <- dim(img)[2:1]
    windowSize <- windowSize*cf/max(cf) 
  }
  if (sepWindow) {
    ## Hoping that this forces a new window that is device independent
    ## and does not get trapped into opening in the RStudio Plots pane.
    if (grDevices::dev.cur()==1) { 
      ## no window open and not sending to RStudio
      grDevices::dev.new(rescale="fixed",noRStudioGD=TRUE,
                         width=windowSize[1],height=windowSize[2],
                         title=paste0("Image: ",fname))
    } else if (names(grDevices::dev.cur())=="RStudioGD") {
      ## sending to RStudio, so try to avoid that
      grDevices::dev.new(rescale="fixed",noRStudioGD=TRUE,
                         width=windowSize[1],height=windowSize[2],
                         title=paste0("Image: ",fname))
    } else {
      ## sending to an already open window, close it because it might
      ## not be the correct window size for the images aspect ratio,
      ## and then open a new one
      grDevices::dev.off()
      grDevices::dev.new(rescale="fixed",noRStudioGD=TRUE,
                         width=windowSize[1],height=windowSize[2],
                         title=paste0("Image: ",fname))
    }
  }
  withr::local_par(list(mar=c(0,0,0,0)))
  graphics::plot.new()
  graphics::rasterImage(img,0,0,1,1)
  invisible(list(windowSize=windowSize))
}


########################################################################
## Compute a scaling factor from a scale bar on the structure image.
########################################################################
iScaleBar <- function(knownLength,col,lwd) {
  tmp <- as.data.frame(graphics::locator(n=2,type="p",pch=3))
  if (nrow(tmp)<2) {
    warning("Two endpoints were not selected for the scale bar;\n thus, no scaling factor was computed.",call.=FALSE)
    scalingFactor <- 1
  } else {
    graphics::lines(y~x,data=tmp,col=col,lwd=lwd)
    maglength <- sqrt(((tmp$x[2]-tmp$x[1])^2)+((tmp$y[2]-tmp$y[1])^2))
    scalingFactor <- knownLength/maglength
  }
  list(sbSource="ScaleBar",sbPts=tmp,sbLength=knownLength,
       scalingFactor=scalingFactor)
}

########################################################################
## Allows the user to interactively select points on a plot by clicking
## with the first mouse button on the image. When finished, the x- and
## y-coordinates for each selected point will be returned.
########################################################################
iSelectAnnuli <- function(pch.pts,col.pts,cex.pts,
                          addTransect,col.trans,lwd.trans) {
  message("\n3. Select points that are annuli.\n",
          "   * MUST select the focus of the structure FIRST.\n",
          "   * MUST select structure margin SECOND.\n",
          "   * Then select points for annuli.\n",
          "   * When finished selecting points press\n",
          "       ESCape in Windows or OS X,\n",
          "       the 'Stop' button in Windows, or\n",
          "       the 'Finish' button in RStudio.")
  tmp1 <- as.data.frame(graphics::locator(n=2,type="p",pch=pch.pts,
                                          col=col.pts,cex=cex.pts))
  if (nrow(tmp1)<2) stop("Either the focus or margin was not selected.",
                         call.=FALSE)
  graphics::lines(y~x,data=tmp1,lwd=lwd.trans,col=col.trans)
  tmp2 <- as.data.frame(graphics::locator(type="p",pch=pch.pts,
                                          col=col.pts,cex=cex.pts))
  if (nrow(tmp2)<1) stop("No points were selected as annuli.",
                         call.=FALSE)
  tmp <- rbind(tmp1,tmp2)
  message("   ",nrow(tmp)," points were selected.\n")
  tmp
}


########################################################################
## Processes the point locations to radial measurements, computes
## an age-at-capture, and radius-at-capture. Combines all structure-
## related information into a data.frame. Combines all process-related
## and structure-related information into a list and writes that
## information into an R object data file in the current working
## directory.
########################################################################
iProcessAnnuli <- function(fname,pts,ID,reading,suffix,description,
                           edgeIsAnnulus,scalingFactor) {
  ## Convert point locations to radial measurements, 
  n <- nrow(pts)-1
  rad <- sqrt(((pts$x[2:(n+1)]-pts$x[1])^2)+
                ((pts$y[2:(n+1)]-pts$y[1])^2))*scalingFactor
  ## Sort the radii to be in increasing order (allows user to select
  ##   them in any order)
  rad <- rad[order(rad)]
  ## create data.frame with radii information
  reading <- ifelse(is.null(reading),NA,reading)
  radii <- data.frame(ID=as.character(rep(ID,n)),
                      reading=as.character(rep(reading,n)),
                      ageCap=ifelse(edgeIsAnnulus,n,n-1),
                      ann=seq_len(n),
                      rad=rad,radCap=max(rad),
                      stringsAsFactors=FALSE)
  ## Organize all results for later processing
  dat <- list(description=description,image=fname,
              datobj=paste0(tools::file_path_sans_ext(fname),
                            ifelse(!is.null(suffix),"_",""),
                            suffix,".RData"),
              pts=pts,radii=radii)
  dat
}
