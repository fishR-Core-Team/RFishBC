#' @title Internal functions.
#'
#' @description Internal functions.
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach iReadImage iScaleBar iAddTransect iSelectAnnuli iProcessAnnuli


########################################################################
## Sends a start-up message to the console when the package is loaded.
########################################################################
.onAttach <- function(lib,pkg,...) {
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),
                   fields="Version")
  msg <- paste0("## RFishBC v",vers,". See vignettes.\n")
  packageStartupMessage(msg)
}

########################################################################
## Loads and plots any of the three bitmapped types of images as chosen
## by the user. This uses readbitmap::read.bitmap() so that any of PNG,
## JPG, or BMP files will be automatically detected. This function is
## nearly exactly the unexported digitize::ReadImg(), except that I
## added the code for the dialog box for choosing the file and the use
## of withr.
########################################################################
iReadImage <- function(fname=file.choose(),
                       sepWindow=TRUE,titleMsg=NULL) {
  img <- readbitmap::read.bitmap(fname)
  if (sepWindow & grDevices::dev.interactive())
    grDevices::dev.new(rescale="fit",noRStudioGD=TRUE,
                      title=paste(titleMsg,fname))
  withr::local_par(list(mar=c(0,0,0,0)))
  graphics::plot.new()
  graphics::rasterImage(img,0,0,1,1)
  message("1. Working with the ",fname," image.")
  invisible(fname)
}


########################################################################
## Compute a scaling factor from a scale bar on the structure image.
########################################################################
iScaleBar <- function(knownlength,col,lwd,lty) {
  message("\n\n2. Find scaling factor from scale bar.\n",
          "   * Select endpoints on the scale bar.")
  tmp <- as.data.frame(graphics::locator(n=2,type="p",pch=3))
  if (nrow(tmp)<2) {
    warning("Two endpoints were not selected for the scale bar;\n thus, no scaling factor was computed.",call.=FALSE)
    scalingFactor <- 1
  } else {
    lines(y~x,data=tmp,col=col,lwd=lwd,lty=lty)
    maglength <- sqrt(((tmp$x[2]-tmp$x[1])^2)+((tmp$y[2]-tmp$y[1])^2))
    scalingFactor <- knownlength/maglength
  }
  scalingFactor
}

########################################################################
## Adds a linear transect to the structure image.
########################################################################
iAddTransect <- function(col,lwd,lty) {
  message("\n\n3. Add a linear transect to the image.\n",
          "   * Select the focus and a point on the structure margin.")
  tmp <- as.data.frame(graphics::locator(n=2,type="l",
                                         col=col,lwd=lwd,lty=lty))
  if (nrow(tmp)<2) warning("Two points were not selected for the transect;\n thus, no transect was added to the image.",call.=FALSE)
}

########################################################################
## Allows the user to interactively select points on a plot by clicking
## with the first mouse button on the image. When finished, the x- and
## y-coordinates for each selected point will be returned.
########################################################################
iSelectAnnuli <- function(pch,col,cex) {
  message("\n\n4. Select points that are annuli.\n",
          "   * Select the focus of the structure first.\n",
          "   * Select other annuli next.\n",
          "   * Select the structure margin (if not an annulus).\n\n",
          "   * When finished selecting points press\n",
          "       ESCape in Windows or OS X,\n",
          "       the 'Stop' button in Windows, or\n",
          "       the 'Finish' button in RStudio.")
  tmp <- as.data.frame(graphics::locator(type="p",pch=pch,col=col,
                                         cex=cex))
  if (nrow(tmp)<1) stop("No points were selected as annuli.",call.=FALSE)
  message("\n   ",nrow(tmp)," points were selected.\n")
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
  radii <- data.frame(ID=as.character(rep(ID,n)),
                      reading=as.character(rep(reading,n)),
                      ageCap=ifelse(edgeIsAnnulus,n,n-1),
                      ann=seq_len(n),
                      rad=rad,radCap <- max(rad),
                      stringsAsFactors=FALSE)
  ## Organize all results for later processing
  dat <- list(description=description,ID=ID,image=fname,
              fn=paste0(tools::file_path_sans_ext(fname),
                        ifelse(!is.null(suffix),"_",""),
                        suffix,".RData"),
              pts=pts,radii=radii,scalingFactor=scalingFactor)
  dat
}
