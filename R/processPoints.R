#' @title Computes radial measurements from selected points and writes all to an R data object
#' 
#' @description Creates radial measurements from selected points and writes all to an R data object
#' 
#' @param fname A string that indicates the image file that was read with \code{readImage} and on which points were selected with \code{selectPoints}. Typically, this is the string returned by \code{readImage}. This should contain the fully pathed file name.
#' @param pts A data.frame of x- and y-coordinates of selected points on the structure. Typically, this is the data.frame returned by \code{selectPoints}.
#' @param ID A unique identifier for the fish or structure being examined. Will be coerced to a character.
#' @param read Identifies the reading for this structure. Typically this can simply be set to \sQuote{1} if the structure will not be read multiple times. If the structure is read multiple times then this should be used to identify with time (which read) this one is. Will be coerced to a character.
#' @param edgeIsAnnulus A logical that indicates whether the point at the structure edge should be considered as an annulus (\code{TRUE}) or not (\code{FALSE}). Use \code{FALSE} if the last selected point represents an incomplete year's worth of growth (i.e., \sQuote{plus-growth}).
#' @param suffix A string that will be added to the R object data file (see Details). This defaults to being the same as the string (possibly coerced) in \code{read}.
#' @param description A string that possibly contains a short description for the read of this structure.
#'
#' @details None yet.
#'
#' @return None, but an R data object (i.e., .RData file) will be written to the same directory/folder as the file in \code{fname} that is a list of the following items:
#' \itemize{
#'   \item description: The description given in \code{description}.
#'   \item ID: The unique identifier given in \code{ID}.
#'   \item image: The filename for the structure image given in \code{fname}.
#'   \item pts: The data.frame of x- and y-coordinates for the points given in \code{pts}.
#'   \item radii: A data.frame that contains the radial measurements (i.e., from the focus to the selected point) for the structure. Specifically, it contains the following items:
#'   \itemize{
#'     \item XXX
#'   }
#' }
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## None yet.
#' 
processPoints <- function(fname,pts,ID,read,edgeIsAnnulus,
                          suffix=read,description=NULL) {
  ## Convert the measured points to radial measurements
  ##   and output a data.frame
  n <- nrow(pts)-1
  ageCap <- ifelse(edgeIsAnnulus,n,n-1)
  radii <- data.frame(ID=as.character(rep(ID,n)),
                      read=as.character(rep(read,n)),
                      ageCap=rep(ageCap,n),
                      ann=1:n,rad=sqrt(((pts$x[2:(n+1)]-pts$x[1])^2)+((pts$y[2:(n+1)]-pts$y[1])^2)),
                      stringsAsFactors=FALSE)
  radii$radCap <- rep(radii$rad[n],n)
  ## Save all the data for later processing
  dat <- list(description=description,ID=ID,
              image=fname,pts=pts,radii=radii)
  fnout <- paste0(tools::file_path_sans_ext(fname),
                  ifelse(!is.null(suffix),"_",""),
                  suffix,".RData")
  save(dat,file=fnout)
  message("Results written to ",fnout)
}
