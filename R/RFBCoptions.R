#' @title See or set arguments for common RFishBC functions
#' 
#' @description The \code{\link{digitizeRadii}} and \code{\link{showDigitizedImage}} functions have a variety of arguments that create great flexibility. Default values for these arguments can be seen with this function.
#' 
#' Additionally, a user may want to change a number of these arguments from their default values and use those new values in a large number of function calls (e.g., processing a larger number of structures with the same characteristics). Changing the argument values from the defaults during each function call is inefficient. Thus, default values for these arguments may be changed for the SESSION with this function.
#' 
#' @param reset A logical that will reset the values to their \dQuote{factory-fresh} defaults if \code{TRUE}.
#' @param \dots An arbitrary number of \code{argument}=\code{value} pairs where \code{argument} is one of the argument names and \code{value} is the new value for the argument. See details and examples.
#' 
#' @return None, but the list in \code{RFBCoptions} will be modified.
#' 
#' @details The arguments that can be set with this function are:
#' \itemize{
#' \item{\code{reading}: }{A single character string (or an object that can be coerced to a character) that identifies the reading for a structure. If the structure will be read multiple times, then this may be used to specify the particular reading. Defaults to \code{NULL}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{description}: }{A single character string that contains a short (but more detailed than in \code{reading}) description for a reading of a structure. Defaults to \code{NULL}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{suffix}: }{A single character string that will be added to the RData file name. If \code{NULL} and \code{reading} is not \code{NULL}, then this will be replaced with the value in \code{reading}. Defaults to \code{NULL}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{edgeIsAnnulus}: }{A single logical that indicates whether the structure margin should be considered an annulus (\code{TRUE}) or not (\code{FALSE}). Use \code{FALSE} if growth between the last annulus and the structure margin is not a complete year's worth of growth (i.e., \dQuote{plus-growth}). Defaults to \code{NULL} which means that the user must set this value. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{windowSize}: }{A single numeric used to set the size of the largest dimension for the window in which the structure image is opened. This size will be the width for wider images and the height for taller images. The other dimension will be set relative to this so that the image is displayed in its native aspect ratio. Defaults to 7 inches. Used in \code{\link{digitizeRadii}} and \code{\link{findScalingFactor}}.}
#' \item{\code{deviceType}: }{A single character that identifies the type of graphic device in which the image will be shown. Defaults to \code{deviceType="windows} which should be used with a Windows OS, but can be set to \code{deviceType="X11} which should be used with a Mac OS.}
#' \item{\code{popID}: }{A single logical that indicates if the fish ID dialog box (only on Windows if no \code{ID} is given in \code{\link{digitizeRadii}}) is populated with a guess at the fish ID. The guess is from using the pattern in \code{IDpattern} (see below) on the image file name sans the extension. This may be useful for when the image name contains the fish ID (and no other numbers). Defaults to \code{TRUE}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{IDpattern}: }{A single regular expression that indicates how to extract a possible fish ID from an image file name. Defaults to selecting all characters after the last underscore in the image file name (sans extension). Used in \code{\link{digitizeRadii}}.}
#' \item{\code{scalingFactor}: }{A single numeric used to convert measurements on the structure image to actual measurements on the structure. Measurements on the structure image will be multiplied by this value. Ignored if \code{scaleBar=TRUE}. Defaults to \code{1}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{scaleBar}: }{A single logical that indicates whether the user will be prompted to select the endpoints of a scale-bar on the structure image. If \code{TRUE}, then must also use \code{scaleBarLength}. If \code{FALSE}, then consider using \code{scalingFactor}. Defaults to \code{FALSE}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{scaleBarLength}: }{A single numeric that represents the actual length of the scale-bar. Ignored if \code{scaleBar=FALSE}. Defaults to \code{NULL}; thus, the user must enter a value if \code{scaleBar=TRUE}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{col.scaleBar}: }{The color of the scale-bar line if \code{scalebar=TRUE}. Defaults to \code{"yellow"}. Used in \code{\link{digitizeRadii}}, \code{\link{showDigitizedImage}}, and \code{\link{findScalingFactor}}.}
#' \item{\code{lwd.scaleBar}: }{The line width of the scale-bar line if \code{scalebar=TRUE}. Defaults to \code{2}. Used in \code{\link{digitizeRadii}}, \code{\link{showDigitizedImage}}, and \code{\link{findScalingFactor}}.}
#' \item{\code{showTransect}: }{A single logical that indicates whether the transect between the points selected at the structure center and margin should be shown. Defaults to \code{TRUE}. Used in \code{\link{digitizeRadii}} and  \code{\link{showDigitizedImage}}.}
#' \item{\code{snap2Transect}: }{A single logical that indicates whether the coordinates of the selected points that represent annuli should be moved to fall exactly on the transect from the structure center to margin. If \code{TRUE} then the points will be moved perpendicularly to the transect (and the original user-selected point will not be seen on the image). If \code{FALSE} then the points will be where the user selected them. Defaults to \code{TRUE}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{col.transect}: }{The color of the transect line if \code{showTransect=TRUE}. Defaults to \code{"cyan"}. Used  in \code{\link{digitizeRadii}} or \code{showTransect=TRUE} in \code{\link{showDigitizedImage}}.}
#' \item{\code{lwd.transect}: }{The width of the transect line if \code{showTransect=TRUE}. Defaults to \code{2}. Used  in \code{\link{digitizeRadii}} or \code{showTransect=TRUE} in \code{\link{showDigitizedImage}}.}
#' \item{\code{pch.sel}: }{The plotting character of points for selected annuli in \code{\link{digitizeRadii}}. Defaults to \code{20} (a small solid circle).}
#' \item{\code{col.sel}: }{The color of points for selected annuli in \code{\link{digitizeRadii}}. Defaults to \code{"yellow"}.}
#' \item{\code{cex.sel}: }{The character expansion value of points for selected annuli in \code{\link{digitizeRadii}}. Defaults to \code{1}.}
#' \item{\code{pch.del}: }{The plotting character of points for DEselected annuli in \code{\link{digitizeRadii}}. Defaults to \code{13} (a circle with an X in it).}
#' \item{\code{col.del}: }{The color of points for DEselected annuli in \code{\link{digitizeRadii}}. Defaults to \code{"red"}.}
#' \item{\code{pch.show}: }{The plotting character for points shown in \code{\link{showDigitizedImage}}. Defaults to \code{19} (a solid circle).}
#' \item{\code{col.show}: }{The color of points shown in \code{\link{showDigitizedImage}}. Defaults to \code{"yellow"}.}
#' \item{\code{cex.show}: }{The character expansion value of points shown in \code{\link{showDigitizedImage}}. Defaults to \code{1}.}
#' \item{\code{showInfo}: }{A single logical that indicates whether the ID information should be shown on the image in \code{\link{digitizeRadii}}. Defaults to \code{TRUE}.}
#' \item{\code{pos.info}: }{A single character that indicates where the ID information should be placed when \code{showInfo=TRUE}. See \code{\link{legend}} for position choices. Defaults to \code{"topleft"}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{cex.info}: }{The character expansion for the ID information when \code{showInfo=TRUE}. Defaults to \code{1.2} Used in \code{\link{digitizeRadii}}.}
#' \item{\code{col.info}: }{The color for the ID information when \code{showInfo=TRUE}. Defaults to \code{"yellow"}. Used in \code{\link{digitizeRadii}}.}
#' \item{\code{showAnnuliLabels}: }{A single logical that indicates whether annulus labels should be shown on the image from \code{\link{showDigitizedImage}}. Defaults to \code{TRUE}, but is ignored if more than one set of annuli will be plotted.}
#' \item{\code{annuliLabels}: }{A numeric vector that indicates the numbers for which annuli should be labeled when \code{showAnnuliLabels=TRUE}. Defaults to \code{NULL} which indicates that all annuli should be labeled. Used in \code{\link{showDigitizedImage}}.}
#' \item{\code{col.ann}: }{The color of the annuli number text when \code{showAnnuliLabels=TRUE} in \code{\link{showDigitizedImage}}. Defaults to \code{"yellow"}.}
#' \item{\code{cex.ann}: }{The character expansion value  of the annuli number text when \code{showAnnuliLabels=TRUE} in \code{\link{showDigitizedImage}}. Defaults to \code{1.2}.}
#' } 
#' 
#' The user will likely only use this function to change arguments at the start of a script, so that those values will be used throughout the analyses in the script. If the values for the arguments need to be changed in any instance of \code{\link{digitizeRadii}} or \code{\link{showDigitizedImage}}, then it is more efficient to change the argument within the call to those functions.
#' 
#' The argument values can be reset to the original defaults by using \code{reset=TRUE}. See examples.
#' 
#' @seealso \code{\link{digitizeRadii}} and \code{\link{showDigitizedImage}}
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @export
#'
#' @examples
#' ## Show all options
#' RFBCoptions()
#' 
#' ## Show how to see and set one option
#' RFBCoptions()$showTransect
#' RFBCoptions(showTransect=FALSE)
#' RFBCoptions()$showTransect
#' RFBCoptions(reset=TRUE)
#' RFBCoptions()$showTransect
#' 
#' ## Multiple options can also be set at once
#' RFBCoptions(pch.show=3,col.show="blue",cex.show=3)
#' 
RFBCoptions <- function(reset=FALSE,...) {
  if (reset) settings::reset(iRFBCopts)
  else iRFBCopts(...) 
}


########################################################################
# This sets default options for the selecting and showing of the radii.
########################################################################
iRFBCopts <- settings::options_manager(reading=NULL,description=NULL,
                suffix=NULL,edgeIsAnnulus=NULL,
                windowSize=7,deviceType="default",
                popID=TRUE,IDpattern='.*\\_',
                scalingFactor=1,scaleBar=FALSE,scaleBarLength=NULL,
                col.scaleBar="yellow",lwd.scaleBar=2,
                showTransect=TRUE,snap2Transect=TRUE,
                col.transect="cyan",lwd.transect=2,
                pch.sel=20,col.sel="yellow",cex.sel=1,
                pch.del=13,col.del="red",
                pch.show=19,col.show="yellow",cex.show=1,
                showInfo=TRUE,pos.info="topleft",col.info="yellow",cex.info=1.2,
                showAnnuliLabels=TRUE,annuliLabels=NULL,
                col.ann="yellow",cex.ann=1.2,
              .allowed=list(
                windowSize=settings::inrange(min=1,max=30),
                deviceType=settings::inlist("default","X11"),
                popID=settings::inlist(TRUE,FALSE),
                scalingFactor=settings::inrange(min=1e-10,max=Inf),
                scaleBar=settings::inlist(TRUE,FALSE),
                lwd.scaleBar=settings::inrange(min=1,max=10),
                showTransect=settings::inlist(TRUE,FALSE),
                snap2Transect=settings::inlist(TRUE,FALSE),
                lwd.transect=settings::inrange(min=1,max=10),
                cex.sel=settings::inrange(min=0.1,max=10),
                cex.show=settings::inrange(min=0.1,max=10),
                showInfo=settings::inlist(TRUE,FALSE),
                pos.info=settings::inlist("topleft","top","topright","right",
                                          "bottomright","bottom","bottomleft",
                                          "left"),
                cex.info=settings::inrange(min=0.1,max=10),
                showAnnuliLabels=settings::inlist(TRUE,FALSE),
                cex.ann=settings::inrange(min=0.1,max=10)
              )
)


########################################################################
## Used to get an option value out of RFBCoptions.
########################################################################
iGetopt <- function(opt) {
  tmp <- RFBCoptions()
  if (!opt %in% names(tmp)) STOP("'opt' is not the name of an option.")
  tmp[[opt]]
}
