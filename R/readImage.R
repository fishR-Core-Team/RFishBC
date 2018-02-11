#' @title Loads and plots any of the three bitmapped types of images.
#' 
#' @description Loads and plots any of the three bitmapped types of images as chosen by the user.
#' 
#' @param fname A string that indicates the image to be loaded and plotted. By default the user will be provided a dialog box from which to choose the file. Alternatively the user can supply the name of the file (will look for this file in the current working directory unless a fully pathed name is given).
#'
#' @details This uses \code{\link[readbitmap]{read.bitmap}} from the \pkg{readbitmap} package so that any of PNG, JPG, or BMP files will be automatically detected.
#'
#' @return A string that contains the filename (with path).
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}. However, note that this is nearly exactly the unexported \code{ReadImg} from the \pkg{digitize} package. I did, however, add the code that produces the dialog box for choosing the file.
#' 
#' @export
#'
#' @examples
#' ## None yet
#' 
readImage <- function(fname=file.choose()) {
  op <- graphics::par(mar=c(0,0,0,0))
  on.exit(graphics::par(op))
  graphics::plot.new()
  img <- readbitmap::read.bitmap(fname)
  graphics::rasterImage(img,0,0,1,1)
  invisible(fname)
}
