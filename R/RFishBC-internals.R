#' @title Internal functions.
#'
#' @description Internal functions.
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach


########################################################################
## Sends a start-up message to the console when the package is loaded.
########################################################################
.onAttach <- function(lib,pkg,...) {
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),
                   fields="Version")
  msg <- paste0("## RFishBC v",vers,". See vignettes.\n")
  packageStartupMessage(msg)
}

