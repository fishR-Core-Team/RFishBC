#' @title Finds standard intercept (a) for Fraser-Lee back-calculation model for a particular species.
#'
#' @description Finds standard intercept (a; mm) for Fraser-Lee back-calculation model for a particular species for all species for which a value of a has been defined.
#'
#' @param species A string that contains the species name for which to find teh standard intercept value.
#' @return A single value from \code{\link{StdIntLit}} that is the standard intercept value (a; mm) for the species provided in \code{species}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso \code{\link{StdIntLit}}
#' 
#' @keywords manip
#'
#' @examples
#' aStandard("Bluegill")
#' aStandard("Walleye")
#'
#' @export
aStandard <- function(species) {
  # Load StdIntLit data frame into this function's environment. The data/get
  #   combination are used to avoide the "no global binding" note at CHECK
  StdIntLit <- get(utils::data("StdIntLit",envir=environment()),envir=environment())
  # Check species
  if (missing(species)) STOP("'species' must be one of: ",
                             paste(StdIntLit$species,collapse=", "))
  species <- FSA::capFirst(species)
  if (length(species)>1) STOP("Can have only one name in 'species'.")
  if (!species %in% StdIntLit$species) STOP("'species' must be one of: ",
                                            paste(StdIntLit$species,collapse=", "))
  # Return a for that species
  StdIntLit$a[StdIntLit==species]
}
