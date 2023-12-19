#' @title Back-calculate length at previous ages from standard data format.
#' 
#' @description Back-calculates length at previous ages from a data.frame that was primarily created from \code{\link{combineData}} and \code{\link{digitizeRadii}}. One of several back-calculation models, described in \code{\link{bcFuns}} and Vigliola and Meekan (2009), can be used. Parameter estimates from various models of fish length on structure radius or structure radius on fish length are computed internally and used in the back-calculations. This function is intended to make back-calculation of fish length at previous ages as streamlined as possible.
#'
#' @param dat A data.frame created with \code{\link{combineData}} that MUST have at least the length-at-capture appended as a variable. See Details.
#' @param lencap The unquoted name of the length-at-capture variable.
#' @param BCM A single numeric between 1 and 22 or a string that indicates which model to use (based on numbers and names in Vigliola and Meekan (2009)). See Details in \code{\link{bcFuns}} for the list of available models.
#' @param a The fish length when the structure first forms as used in the Fraser-Lee model (i.e., \code{BCM=1} or \code{BCM="FRALE"}). If this is missing then \code{a} will be estimated as the intercept from the fish length on structure radius linear regression.
#' @param L0p The length at the \dQuote{Biological Intercept} point. Only used in the \dQuote{Biological Intercept} (\code{BCM=3}), \dQuote{Watanabe and Kuroki} (\code{BCM=12}), and \dQuote{Modified Fry} (\code{BCM=14}) models.
#' @param R0p The structure radius at the \dQuote{Biological Intercept} point. Only used in the \dQuote{Biological Intercept} (\code{BCM=3}), \dQuote{Watanabe and Kuroki} (\code{BCM=12}), and \dQuote{Modified Fry} (\code{BCM=14}) models.
#' @param L0 The length at the arbitrarily selected point in the \dQuote{Fry} (\code{BCM=13}) model.
#' @param R0 The structure radius at the arbitrarily selected point in the \dQuote{Fry} (\code{BCM=13}) model.
#' @param inFormat The format of the data in \code{dat}. The two choices are \code{"long"} with one radial measurement per line (and all radial measurements for a fish in separate rows) and \code{"wide"} with one fish per line (and all radial measurements in separate variables). Defaults to \code{"long"}.
#' @param outFormat The format for the returned data.frame. Choices are as described for \code{inFormat}. Defaults to be the same as \code{inFormat}.
#' @param deletePlusGrowth A logical that indicates whether the radial measurement that corresponds to \dQuote{plus-growth} on the structure should be deleted (\code{TRUE}; Default) or not (\code{FALSE}).
#' @param digits Number of digits to which the back-calculated lengths should be rounded. Defaults to the value returned by \code{getOptions("digits")}, which is generally 7 digits.
#'
#' @return A data.frame similar to \code{dat} but with the radial measurements replaced by back-calculated lengths at previous ages.
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @keywords manip
#'
#' @examples
#' ## Get some data
#' data(SMBassWB1,package="RFishBC") ## fish data
#' data(SMBassWB2,package="RFishBC") ## rad data
#' 
#' # Simplify to 3 fish so we can see what is going on
#' tmp1 <- subset(SMBassWB1,id %in% c(377,378,379))
#' tmp2 <- subset(SMBassWB2,id %in% c(377,378,379))
#' 
#' # Combine data frames to form a wide data frame (i.e., a left join)
#' wdat1 <- merge(tmp1,tmp2,by="id",all.x=TRUE)
#' wdat1
#'  
#' # Make a long data frame for examples (remove annuli with NA rads)
#' ldat1 <- tidyr::pivot_longer(wdat1,rad1:rad9,names_to="ann",names_prefix="rad",
#'                              values_to="rad")
#' ldat1 <- subset(ldat1,!is.na(rad))
#' ldat1 <- as.data.frame(ldat1)
#' ldat1
#' 
#' ## Back-calculate using Dahl-Lea method
#' # wide in and wide out
#' wwres1 <- backCalc(wdat1,lencap,BCM="DALE",inFormat="wide",digits=0)
#' wwres1
#' 
#' # wide in and long out
#' wlres1 <- backCalc(wdat1,lencap,BCM="DALE",inFormat="wide",
#'                    outFormat="long",digits=0)
#' wlres1
#' 
#' # long in and wide out
#' lwres1 <- backCalc(ldat1,lencap,BCM="DALE",inFormat="long",digits=0)
#' lwres1
#' 
#' # wide in and long out
#' llres1 <- backCalc(ldat1,lencap,BCM="DALE",inFormat="long",
#'                    outFormat="long",digits=0)
#' llres1
#' 
#' ## Situation with no radial measurements for some fish
#' # Create an extra fish with length (tmp1) but no rad
#' tmp1a <- rbind(tmp1,
#'                data.frame(id=999,
#'                species="SMB",lake="WB",gear="E",
#'                yearcap=1990,lencap=225))
#' wdat2 <- merge(tmp1a,tmp2,by="id",all.x=TRUE)
#' wdat2
#'                 
#' # wide in and wide out
#' wwres2 <- backCalc(wdat2,lencap,BCM="DALE",inFormat="wide",digits=0)
#' wwres2
#' 
#' @export
backCalc <- function(dat,lencap,BCM,inFormat,outFormat=inFormat,
                     a=NULL,L0p=NULL,R0p=NULL,L0=NULL,R0=NULL,
                     deletePlusGrowth=TRUE,digits=getOption("digits")) {
  ## Some error checking
  BCM <- iGetBCMethod(BCM)
  if (BCM %in% c(5,19,20)) STOP("The BCM",BCM," model is not yet implemented.")
  if (missing(lencap))
    STOP("Variable with length-at-capture data must be given in 'lencap'.")
  if (missing(inFormat)) STOP("'inFormat' must be 'wide' or 'long'.")
  if (!inFormat %in% c("long","wide"))
    STOP("'inFormat' must be 'wide' or 'long'.")
  if (!outFormat %in% c("long","wide"))
    STOP("'outFormat' must be 'wide' or 'long'.")
  msg <- "A value must be provided for "
  if (BCM %in% c(3,12,14) & is.null(L0p)) STOP(msg,"'L0p' when 'BCM=",BCM,"'.")
  if (BCM %in% c(3,14) & is.null(R0p)) STOP(msg,"'R0p' when 'BCM=",BCM,"'.")
  if (BCM %in% c(13) & is.null(L0)) STOP(msg,"'L0' when 'BCM=",BCM,"'.")
  if (BCM %in% c(12,13) & is.null(R0)) STOP(msg,"'R0' when 'BCM=",BCM,"'.")
  if (BCM %in% c(13,14) & is.null(a)) STOP(msg,"'a' when 'BCM=",BCM,"'.")
  ## Trying to avoid global bindings issues
  ann <- rad <- bclen <- NULL
  
  ## Change wide to long (if need to)
  if (inFormat=="wide") {
    ## Convert wide to long
    nms <- names(dat)
    rads <- nms[grepl("rad",nms) & !grepl("radcap",nms)]
    dat <- tidyr::pivot_longer(dat,rads[1]:rads[length(rads)],
                               names_to="ann",values_to="rad")
    ## Change annuli labels into annuli numbers
    dat$ann <- as.numeric(stringr::str_replace_all(dat$ann,"rad",""))
  }
  ## Extract fish for which a radius was not measured (save to add back at end)
  ##   assumes no rads measured if first was not measured
  norad_dat <- dat[dat$ann==1 & is.na(dat$rad),]
  
  ## Remove annuli where the radius was missing
  dat <- dat[!is.na(dat$rad),]
  ## Delete plus-growth if asked to do so
  if (deletePlusGrowth) dat <- dat[dat$ann<=dat$agecap,]
  ## Sort by id and then ann number
  dat <- dat[order(dat$id,dat$ann),]
  
  ## Perform relevant regressions if needed
  ### initiate all possible regression variables (except for a)
  b <- c <- A <- B <- C <- rsq <- NULL
  ### Get data (one lencap and one radcap per id) for regressions
  regdat <- dat[dat$ann==1,]
  regLcap <- regdat[,rlang::quo_name(rlang::enquo(lencap)),drop=TRUE]
  regRcap <- regdat$radcap
  regAcap <- regdat$agecap
  ### Fit the models
  if (BCM %in% c(2,4)) { # SLR of L on R (extract a, b)
    regLR <- stats::lm(regLcap~regRcap)
    if (is.null(a) | BCM!=2) a <- stats::coef(regLR)[[1]]
    b <- stats::coef(regLR)[[2]]
    rsq <- FSA::rSquared(regLR)
  } else if (BCM==6) { # SLR of R on L (extract A, B)
    regRL <- stats::lm(regRcap~regLcap)
    A <- stats::coef(regRL)[[1]]
    B <- stats::coef(regRL)[[2]]
    rsq <- FSA::rSquared(regRL)
  } else if (BCM==7) { # MLR of R on L and A (extract A, B, C)
    regRLA <- stats::lm(regRcap~regLcap+regAcap)
    A <- stats::coef(regRLA)[[1]]
    B <- stats::coef(regRLA)[[2]]
    C <- stats::coef(regRLA)[[3]]
    rsq <- FSA::rSquared(regRLA)
  } else if (BCM==8) { # MLR of L on R and A (extract a, b, c)
    regLRA <- stats::lm(regLcap~regRcap+regAcap)
    a <- stats::coef(regLRA)[[1]]
    b <- stats::coef(regLRA)[[2]]
    c <- stats::coef(regLRA)[[3]]
    rsq <- FSA::rSquared(regLRA)
  } else if (BCM==9) { # SLR of log(L) on log(R) (extract c)
    regLR2 <- stats::lm(log(regLcap)~log(regRcap))
    c <- stats::coef(regLR2)[[2]]
    rsq <- FSA::rSquared(regLR2)
  } else if (BCM==10) { # NLS of L on R (extract c)
    tmp <- stats::lm(log(regLcap)~log(regRcap))
    sv <- stats::coef(tmp)
    names(sv) <- c("b","c")
    nlsLR1 <- stats::nls(regLcap~b*regRcap^c,start=sv)
    c <- stats::coef(nlsLR1)[[2]]
  } else if (BCM==11) { # NLS of R on L (extract C)
    tmp <- stats::lm(log(regRcap)~log(regLcap))
    sv <- stats::coef(tmp)
    names(sv) <- c("B","C")
    nlsRL1 <- stats::nls(regRcap~B*regLcap^C)
    C <- stats::coef(nlsRL1)[[2]]
  } else if (BCM==15) { # NLS of L on R (extract a,b,c)
    tmp <- stats::lm(log(regLcap)~log(regRcap))
    sv <- c(0,stats::coef(tmp))
    names(sv) <- c("a","b","c")
    nlsLR2 <- stats::nls(regLcap~a+b*regRcap^c)
    a <- stats::coef(nlsLR2)[[1]]
    b <- stats::coef(nlsLR2)[[2]]
    c <- stats::coef(nlsLR2)[[3]]
  } else if (BCM==16) { # NLS of R on L (extract B,C)
    tmp <- stats::lm(log(regRcap)~log(regLcap))
    C <- 1/stats::coef(tmp)[[2]]
    B <- exp(C*stats::coef(tmp)[[1]])
    sv <- c(A=0,B=B,C=C)
    nlsRL2 <- stats::nls(regRcap~((regLcap-A)/B)^(1/C),start=sv)
    A <- stats::coef(nlsRL2)[[1]]
    C <- stats::coef(nlsRL2)[[3]]
  } else if (BCM==17) { # QR of L on R (extract a,b,c)
    qregLR <- stats::lm(regLcap~regRcap+I(regRcap^2))
    a <- stats::coef(qregLR)[[1]]
    b <- stats::coef(qregLR)[[2]]
    c <- stats::coef(qregLR)[[3]]
    rsq <- FSA::rSquared(qregLR)
  } else if (BCM==18) { # QR of R on L (extract A,B,C)
    qregRL <- stats::lm(regRcap~regLcap+I(regLcap^2))
    A <- stats::coef(qregRL)[[1]]
    B <- stats::coef(qregRL)[[2]]
    C <- stats::coef(qregRL)[[3]]
    rsq <- FSA::rSquared(qregRL)
  } else if (BCM==21) { # NLS L on R (extract a, bb)
    tmp <- stats::lm(log(regLcap)~regRcap)
    sv <- stats::coef(tmp)
    names(sv) <- c("a","b")
    nlsLR3 <- stats::nls(regLcap~exp(a+b*regRcap),start=sv)
    a <- stats::coef(nlsLR3)[[1]]
    b <- stats::coef(nlsLR3)[[2]]
  } else if (BCM==22) { # NLS of R on L (extract AA and BB)
    tmp <- stats::lm(regRcap~log(regLcap))
    B <- 1/stats::coef(tmp)[[2]]
    A <- -stats::coef(tmp)[[1]]*A
    sv <- c(A=A,B=B)
    nlsRL <- stats::nls(regRcap~(log(regLcap)-A)/B,start=sv)
    A <- stats::coef(nlsRL)[[1]]
    B <- stats::coef(nlsRL)[[2]]
  }
  
  # Warn about possible poor back-calculation values
  if (!is.null(rsq)) {
    if (rsq<0.80) 
      WARN("R-squared for the length-structure relationship is low (",
           formatC(rsq,format="f",digits=3),"). The\n",
           "computed model coefficients and resulting back-calculated lengths\n",
           "may be suspect! Examine the length-structure plot for your data.\n")
  }
  
  ## Perform the back-calculation
  ### Get the back-calculation model function
  BCFUN <- bcFuns(BCM)
  ### Get the length-at-capture info (as it may variable names)
  Lcap <- dat[,rlang::quo_name(rlang::enquo(lencap)),drop=TRUE]
  ### Back-calculate ... assumes names from digitizeRadii
  dat$bclen <- BCFUN(Lcap,dat$rad,dat$radcap,
                     Ai=dat$ann,Acap=dat$agecap,
                     a=a,b=b,c=c,A=A,B=B,C=C,
                     L0p=L0p,R0p=R0p,L0=L0,R0=R0)
  ### Round the back-calculated length
  dat$bclen <- round(dat$bclen,digits=digits)
  
  ## Prepare data to return
  ### Add back fish with no radial measurements if they exist
  if (nrow(norad_dat)>0) {
    norad_dat$bclen <- NA
    dat <- rbind(dat,norad_dat)
  }
  ### Remove radii information
  dat <- dat[,!grepl("rad",names(dat))]
  ### Convert to wide format (if asked to do so)
  if (outFormat=="wide") {
    dat <- tidyr::pivot_wider(dat,names_from="ann",names_prefix="len",
                              values_from="bclen")
  }
  ## Return the data
  dat
}
