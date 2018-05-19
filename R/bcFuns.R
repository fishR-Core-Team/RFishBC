#' @title Creates a function for a specific model.
#'
#' @description Creates a function for a specific model based on definitions in Vigliola and Meekan (2009).
#'
#' @param BCM A single numeric between 1 and 22 or a string that indicates which model to use (based on numbers and names in Vigliola and Meekan (2009)).
#' @param verbose A logical that indicates whether a message about the model and parameter definitions should be output.
#'
#' @details The following models, based on definitions with abbreviations and model numbers from Vigliola and Meekan (2009), are supported.
#'
#' \tabular{ccl}{
#' \bold{Abbreviation} \tab \bold{Number} \tab \bold{Model} \cr
#' DALE \tab 1 \tab Dahl-Lea \cr
#' FRALE \tab 2 \tab Fraser-Lee \cr
#' BI, LBI \tab 3 \tab (Linear) Biological Intercept \cr
#' BPH, LBPH \tab 4 \tab (Linear) Body Proportional Hypothesis \cr
#' TVG \tab 5 \tab Time-Varying Growth \cr
#' SPH, LSPH \tab 6 \tab (Linear) Scale Proportional Hypothesis \cr
#' AE, AESPH \tab 7 \tab (Age Effect) Scale Proportional Hypothesis \cr
#' AEBPH \tab 8 \tab (Age Effect) Body Proportional Hypothesis \cr
#' MONA \tab 9 \tab Monastyrsky \cr
#' MONA-BPH \tab 10 \tab Monastyrsky Body Proportional Hypothesis \cr
#' MONA-SPH \tab 11 \tab Monastyrsky Scale Proportional Hypothesis \cr
#' WAKU \tab 12 \tab Watanabe and Kuroki \cr
#' FRY \tab 13 \tab Fry \cr
#' MF, ABI \tab 14 \tab Modified Fry, Allometric Biological Intercept \cr
#' FRY-BPH, ABPH \tab 15 \tab Fry, Allometric Body Proportional Hypothesis \cr
#' FRY-SPH, ASPH \tab 16 \tab Fry, Allometric Scale Proportional Hypothesis \cr
#' QBPH \tab 17 \tab Quadratic Body Proportional Hypothesis \cr
#' QSPH \tab 18 \tab Quadratic Scale Proportional Hypothesis \cr
#' PBPH \tab 19 \tab Polynomial Body Proportional Hypothesis \cr
#' PSPH \tab 20 \tab Polynomial Scale Proportional Hypothesis \cr
#' EBPH \tab 21 \tab Exponential Body Proportional Hypothesis \cr
#' ESPH \tab 22 \tab Exponential Scale Proportional Hypothesis \cr
#' }
#' @return A function that can be used to predict length at previous age (Li) given length-at-capture (Lcap), hard-part radius-at-age i (Ri), and hard-part radius-at-capture (Rcap). In addition, some functions/models may require the previous age (agei) and the age-at-capture (agec), certain parameters related to the biological intercept (R0p & L0p), or certain parameters estimated from various regression models (a,b,c,A,B,C). See source for more information.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Supplement: \url{http://derekogle.com/IFAR/supplements/backcalculation/}
#'
#' @references
#' Vigliola, L. and M.G. Meekan. 2009. The back-calculation of fish growth from otoliths. pp. 174-211. in B.S. Green et al. (editors). Tropical Fish Otoliths: Information for Assessment, Management and Ecology. Review: Methods and Technologies in Fish Biology and Fisheries 11. Springer. [Was (is?) available from https://www.researchgate.net/publication/226394736_The_Back-Calculation_of_Fish_Growth_From_Otoliths.]
#'
#' @keywords manip
#'
#' @examples
#' ## Simple Examples
#' ( bcm1 <- bcFuns(1) )
#' bcm1(20,10,40)
#'
#' ## Example with dummy length-at-cap, radii-at-cap, and radii-at-age
#' lencap <- c(100,100,100,150,150)
#' radcap <- c(20,20,20,30,30)
#' rad    <- c( 5,10,15,15,25)
#' bcm1(lencap,rad,radcap)
#'
#' ( bcm2 <- bcFuns("FRALE") )
#' bcm2(lencap,rad,radcap,2)  # demonstrated with a=2
#'
#' @export
bcFuns <- function(BCM,verbose=FALSE) {
  ## Do some checking
  if (missing(BCM)) STOP("A back-calculation function must be chosen with 'BCM'")
  if (length(BCM)>1) STOP("Only one value may be given to 'BCM'")
  if (is.numeric(BCM)) {
    ## Function declared numerically
    if (BCM<1 | BCM>22) STOP("BCM number must be between 1 and 22 inclusive.")
  } else {
    ## Function declared by name
    BCM.nms <- c("DALE","FRALE","BI","LBI","BPH","LBPH","TVG","SPH",
                 "LSPH","AE","AESPH","AEBPH","MONA","MONA-BPH",
                 "MONA-SPH","WAKU","FRY","MF","ABI","FRY-BPH","ABPH",
                 "FRY-SPH","ASPH","QBPH","QSPH","PBPH","PSPH","EBPH",
                 "ESPH")
    BCM.nums <- c(1,2,3,3,4,4,5,6,6,7,7,8,9,10,11,12,13,14,14,15,15,
                  16,16,17,18,19,20,21,22)
    BCM <- toupper(BCM)
    if (!(BCM %in% BCM.nms)) {
      msg <- paste(strwrap(paste("'BCM' must be one of:",
                                 paste(BCM.nms,collapse=", ")),
                           width=62),collapse="\n")
      STOP(msg)
    } else {
      # All is good ... convert string to numeric
      BCM <- BCM.nums[which(BCM.nms %in% BCM)]
    }
  }

  ## identify the functions
  if (BCM==1) {
      if (verbose) message("You chose the BCM1 or DALE model.\n\n")
      function(Lcap,Ri,Rcap) { (Ri/Rcap)*Lcap }
  } else if (BCM==2) {
      if (verbose) message("You chose the BCM2 or FRALE model.\n\n")
      function(Lcap,Ri,Rcap,a) { a+(Lcap-a)*(Ri/Rcap) }
  } else if (BCM==3) {
      if (verbose) message("You chose the BCM3, BI, or LBI model.\n\n")
      function(Lcap,Ri,Rcap,L0p,R0p) { Lcap+(Ri-Rcap)*(Lcap-L0p)/(Rcap-R0p) }
  } else if (BCM==4) {
      if (verbose) message("You chose the BCM4 or LBPH model.\n\n")
      function(Lcap,Ri,Rcap,a,b) { (a+b*Ri)*Lcap/(a+b*Rcap) }
  } else if (BCM==5) {
      STOP("The BCM5 (TVG) function is not yet implemented.",call.=FALSE)
  } else if (BCM==6) {
      if (verbose) message("You chose the BCM6 or LSPH model.\n\n")
      function(Lcap,Ri,Rcap,A,B) { (Ri/Rcap*(A+B*Lcap)-A)/B }
  } else if (BCM==7) {
      if (verbose) message("You chose the BCM7, AE, or AESPH model.\n\n")
      function(Lcap,Ri,Rcap,agei,agec,a,b,c) { -a/b+(Lcap+a/b+c/b*agec)*Ri/Rcap-c/b*agei }
  } else if (BCM==8) {
      if (verbose) message("You chose the BCM8 or AEBPH model.\n\n")
      function(Lcap,Ri,Rcap,agei,agec,A,B,C) { (A+B*Ri+C*agei)/(A+B*Rcap+C*agec)*Lcap }
  } else if (BCM==9) {
      if (verbose) message("You chose the BCM9 or MONA model.\n\n")
      function(Lcap,Ri,Rcap,c) { Lcap*((Ri/Rcap)^c) }
  } else if (BCM==10) {
      if (verbose) message("You chose the BCM10 or MONA-BPH model.\n\n")
      # Same as BCM9 but uses nls results to estimate c
      function(Lcap,Ri,Rcap,c) { Lcap*((Ri/Rcap)^c) }
  } else if (BCM==11) {
      if (verbose) message("You chose the BCM11 or MONA-SPH model.\n\n")
      function(Lcap,Ri,Rcap,C) { Lcap*((Ri/Rcap)^(1/C)) }
  } else if (BCM==12) {
      if (verbose) message("You chose the BCM12 or WAKU model.\n\n")
      function(Lcap,Ri,Rcap,L0p,R0) {
        exp(log(L0p) + ((log(Lcap)-log(L0p))*(log(Ri)-log(R0)))/(log(Rcap)-log(R0)))
      }
  } else if (BCM==13) {
      if (verbose) message("You chose the BCM13 or FRY model.\n\n")
      function(Lcap,Ri,Rcap,L0,R0,a) {
        a + exp(log(L0-a) + ((log(Lcap-a)-log(L0-a))*(log(Ri)-log(R0)))/(log(Rcap)-log(R0))) 
      }
  } else if (BCM==14) {
      if (verbose) message("You chose the BCM14, MF, or ABI model.\n\n")
      function(Lcap,Ri,Rcap,L0p,R0p,a) {
        a + exp(log(L0p-a) + ((log(Lcap-a)-log(L0p-a))*(log(Ri)-log(R0p)))/(log(Rcap)-log(R0p)))
      }
  } else if (BCM==15) {
      if (verbose) message("You chose the BCM15, FRY-BPH, or ABPH model.\n\n")
      function(Lcap,Ri,Rcap,a,b,c) { (a+b*Ri^c)/(a+b*Rcap^c)*Lcap }
  } else if (BCM==16) {
      if (verbose) message("You chose the BCM16, FRY-SPH, or ASPH model.\n\n")
      function(Lcap,Ri,Rcap,a,b,c) { a+(Lcap-a)*((Ri/Rcap)^c) }
  } else if (BCM==17) {
      if (verbose) message("You chose the BCM17 or QBPH model.\n\n")
      function(Lcap,Ri,Rcap,a,b,c) { (a+b*Ri+c*(Ri^2))/(a+b*Rcap+c*(Rcap^2))*Lcap }
  } else if (BCM==18) {
      if (verbose) message("You chose the BCM18 or QSPH model.\n\n")
      function(Lcap,Ri,Rcap,a,b,c) { 
        cf1 <- b
        cf2 <- c
        Li <- numeric(length(Lcap))
        for (i in seq_along(Li)) {
          cf0 <- a-((Ri[i]/Rcap[i])*(a+b*Lcap[i]+c*Lcap[i]^2))
          roots <- Re(polyroot(c(cf0,cf1,cf2)))
          Li[i] <- roots[which(sign(roots)==1)]
        }
        Li
      }
  } else if (BCM==19) {
      if (verbose) message("You chose the BCM19 or PBPH model.\n\n")
      function(Lcap,Ri,Rcap,a) {
        # a must be a vector of coefficients from polynomial regression
        exps <- 0:(length(a)-1)
        Li <- numeric(length(Lcap))
        for (i in seq_along(Lcap)) {
          num <- sum( a*Ri[i]^exps )
          denom <- sum( a*Rcap[i]^exps )
          Li[i] <- num/denom*Lcap[i]
        }
        Li
      }
  } else if (BCM==20) { 
      # Note that this is a function that should be used when finding
      #   a root, not to actually back-calculate
      if (verbose) message("You chose the BCM20 or PSPH model.\n\n")
      function(Lcap,Ri,Rcap,a) {
        # a must be a vector of coefficients from polynomial regression
        exps <- 0:(length(a)-1)
        Li <- numeric(length(Lcap))
        for (i in seq_along(Li)) {
          if (Ri[i]==Rcap[i]) { Li[i] <- Lcap[i] }
          else {
            cf <- a
            cf[1] <- cf[1] - Ri[i]/Rcap[i]*sum(a*Lcap[i]^exps)
            roots <- Re(polyroot(cf))
            # find only positive roots
            roots <- roots[which(sign(roots)==1)]
            # only find root less than lencap
            roots <- roots[which(roots<=Lcap[i])]
            ifelse(length(roots)!=1,Li[i] <- NA,Li[i] <- roots)
          }
        }
        Li
      }      
  } else if (BCM==21) {
      if (verbose) message("You chose the BCM21 or EBPH model.\n\n")
      function(Lcap,Ri,Rcap,a,b) { exp(a+b*Ri)/exp(a+b*Rcap)*Lcap }
  } else if (BCM==22) {
      if (verbose) message("You chose the BCM22 or ESPH model.\n\n")
      function(Lcap,Ri,Rcap,a) { exp(a+(log(Lcap)-a)*Ri/Rcap) }
  } 
}



#' @name BCfunctions
#' @rdname BCfunctions
#' @title Specific back-calculation functions
#' @description Provides a function to compute back-calculated lengths at previous ages for several of the most common back-calculation models: Dahl-Lea, Fraser-Lee, Body Proportional Hypothesis (BPH), Scale Proportional Hypothesis (SPH), Biological Intercept, and Modified Fry. See \code{\link{bcFuns}} for other back-calculation functions.
#' @param Lcap A numeric vector of lengths-at-capture.
#' @param Ri A numeric vector of radial measurements to the ith annulus.
#' @param Rcap A numeric vector of total structure radii-at-capture.
#' @param A The intercept of the radius- on length-at-capture regression.
#' @param B The slope of the radius- on length-at-capture regression.
#' @param a The length of the fish at the time of structure formation, the intercept of the length- on radius-at-capture regression, or, when using scales, from published “standards” for a species (Carlander 1982).
#' @param b The slope of the length- on radius-at-capture regression.
#' @param L0p Fish length at the \dQuote{Biological Intercept}.
#' @param R0p Radius at the \dQuote{Biological Intercept}.
#' @references See references in \code{\link{bcFuns}}.
#' @seealso \code{\link{bcFuns}}.
#' @return A back-calculated length-at-age.
NULL

#' @rdname BCfunctions
#' @export
DahlLea <- function(Lcap,Ri,Rcap) {
  if (!is.numeric(Lcap)) STOP("'Lcap' must be numeric.")
  if (any(Lcap<=0)) STOP("All 'Lcap' must be positive.")
  if (!is.numeric(Ri)) STOP("'Ri' must be numeric.")
  if (any(Ri<=0)) STOP("All 'Ri' must be positive.")
  if (!is.numeric(Rcap)) STOP("'Rcap' must be numeric.")
  if (any(Rcap<=0)) STOP("All 'Rcap' must be positive.")
  (Ri/Rcap)*Lcap
}

#' @rdname BCfunctions
#' @export
FraserLee <- function(Lcap,Ri,Rcap,a) {
  if (!is.numeric(Lcap)) STOP("'Lcap' must be numeric.")
  if (any(Lcap<=0)) STOP("All 'Lcap' must be positive.")
  if (!is.numeric(Ri)) STOP("'Ri' must be numeric.")
  if (any(Ri<=0)) STOP("All 'Ri' must be positive.")
  if (!is.numeric(Rcap)) STOP("'Rcap' must be numeric.")
  if (any(Rcap<=0)) STOP("All 'Rcap' must be positive.")
  if (!is.numeric(a)) STOP("'a' must be numeric.")
  if (length(a)>1) STOP("'a' must be a single value.")
  (Ri/Rcap)*(Lcap-a)+a
}

#' @rdname BCfunctions
#' @export
SPH <- function(Lcap,Ri,Rcap,A,B) {
  if (!is.numeric(Lcap)) STOP("'Lcap' must be numeric.")
  if (any(Lcap<=0)) STOP("All 'Lcap' must be positive.")
  if (!is.numeric(Ri)) STOP("'Ri' must be numeric.")
  if (any(Ri<=0)) STOP("All 'Ri' must be positive.")
  if (!is.numeric(Rcap)) STOP("'Rcap' must be numeric.")
  if (any(Rcap<=0)) STOP("All 'Rcap' must be positive.")
  if (!is.numeric(A)) STOP("'A' must be numeric.")
  if (length(A)>1) STOP("'A' must be a single value.")
  if (!is.numeric(B)) STOP("'B' must be numeric.")
  if (length(B)>1) STOP("'B' must be a single value.")
  (Ri/Rcap)*(Lcap+A/B)-A/B
}

#' @rdname BCfunctions
#' @export
BPH <- function(Lcap,Ri,Rcap,a,b) {
  if (!is.numeric(Lcap)) STOP("'Lcap' must be numeric.")
  if (any(Lcap<=0)) STOP("All 'Lcap' must be positive.")
  if (!is.numeric(Ri)) STOP("'Ri' must be numeric.")
  if (any(Ri<=0)) STOP("All 'Ri' must be positive.")
  if (!is.numeric(Rcap)) STOP("'Rcap' must be numeric.")
  if (any(Rcap<=0)) STOP("All 'Rcap' must be positive.")
  if (!is.numeric(a)) STOP("'a' must be numeric.")
  if (length(a)>1) STOP("'a' must be a single value.")
  if (!is.numeric(b)) STOP("'b' must be numeric.")
  if (length(b)>1) STOP("'b' must be a single value.")
  Lcap*((a+b*Ri)/(a+b*Rcap))
}

#' @rdname BCfunctions
#' @export
BiolInt <- function(Lcap,Ri,Rcap,L0p,R0p) {
  if (!is.numeric(Lcap)) STOP("'Lcap' must be numeric.")
  if (any(Lcap<=0)) STOP("All 'Lcap' must be positive.")
  if (!is.numeric(Ri)) STOP("'Ri' must be numeric.")
  if (any(Ri<=0)) STOP("All 'Ri' must be positive.")
  if (!is.numeric(Rcap)) STOP("'Rcap' must be numeric.")
  if (any(Rcap<=0)) STOP("All 'Rcap' must be positive.")
  if (!is.numeric(L0p)) STOP("'L0p' must be numeric.")
  if (length(L0p)>1) STOP("'L0p' must be a single value.")
  if (!is.numeric(R0p)) STOP("'R0p' must be numeric.")
  if (length(R0p)>1) STOP("'R0p' must be a single value.")
  Lcap+(Ri-Rcap)*(Lcap-L0p)/(Rcap-R0p)
}
#' @rdname BCfunctions
#' @export
ModFry <- function(Lcap,Ri,Rcap,L0p,R0p,a) {
  if (!is.numeric(Lcap)) STOP("'Lcap' must be numeric.")
  if (any(Lcap<=0)) STOP("All 'Lcap' must be positive.")
  if (!is.numeric(Ri)) STOP("'Ri' must be numeric.")
  if (any(Ri<=0)) STOP("All 'Ri' must be positive.")
  if (!is.numeric(Rcap)) STOP("'Rcap' must be numeric.")
  if (any(Rcap<=0)) STOP("All 'Rcap' must be positive.")
  if (!is.numeric(L0p)) STOP("'L0p' must be numeric.")
  if (length(L0p)>1) STOP("'L0p' must be a single value.")
  if (!is.numeric(R0p)) STOP("'R0p' must be numeric.")
  if (length(R0p)>1) STOP("'R0p' must be a single value.")
  if (!is.numeric(a)) STOP("'a' must be numeric.")
  if (length(a)>1) STOP("'a' must be a single value.")
  a + exp(log(L0p-a) + ((log(Lcap-a)-log(L0p-a))*(log(Ri)-log(R0p)))/(log(Rcap)-log(R0p)))
}
