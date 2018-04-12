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
#' @return A function that can be used to predict length at previous age (Li) given length-at-capture (Lc), hard-part radius-at-age i (Ri), and hard-part radius-at-capture (Rc).  In addition, some functions/models may require the previous age (agei) and the age-at-capture (agec), certain parameters related to the biological intercept (R0p & L0p), or certain parameters estimated from various regression models (a,b,c,A,B,C).  See source for more information.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Supplement: \url{http://derekogle.com/IFAR/supplements/backcalculation/}
#'
#' @references
#' Vigliola, L. and M.G. Meekan.  2009.  The back-calculation of fish growth from otoliths.  pp. 174-211.  in B.S. Green et al. (editors).  Tropical Fish Otoliths: Information for Assessment, Management and Ecology.  Review: Methods and Technologies in Fish Biology and Fisheries 11.  Springer.  [Was (is?) available from https://www.researchgate.net/publication/226394736_The_Back-Calculation_of_Fish_Growth_From_Otoliths.]
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
  if (missing(BCM)) stop("A back-calculation function must be chosen with 'BCM'",call.=FALSE)
  if (length(BCM)>1) stop("Only one value may be given to 'BCM'",
                          call.=FALSE)
  if (is.numeric(BCM)) {
    ## Function declared numerically
    if (BCM<1 | BCM>22) stop("BCM number must be between 1 and 22 inclusive.",call.=FALSE)
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
      stop(msg,call.=FALSE)
    } else {
      # All is good ... convert string to numeric
      BCM <- BCM.nums[which(BCM.nms %in% BCM)]
    }
  }

  ## identify the functions
  if (BCM==1) {
      if (verbose) message("You chose the BCM1 or DALE model.\n\n")
      function(Lc,Ri,Rc) { (Ri/Rc)*Lc }
  } else if (BCM==2) {
      if (verbose) message("You chose the BCM2 or FRALE model.\n\n")
      function(Lc,Ri,Rc,a) { a+(Lc-a)*(Ri/Rc) }
  } else if (BCM==3) {
      if (verbose) message("You chose the BCM3, BI, or LBI model.\n\n")
      function(Lc,Ri,Rc,L0p,R0p) { Lc+(Ri-Rc)*(Lc-L0p)/(Rc-R0p) }
  } else if (BCM==4) {
      if (verbose) message("You chose the BCM4 or LBPH model.\n\n")
      function(Lc,Ri,Rc,a,b) { (a+b*Ri)*Lc/(a+b*Rc) }
  } else if (BCM==5) {
      stop("The BCM5 (TVG) function is not yet implemented.",call.=FALSE)
  } else if (BCM==6) {
      if (verbose) message("You chose the BCM6 or LSPH model.\n\n")
      function(Lc,Ri,Rc,A,B) { (Ri/Rc*(A+B*Lc)-A)/B }
  } else if (BCM==7) {
      if (verbose) message("You chose the BCM7, AE, or AESPH model.\n\n")
      function(Lc,Ri,Rc,agei,agec,a,b,c) { -a/b+(Lc+a/b+c/b*agec)*Ri/Rc-c/b*agei }
  } else if (BCM==8) {
      if (verbose) message("You chose the BCM8 or AEBPH model.\n\n")
      function(Lc,Ri,Rc,agei,agec,A,B,C) { (A+B*Ri+C*agei)/(A+B*Rc+C*agec)*Lc }
  } else if (BCM==9) {
      if (verbose) message("You chose the BCM9 or MONA model.\n\n")
      function(Lc,Ri,Rc,c) { Lc*((Ri/Rc)^c) }
  } else if (BCM==10) {
      if (verbose) message("You chose the BCM10 or MONA-BPH model.\n\n")
      # Same as BCM9 but uses nls results to estimate c
      function(Lc,Ri,Rc,c) { Lc*((Ri/Rc)^c) }
  } else if (BCM==11) {
      if (verbose) message("You chose the BCM11 or MONA-SPH model.\n\n")
      function(Lc,Ri,Rc,C) { Lc*((Ri/Rc)^(1/C)) }
  } else if (BCM==12) {
      if (verbose) message("You chose the BCM12 or WAKU model.\n\n")
      function(Lc,Ri,Rc,L0p,R0) {
        exp(log(L0p) + ((log(Lc)-log(L0p))*(log(Ri)-log(R0)))/(log(Rc)-log(R0)))
      }
  } else if (BCM==13) {
      if (verbose) message("You chose the BCM13 or FRY model.\n\n")
      function(Lc,Ri,Rc,L0,R0,a) {
        a + exp(log(L0-a) + ((log(Lc-a)-log(L0-a))*(log(Ri)-log(R0)))/(log(Rc)-log(R0))) 
      }
  } else if (BCM==14) {
      if (verbose) message("You chose the BCM14, MF, or ABI model.\n\n")
      function(Lc,Ri,Rc,L0p,R0p,a) {
        a + exp(log(L0p-a) + ((log(Lc-a)-log(L0p-a))*(log(Ri)-log(R0p)))/(log(Rc)-log(R0p)))
      }
  } else if (BCM==15) {
      if (verbose) message("You chose the BCM15, FRY-BPH, or ABPH model.\n\n")
      function(Lc,Ri,Rc,a,b,c) { (a+b*Ri^c)/(a+b*Rc^c)*Lc }
  } else if (BCM==16) {
      if (verbose) message("You chose the BCM16, FRY-SPH, or ASPH model.\n\n")
      function(Lc,Ri,Rc,a,b,c) { a+(Lc-a)*((Ri/Rc)^c) }
  } else if (BCM==17) {
      if (verbose) message("You chose the BCM17 or QBPH model.\n\n")
      function(Lc,Ri,Rc,a,b,c) { (a+b*Ri+c*(Ri^2))/(a+b*Rc+c*(Rc^2))*Lc }
  } else if (BCM==18) {
      if (verbose) message("You chose the BCM18 or QSPH model.\n\n")
      function(Lc,Ri,Rc,a,b,c) { 
        cf1 <- b
        cf2 <- c
        Li <- numeric(length(Lc))
        for (i in seq_along(Li)) {
          cf0 <- a-((Ri[i]/Rc[i])*(a+b*Lc[i]+c*Lc[i]^2))
          roots <- Re(polyroot(c(cf0,cf1,cf2)))
          Li[i] <- roots[which(sign(roots)==1)]
        }
        Li
      }
  } else if (BCM==19) {
      if (verbose) message("You chose the BCM19 or PBPH model.\n\n")
      function(Lc,Ri,Rc,a) {
        # a must be a vector of coefficients from polynomial regression
        exps <- 0:(length(a)-1)
        Li <- numeric(length(Lc))
        for (i in seq_along(Lc)) {
          num <- sum( a*Ri[i]^exps )
          denom <- sum( a*Rc[i]^exps )
          Li[i] <- num/denom*Lc[i]
        }
        Li
      }
  } else if (BCM==20) { 
      # Note that this is a function that should be used when finding
      #   a root, not to actually back-calculate
      if (verbose) message("You chose the BCM20 or PSPH model.\n\n")
      function(Lc,Ri,Rc,a) {
        # a must be a vector of coefficients from polynomial regression
        exps <- 0:(length(a)-1)
        Li <- numeric(length(Lc))
        for (i in seq_along(Li)) {
          if (Ri[i]==Rc[i]) { Li[i] <- Lc[i] }
          else {
            cf <- a
            cf[1] <- cf[1] - Ri[i]/Rc[i]*sum(a*Lc[i]^exps)
            roots <- Re(polyroot(cf))
            # find only positive roots
            roots <- roots[which(sign(roots)==1)]
            # only find root less than lencap
            roots <- roots[which(roots<=Lc[i])]
            ifelse(length(roots)!=1,Li[i] <- NA,Li[i] <- roots)
          }
        }
        Li
      }      
  } else if (BCM==21) {
      if (verbose) message("You chose the BCM21 or EBPH model.\n\n")
      function(Lc,Ri,Rc,a,b) { exp(a+b*Ri)/exp(a+b*Rc)*Lc }
  } else if (BCM==22) {
      if (verbose) message("You chose the BCM22 or ESPH model.\n\n")
      function(Lc,Ri,Rc,a) { exp(a+(log(Lc)-a)*Ri/Rc) }
  } 
}
