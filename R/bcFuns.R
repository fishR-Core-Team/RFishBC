#' @title Creates a function for a specific model.
#'
#' @description Creates a function for a specific model based on definitions in Vigliola and Meekan (2009).
#'
#' @param BCM A single numeric between 1 and 22 or a string that indicates which model to use (based on numbers and names in Vigliola and Meekan (2009)).
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
#' @section IFAR Supplement: \url{https://derekogle.com/IFAR/supplements/backcalculation.html}
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
bcFuns <- function(BCM) {
  ## Check if BCM is viable and convert to number
  BCM <- iGetBCMethod(BCM)
  ## identify the functions
  if (BCM==1) {
      function(Lcap,Ri,Rcap,...,verbose=TRUE) {
        if (verbose) DONE("Using the Dahl-Lea model.")
        (Ri/Rcap)*Lcap
      }
  } else if (BCM==2) {
      function(Lcap,Ri,Rcap,a,...,verbose=TRUE) {
        if (verbose) DONE("Using the Fraser-Lee model with a=",a,".")
        a+(Lcap-a)*(Ri/Rcap)
      }
  } else if (BCM==3) {
      function(Lcap,Ri,Rcap,L0p,R0p,...,verbose=TRUE) {
        if (verbose) DONE("Using the Biological Intercept model with L0p=",L0p,
                          " and R0p=",R0p,".")
        Lcap+(Ri-Rcap)*(Lcap-L0p)/(Rcap-R0p)
      }
  } else if (BCM==4) {
      function(Lcap,Ri,Rcap,a,b,...,verbose=TRUE) {
        if (verbose) DONE("Using the Linear BPH with a=",a," and b=",b,".")
        (a+b*Ri)*Lcap/(a+b*Rcap)
      }
  } else if (BCM==5) {
      STOP("The BCM5 (TVG) function is not yet implemented.")
  } else if (BCM==6) {
      function(Lcap,Ri,Rcap,A,B,...,verbose=TRUE) {
        if (verbose) DONE("Using the Linear SPH model with A=",A," and B=",B,".")
        (Ri/Rcap*(A+B*Lcap)-A)/B
      }
  } else if (BCM==7) {
      function(Lcap,Ri,Rcap,Ai,Acap,A,B,C,...,verbose=TRUE) {
        if (verbose) DONE("Using the Age-Effects SPH model with A=",A," B=",B,
                          " and C=",C,".")
        -A/B+(Lcap+A/B+C/B*Acap)*Ri/Rcap-C/B*Ai
      }
  } else if (BCM==8) {
      function(Lcap,Ri,Rcap,Ai,Acap,a,b,c,...,verbose=TRUE) {
        if (verbose) DONE("Using the Age-Effects BPH model with a=",a," b=",b,
                          " and c=",c,".")
        (a+b*Ri+c*Ai)/(a+b*Rcap+c*Acap)*Lcap
      }
  } else if (BCM==9) {
      function(Lcap,Ri,Rcap,c,...,verbose=TRUE) {
        if (verbose) DONE("Using the Monastrysky model with c=",c,".")
        Lcap*((Ri/Rcap)^c)
      }
  } else if (BCM==10) { # Same as BCM9 but uses nls results to estimate c
      function(Lcap,Ri,Rcap,c,...,verbose=TRUE) {
        if (verbose) DONE("Using the non-linear Monastrysky BPH model with c=",c,".")
        Lcap*((Ri/Rcap)^c)
      }
  } else if (BCM==11) {
      function(Lcap,Ri,Rcap,C,...,verbose=TRUE) {
        if (verbose) DONE("Using the non-linear Monastrysky SPH model with C=",C,".")
        Lcap*((Ri/Rcap)^(1/C))
      }
  } else if (BCM==12) {
      function(Lcap,Ri,Rcap,L0p,R0,...,verbose=TRUE) {
        if (verbose) DONE("Using the Watanabe and Kuroki model with L0p=",L0p,
                          " and R0=",R0,".")
        exp(log(L0p) + ((log(Lcap)-log(L0p))*(log(Ri)-log(R0)))/(log(Rcap)-log(R0)))
      }
  } else if (BCM==13) {
      function(Lcap,Ri,Rcap,L0,R0,a,...,verbose=TRUE) {
        if (verbose) DONE("Using the Fry model with L0=",L0," R0=",R0,
                          " and a=",a,".")
        a + exp(log(L0-a) + ((log(Lcap-a)-log(L0-a))*(log(Ri)-log(R0)))/(log(Rcap)-log(R0))) 
      }
  } else if (BCM==14) {
      function(Lcap,Ri,Rcap,L0p,R0p,a,...,verbose=TRUE) {
        if (verbose) DONE("Using the Modified Fry model with L0p=",L0p,
                          " R0p=",R0p," and a=",a,".")
        a + exp(log(L0p-a) + ((log(Lcap-a)-log(L0p-a))*(log(Ri)-log(R0p)))/(log(Rcap)-log(R0p)))
      }
  } else if (BCM==15) {
      function(Lcap,Ri,Rcap,a,b,c,...,verbose=TRUE) {
        if (verbose) DONE("Using the Fry BPH model with a=",a," b=",b,
                          " and c=",c,".")
        (a+b*Ri^c)/(a+b*Rcap^c)*Lcap
      }
  } else if (BCM==16) {
      function(Lcap,Ri,Rcap,A,C,...,verbose=TRUE) {
        if (verbose) DONE("Using the Fry SPH model with A=",A," and C=",C,".")
        A+(Lcap-A)*((Ri/Rcap)^C)
      }
  } else if (BCM==17) {
      function(Lcap,Ri,Rcap,a,b,c,...,verbose=TRUE) {
        if (verbose) DONE("Using the Quadratic BPH model with a=",a," b=",b,
                          " and c=",c,".")
        (a+b*Ri+c*(Ri^2))/(a+b*Rcap+c*(Rcap^2))*Lcap
      }
  } else if (BCM==18) {
      function(Lcap,Ri,Rcap,A,B,C,...,verbose=TRUE) { 
        if (verbose) DONE("Using the Quadratic SPH model with A=",A," B=",B,
                          " and C=",C,".")
        cf1 <- B
        cf2 <- C
        Li <- numeric(length(Lcap))
        for (i in seq_along(Li)) {
          cf0 <- A-((Ri[i]/Rcap[i])*(A+B*Lcap[i]+C*Lcap[i]^2))
          roots <- Re(polyroot(c(cf0,cf1,cf2)))
          Li[i] <- roots[which(sign(roots)==1)]
        }
        Li
      }
  } else if (BCM==19) {
      function(Lcap,Ri,Rcap,a,...,verbose=TRUE) {
        if (verbose) DONE("Using the Polynomial BPH model.")
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
      function(Lcap,Ri,Rcap,A,...,verbose=TRUE) {
        if (verbose) DONE("Using the Polynomial SPH model.")
        # a must be a vector of coefficients from polynomial regression
        exps <- 0:(length(A)-1)
        Li <- numeric(length(Lcap))
        for (i in seq_along(Li)) {
          if (Ri[i]==Rcap[i]) { Li[i] <- Lcap[i] }
          else {
            cf <- A
            cf[1] <- cf[1] - Ri[i]/Rcap[i]*sum(A*Lcap[i]^exps)
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
      function(Lcap,Ri,Rcap,a,b,...,verbose=TRUE) {
        if (verbose) DONE("Using the Exponential BPH model with a=",a,
                          " and b=",b,".")
        exp(a+b*Ri)/exp(a+b*Rcap)*Lcap
      }
  } else if (BCM==22) {
      function(Lcap,Ri,Rcap,A,...,verbose=TRUE) {
        if (verbose) DONE("Using the Exponential SPH model with A=",A,".")
        exp(A+(log(Lcap)-A)*Ri/Rcap)
      }
  } 
}
