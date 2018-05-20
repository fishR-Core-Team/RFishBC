backCalc <- function(dat,lencap,
                     method=c("DahlLea","FraserLee","SPH","BPH","BiolInt","ModFry"),
                     a=NULL,L0p=NULL,R0p=NULL,
                     inFormat=c("long","wide"),outFormat=inFormat,
                     deletePlusGrowth=TRUE) {
  ## Handle default arguments
  method <- match.arg(method)
  if (method=="DahlLea") NOTE("You are using the 'Dahl-Lea' method to back-calculate lengths at a previous age.\n   While this method is the default, it is known to produce poor estimates of lengths\n   at previous ages. Please consider using one of the other methods.")
  inFormat <- match.arg(inFormat)
  
  ## Some error checking
  if (method %in% c("BiolInt","ModFry")) {
    if (is.null(L0p)) STOP("Must set 'L0p' when 'method' is ",method,".")
    if (is.null(R0p)) STOP("Must set 'R0p' when 'method' is ",method,".")
  }
  
  ## Trying to avoid global bindings issues
  ann <- rad <- bclen <- NULL
  ## Change wide to long (if need to)
  if (inFormat=="wide") {
    ## Convert wide to long
    nms <- names(dat)
    rads <- nms[grepl("rad",nms) & !grepl("radcap",nms)]
    dat <- tidyr::gather(dat,key=ann,value=rad,rads[1]:rads[length(rads)])
    ## Change annuli labels into annuli numbers
    dat$ann <- as.numeric(stringr::str_replace_all(dat$ann,"rad",""))
    ## Remove annuli where the radius was missing
    dat <- dat[!is.na(dat$rad),]
    ## Delete plus-growth if asked to do so
    if (deletePlusGrowth) dat <- dat[dat$ann<=dat$agecap,]
    ## Sort by id and then ann number
    dat <- dat[order(dat$id,dat$ann),]
  }
  
  ## Perform relevant regressions if needed
  ### Get data (one lencap and one radcap per id) for regressions
  if (method %in% c("FraserLee","SPH","BPH","ModFry")) {
    regdat <- dat[dat$ann==1,]
    regLcap <- regdat[,rlang::quo_name(rlang::enquo(lencap))]
    regRcap <- regdat$radcap
    regLR <- stats::lm(regLcap~regRcap)
    if (is.null(a)) a <- stats::coef(regLR)[[1]]
    b <- stats::coef(regLR)[[2]]
    regRL <- stats::lm(regRcap~regLcap)
    A <- stats::coef(regRL)[[1]]
    B <- stats::coef(regRL)[[2]]
  }
  
  ## Perform the back-calculation
  Lcap <- dat[,rlang::quo_name(rlang::enquo(lencap))]
  Ri <- dat$rad
  Rcap <- dat$radcap
  dat$bclen <- switch(method,
                    "DahlLea" = DahlLea(Lcap,Ri,Rcap),
                    "FraserLee" = FraserLee(Lcap,Ri,Rcap,a),
                    "SPH" = SPH(Lcap,Ri,Rcap,A,B),
                    "BPH" = BPH(Lcap,Ri,Rcap,a,b),
                    "BiolInt" = BiolInt(Lcap,Ri,Rcap,L0p,R0p),
                    "ModFry" = ModFry(Lcap,Ri,Rcap,L0p,R0p,a))
  
  ## Return the data
  ### Remove radii information
  dat <- dat[,!grepl("rad",names(dat))]
  ### Convert to wide format (if asked to do so)
  if (outFormat=="wide") {
    dat <- tidyr::spread(dat,key=ann,value=bclen,sep="len")
    names(dat) <- gsub("ann","",names(dat))
  }
  ### return the data
  dat
}
