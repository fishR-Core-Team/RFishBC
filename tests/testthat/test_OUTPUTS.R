context("RFishBC OUTPUTS")

source("EXS_collectRadii.R")
source("EXS_growthUtils.R")

test_that("RFBCoptions() important defaults",{
  expect_true(is.null(RFBCoptions()$reading))
  expect_true(is.null(RFBCoptions()$description))
  expect_true(is.null(RFBCoptions()$suffix))
  expect_true(is.null(RFBCoptions()$edgeIsAnnulus))
  expect_true(RFBCoptions()$sepWindow)
  expect_true(RFBCoptions()$popID)
  expect_true(RFBCoptions()$snap2Transect)
  expect_equal(RFBCoptions()$pch.sel,20)
  expect_equal(RFBCoptions()$pch.show,19)
  expect_true(RFBCoptions()$showInfo)
  expect_true(RFBCoptions()$showAnnuliLabels)
})



test_that("digitizeRadii() results without scale-bar",{
  expect_s3_class(dat1,"RFishBC")
  expect_type(dat1,"list")
  expect_equal(names(dat1),c("image","datanm","description","edgeIsAnnulus",
                             "snap2Transect","scalingFactor","sfSource","sbPts",
                             "sbLength","slpTransect","intTransect","slpPerpTransect",
                             "windowSize","pixW2H","pts","radii"))
  expect_type(dat1$image,"character")
  expect_type(dat1$datanm,"character")
  expect_null(dat1$description)
  expect_type(dat1$edgeIsAnnulus,"logical")
  expect_equal(length(dat1$edgeIsAnnulus),1)
  expect_type(dat1$snap2Transect,"logical")
  expect_equal(length(dat1$snap2Transect),1)
  expect_type(dat1$scalingFactor,"double")
  expect_equal(length(dat1$scalingFactor),1)
  expect_type(dat1$sfSource,"character")
  expect_equal(length(dat1$sfSource),1)
  expect_equal(dat1$sfSource,"Provided")
  expect_null(dat1$sbPts)
  expect_null(dat1$sbLength)
  expect_type(dat1$slpTransect,"double")
  expect_equal(length(dat1$slpTransect),1)
  expect_type(dat1$intTransect,"double")
  expect_equal(length(dat1$intTransect),1)
  expect_type(dat1$slpPerpTransect,"double")
  expect_equal(length(dat1$slpPerpTransect),1)
  expect_true(dat1$slpTransect*dat1$slpPerpTransect<0)
  expect_type(dat1$windowSize,"double")
  expect_equal(length(dat1$windowSize),2)
  expect_type(dat1$pixW2H,"double")
  expect_type(dat1$pts,"list")
  expect_s3_class(dat1$pts,"data.frame")
  expect_equal(names(dat1$pts),c("x","y"))
  expect_type(dat1$radii,"list")
  expect_s3_class(dat1$radii,"data.frame")
  expect_equal(names(dat1$radii),c("id","reading","agecap","ann","rad","radcap"))
})



test_that("digitizeRadii() results with scale-bar",{
  expect_s3_class(dat2,"RFishBC")
  expect_type(dat2,"list")
  expect_equal(names(dat2),c("image","datanm","description","edgeIsAnnulus",
                            "snap2Transect","scalingFactor","sfSource","sbPts",
                            "sbLength","slpTransect","intTransect","slpPerpTransect",
                            "windowSize","pixW2H","pts","radii"))
  expect_type(dat2$image,"character")
  expect_type(dat2$datanm,"character")
  expect_type(dat2$description,"character")
  expect_type(dat2$edgeIsAnnulus,"logical")
  expect_equal(length(dat2$edgeIsAnnulus),1)
  expect_type(dat2$snap2Transect,"logical")
  expect_equal(length(dat2$snap2Transect),1)
  expect_type(dat2$scalingFactor,"double")
  expect_equal(length(dat2$scalingFactor),1)
  expect_type(dat2$sfSource,"character")
  expect_equal(length(dat2$sfSource),1)
  expect_equal(dat2$sfSource,"scaleBar")
  expect_type(dat2$sbPts,"list")
  expect_s3_class(dat2$sbPts,"data.frame")
  expect_equal(names(dat2$sbPts),c("x","y"))
  expect_equal(nrow(dat2$sbPts),2)
  expect_type(dat2$sbLength,"double")
  expect_equal(length(dat2$sbLength),1)
  expect_type(dat2$slpTransect,"double")
  expect_equal(length(dat2$slpTransect),1)
  expect_type(dat2$intTransect,"double")
  expect_equal(length(dat2$intTransect),1)
  expect_type(dat2$slpPerpTransect,"double")
  expect_equal(length(dat2$slpPerpTransect),1)
  expect_true(dat2$slpTransect*dat2$slpPerpTransect<0)
  expect_type(dat2$windowSize,"double")
  expect_equal(length(dat2$windowSize),2)
  expect_type(dat2$pixW2H,"double")
  expect_type(dat2$pts,"list")
  expect_s3_class(dat2$pts,"data.frame")
  expect_equal(names(dat2$pts),c("x","y"))
  expect_type(dat2$radii,"list")
  expect_s3_class(dat2$radii,"data.frame")
  expect_equal(names(dat2$radii),c("id","reading","agecap","ann","rad","radcap"))
})



test_that("combineData() results",{
  ## Individual files
  ### Long Format
  #### Deleting plus growth
  tmp <- combineData("Scale_1_DHO.rds")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),5)
  tmp <- combineData("Oto140306_DHO.rds")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  #### Not deleting plus growth
  tmp <- combineData("Scale_1_DHO.rds",deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),6)
  tmp <- combineData("Oto140306_DHO.rds",deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  ### Wide Format
  #### Deleting plus growth
  tmp <- combineData("Scale_1_DHO.rds",outFormat="wide")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "rad1","rad2","rad3","rad4","rad5"))
  expect_equal(nrow(tmp),1)
  #### Not deleting plus growth
  tmp <- combineData("Scale_1_DHO.rds",outFormat="wide",deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "rad1","rad2","rad3","rad4","rad5","rad6"))
  expect_equal(nrow(tmp),1)
  
  ## Multiple files
  ### Long Format
  #### Deleting plus growth
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_DHO2.rds"))
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),10)
  #### Not deleting plus growth
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_DHO2.rds"),deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),12)
  ### Wide Format
  #### Deleting plus growth
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_DHO2.rds"),outFormat="wide")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "rad1","rad2","rad3","rad4","rad5"))
  expect_equal(nrow(tmp),2)
  #### Not deleting plus growth
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_DHO2.rds"),
                     outFormat="wide",deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "rad1","rad2","rad3","rad4","rad5","rad6"))
  expect_equal(nrow(tmp),2)
})



test_that("listFiles() output",{
  expect_equal(listFiles("R"),c("EXS_collectRadii.R","EXS_growthUtils.R",
                                "test_MESSAGES.R","test_OUTPUTS.R"))
  expect_equal(listFiles("jpg"),c("Oto140306.jpg","Scale_1.jpg",
                                  "Scale_2.jpg","small_ex.jpg"))
  expect_equal(listFiles("jpg",other="Scale"),c("Scale_1.jpg","Scale_2.jpg"))
  expect_equal(listFiles("rds",other="DHO2"),
               c("Oto140306_DHO2.rds","Scale_1_DHO2.rds"))
})



test_that("bcFuns() output types",{
  ## List all choices for bcFuns() (TVG is not included because it
  ## is not yet implemented)
  tmp <- c("DALE","FRALE","BI","LBI","BPH","LBPH","TVG","SPH","LSPH",
           "AE","AESPH","AEBPH","MONA","MONA-BPH","MONA-SPH","WAKU",
           "FRY","MF","ABI","FRY-BPH","ABPH","FRY-SPH","ASPH","QBPH",
           "QSPH","PBPH","PSPH","EBPH","ESPH")
  tmp <- tmp[-7]
  ## Do all choices (by number and name) return a function
  for (i in c(1:4,6:22)) expect_is(bcFuns(i),"function")
  for (i in tmp) expect_is(bcFuns(i),"function")
})


test_that("backCalc() output types",{
  ## Test that a data.frame with proper variable names is returned for common BCMs
  wide.nms <- c("id","species","lake","gear","yearcap","lencap","reading","agecap",
                "len1","len2","len3","len4","len5","len6","len7","len8","len9")
  #### ... wide format
  expect_output(backCalc(SMBassWB,lencap,BCM="DALE",
                          inFormat="wide",digits=0),"Dahl-Lea")
  capture.output(SMBassWB_DLW <- backCalc(SMBassWB,lencap,BCM="DALE",
                                          inFormat="wide",digits=0))
  expect_s3_class(SMBassWB_DLW,"data.frame")
  expect_equal(names(SMBassWB_DLW),wide.nms)
  expect_equal(nrow(SMBassWB_DLW),181)
  
  expect_output(backCalc(SMBassWB,lencap,BCM="FRALE",
                         inFormat="wide",digits=0),"Fraser-Lee")
  capture.output(SMBassWB_FLW <- backCalc(SMBassWB,lencap,BCM="FRALE",
                                          inFormat="wide",digits=0))
  expect_s3_class(SMBassWB_FLW,"data.frame")
  expect_equal(names(SMBassWB_FLW),wide.nms)
  expect_equal(nrow(SMBassWB_FLW),181)
  
  expect_output(backCalc(SMBassWB,lencap,BCM="SPH",
                         inFormat="wide",digits=0),"Linear SPH")
  capture.output(SMBassWB_SPHW <- backCalc(SMBassWB,lencap,BCM="SPH",
                                           inFormat="wide",digits=0))
  expect_s3_class(SMBassWB_SPHW,"data.frame")
  expect_equal(names(SMBassWB_SPHW),wide.nms)
  expect_equal(nrow(SMBassWB_SPHW),181)
  
  expect_output(backCalc(SMBassWB,lencap,BCM="BPH",
                         inFormat="wide",digits=0),"Linear BPH")
  capture.output(SMBassWB_BPHW <- backCalc(SMBassWB,lencap,BCM="BPH",
                                           inFormat="wide",digits=0))
  expect_s3_class(SMBassWB_BPHW,"data.frame")
  expect_equal(names(SMBassWB_BPHW),wide.nms)
  expect_equal(nrow(SMBassWB_BPHW),181)

  long.nms <- c("id","species","lake","gear","yearcap","lencap",
                "reading","agecap","ann","bclen")
  #### ... long format
  expect_output(backCalc(SMBassWB,lencap,BCM="DALE",
                         inFormat="wide",outFormat="long",digits=0),"Dahl-Lea")
  capture.output(SMBassWB_DLL <- backCalc(SMBassWB,lencap,BCM="DALE",
                                          inFormat="wide",outFormat="long",digits=0))
  expect_s3_class(SMBassWB_DLL,"data.frame")
  expect_equal(names(SMBassWB_DLL),long.nms)
  expect_equal(nrow(SMBassWB_DLL),767)
  
  expect_output(backCalc(SMBassWB,lencap,BCM="FRALE",
                         inFormat="wide",outFormat="long",digits=0),"Fraser-Lee")
  capture.output(SMBassWB_FLL <- backCalc(SMBassWB,lencap,BCM="FRALE",
                                          inFormat="wide",outFormat="long",digits=0))
  expect_s3_class(SMBassWB_FLL,"data.frame")
  expect_equal(names(SMBassWB_FLL),long.nms)
  expect_equal(nrow(SMBassWB_FLL),767)
  
  expect_output(backCalc(SMBassWB,lencap,BCM="SPH",
                         inFormat="wide",outFormat="long",digits=0),"Linear SPH")
  capture.output(SMBassWB_SPHL <- backCalc(SMBassWB,lencap,BCM="SPH",
                                           inFormat="wide",outFormat="long",digits=0))
  expect_s3_class(SMBassWB_SPHL,"data.frame")
  expect_equal(names(SMBassWB_SPHL),long.nms)
  expect_equal(nrow(SMBassWB_SPHL),767)
  
  expect_output(backCalc(SMBassWB,lencap,BCM="BPH",
                         inFormat="wide",outFormat="long",digits=0),"Linear BPH")
  capture.output(SMBassWB_BPHL <- backCalc(SMBassWB,lencap,BCM="BPH",
                                           inFormat="wide",outFormat="long",digits=0))
  expect_s3_class(SMBassWB_BPHL,"data.frame")
  expect_equal(names(SMBassWB_BPHL),long.nms)
  expect_equal(nrow(SMBassWB_BPHL),767)
})


test_that("backCalc() output values",{
  ## First fish, first increment
  tmp <- SMBassWB[1,]
  ### Dahl-Lea
  capture.output(out <- backCalc(tmp,lencap,BCM="DALE",inFormat="wide",
                                 outFormat="long",digits=1))
  exp1 <- round(with(tmp,lencap*rad1/radcap),1)
  expect_equal(out$bclen[1],exp1)
  ### Fraser-Lee
  a <- aStandard("Smallmouth Bass")
  tmp <- SMBassWB[1,]
  capture.output(out <- backCalc(tmp,lencap,BCM="FRALE",a=a,inFormat="wide",
                                 outFormat="long",digits=1))
  exp1 <- round(with(tmp,(lencap-a)*rad1/radcap+a),1)
  expect_equal(out$bclen[1],exp1)
  
  ## Last fish, first and ninth increment
  tmp <- SMBassWB[181,]
  ### Dahl-Lea
  capture.output(out <- backCalc(tmp,lencap,BCM="DALE",inFormat="wide",
                                 outFormat="long",digits=1))
  exp1 <- round(with(tmp,lencap*rad1/radcap),1)
  expect_equal(out$bclen[1],exp1)
  exp9 <- round(with(tmp,lencap*rad9/radcap),1)
  expect_equal(out$bclen[9],exp9)
  ### Fraser-Lee
  capture.output(out <- backCalc(tmp,lencap,BCM="FRALE",a=a,inFormat="wide",
                                 outFormat="long",digits=1))
  exp1 <- round(with(tmp,(lencap-a)*rad1/radcap+a),1)
  expect_equal(out$bclen[1],exp1)
  exp9 <- round(with(tmp,(lencap-a)*rad9/radcap+a),1)
  expect_equal(out$bclen[9],exp9)
})


test_that("aStandard() outputs",{
  expect_type(aStandard("Bluegill"),"integer")
  expect_equal(aStandard("Bluegill"),20)
})


test_that("gConvert() output",{
  ## Actually constructs increments from radii ... no plus-growth
  tmp <- gConvert(bctmp,in.pre="anu")
  expect_equal(names(tmp),c(names(bctmp)[1:4],paste0("inc",1:3)))
  expect_equivalent(as.numeric(tmp[1,]),
                    as.numeric(cbind(bctmp[1,1:4],1,NA,NA)))
  expect_equivalent(as.numeric(tmp[2,]),
                    as.numeric(cbind(bctmp[2,1:4],1,1,NA)))
  expect_equivalent(as.numeric(tmp[3,]),
                    as.numeric(cbind(bctmp[3,1:4],1,1,1)))
  ## Actually re-constructs radii from increments ... no plus-growth
  tmp <- gConvert(tmp,in.pre="inc",out.type="rad",out.pre="anu")
  expect_equal(bctmp,tmp)
  ## Actually constructs increments from radii ... with plus-growth
  tmp <- gConvert(bctmp2,in.pre="anu")
  expect_equal(names(tmp),c(names(bctmp2)[1:4],paste0("inc",1:4)))
  expect_equivalent(as.numeric(tmp[1,]),
                    as.numeric(cbind(bctmp2[1,1:4],1,0.1,NA,NA)))
  expect_equivalent(as.numeric(tmp[2,]),
                    as.numeric(cbind(bctmp2[2,1:4],1,1,0.1,NA)))
  expect_equivalent(as.numeric(tmp[3,]),
                    as.numeric(cbind(bctmp2[3,1:4],1,1,1,0.1)))
  ## Actually re-constructs radii from increments ... no plus-growth
  tmp <- gConvert(tmp,in.pre="inc",out.type="rad",out.pre="anu")
  expect_equal(bctmp2,tmp)
})

test_that("addRadCap() output",{
  ## Convert radii to increments ... no plus-growth
  tmp <- gConvert(bctmp,in.pre="anu")
  tmp <- addRadCap(tmp,in.pre="inc",var.name="newRadCap")
  expect_equal(tmp$radcap,tmp$newRadCap)
  ## Convert radii to increments ... plus-growth
  tmp <- gConvert(bctmp2,in.pre="anu")
  tmp <- addRadCap(tmp,in.pre="inc",var.name="newRadCap")
  expect_equal(tmp$radcap,tmp$newRadCap)
})
