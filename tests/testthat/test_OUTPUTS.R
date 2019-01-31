context("RFishBC OUTPUTS")

source("EXS_collectRadii.R")
source("EXS_growthUtils.R")

test_that("RFBCoptions() reset works",{
  tmp <- RFBCoptions()
  RFBCoptions(popID=FALSE)
  expect_false(RFBCoptions()$popID)
  expect_equal(tmp,RFBCoptions(reset=TRUE))
})

test_that("RFBCoptions() important defaults",{
  expect_true(is.null(RFBCoptions()$reading))
  expect_true(is.null(RFBCoptions()$description))
  expect_true(is.null(RFBCoptions()$suffix))
  expect_true(is.null(RFBCoptions()$edgeIsAnnulus))
  expect_equal(RFBCoptions()$deviceType,"default")
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
                             "sbLength","sbUnits","slpTransect","intTransect",
                             "slpPerpTransect","windowSize","pixW2H","pts","radii"))
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
  expect_null(dat1$sbUnits)
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
                            "sbLength","sbUnits","slpTransect","intTransect",
                            "slpPerpTransect","windowSize","pixW2H","pts","radii"))
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
  expect_type(dat2$sbUnits,"character")
  expect_equal(length(dat2$sbUnits),1)
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
  ##### Radii
  tmp <- combineData("Scale_1_DHO.rds")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),5)
  tmp <- combineData("Oto140306_DHO.rds")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  ##### Increments
  tmp <- combineData("Scale_1_DHO.rds",typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","radcap","inc"))
  expect_equal(nrow(tmp),5)
  tmp <- combineData("Oto140306_DHO.rds",typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","radcap","inc"))
  #### Not deleting plus growth
  ##### Radii
  tmp <- combineData("Scale_1_DHO.rds",deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),6)
  tmp <- combineData("Oto140306_DHO.rds",deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  ##### Increments
  tmp <- combineData("Scale_1_DHO.rds",deletePlusGrowth=FALSE,typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","radcap","inc"))
  expect_equal(nrow(tmp),6)
  tmp <- combineData("Oto140306_DHO.rds",deletePlusGrowth=FALSE,typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","radcap","inc"))
  ### Wide Format
  #### Deleting plus growth
  ##### Radii
  tmp <- combineData("Scale_1_DHO.rds",formatOut="wide")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "rad1","rad2","rad3","rad4","rad5"))
  expect_equal(nrow(tmp),1)
  ##### Increments
  tmp <- combineData("Scale_1_DHO.rds",formatOut="wide",typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "inc1","inc2","inc3","inc4","inc5"))
  expect_equal(nrow(tmp),1)
  #### Not deleting plus growth
  ##### Radii
  tmp <- combineData("Scale_1_DHO.rds",formatOut="wide",deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "rad1","rad2","rad3","rad4","rad5","rad6"))
  expect_equal(nrow(tmp),1)
  ##### Increments
  tmp <- combineData("Scale_1_DHO.rds",formatOut="wide",
                     deletePlusGrowth=FALSE,typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "inc1","inc2","inc3","inc4","inc5","inc6"))
  expect_equal(nrow(tmp),1)
  
  ## Multiple files
  ### Long Format
  #### Deleting plus growth
  ##### Radii
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds"))
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),10)
  ##### Increments
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds"),typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","radcap","inc"))
  expect_equal(nrow(tmp),10)
  #### Not deleting plus growth
  ##### Radii
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds"),deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),12)
  ##### Increments
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds"),
                     deletePlusGrowth=FALSE,typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","radcap","inc"))
  expect_equal(nrow(tmp),12)
  ### Wide Format
  #### Deleting plus growth
  ##### Radii
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds"),formatOut="wide")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "rad1","rad2","rad3","rad4","rad5"))
  expect_equal(nrow(tmp),2)
  ##### Increments
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds"),
                     formatOut="wide",typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "inc1","inc2","inc3","inc4","inc5"))
  expect_equal(nrow(tmp),2)
  #### Not deleting plus growth
  ##### Radii
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds"),
                     formatOut="wide",deletePlusGrowth=FALSE)
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "rad1","rad2","rad3","rad4","rad5","rad6"))
  expect_equal(nrow(tmp),2)
  ##### Increments
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds"),
                     formatOut="wide",deletePlusGrowth=FALSE,typeOut="inc")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","radcap",
                            "inc1","inc2","inc3","inc4","inc5","inc6"))
  expect_equal(nrow(tmp),2)
})



test_that("listFiles() output",{
  expect_equal(listFiles("R"),c("EXS_collectRadii.R","EXS_growthUtils.R",
                                "test_MESSAGES.R","test_OUTPUTS.R"))
  expect_equal(listFiles("jpg"),c("Oto140306.jpg","Scale_1.jpg",
                                  "Scale_2.jpg","small_ex.jpg"))
  expect_equal(listFiles("jpg",other="Scale"),c("Scale_1.jpg","Scale_2.jpg"))
  expect_equal(listFiles("rds",other="OHD"),
               c("Oto140306_OHD.rds","Scale_1_OHD.rds"))
})



test_that("getID() output",{
  tmp <- listFiles("jpg",other="Scale")
  expect_equal(getID(tmp),c("1","2"))
  expect_type(getID(tmp),"character")
  tmp <- c("PWF_MI345.tiff","PWF_WI567.tiff")
  expect_equal(getID(tmp),c("MI345","WI567"))
  expect_type(getID(tmp),"character")
  tmp <- c("LKT_oto_23.jpg","LKT_finray_34.jpg")
  expect_equal(getID(tmp),c("23","34"))
  tmp <- c("1_Scale.jpg","2_Scale.jpg")
  expect_equal(getID(tmp,IDpattern="\\_.*"),c("1","2"))
  tmp <- c("Junk_1_Scale.jpg","Junk_2_Scale.jpg")
  expect_equal(getID(tmp,IDpattern=".*\\_(.+?)\\_.*",IDreplace="\\1"),c("1","2"))
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


test_that("bcFuns() function results messages",{
  tmp <- bcFuns(1)
  expect_true(any(grepl("Dahl-Lea",capture.output(tmp(1,2,3,verbose=TRUE)))))
  tmp <- bcFuns(2)
  expect_true(any(grepl("Fraser-Lee",capture.output(tmp(1,2,3,4,verbose=TRUE)))))
  tmp <- bcFuns(3)
  expect_true(any(grepl("Biological Intercept",capture.output(tmp(1,2,3,4,5,verbose=TRUE)))))
  tmp <- bcFuns(4)
  expect_true(any(grepl("Linear BPH",capture.output(tmp(1,2,3,4,5,verbose=TRUE)))))
  tmp <- bcFuns(6)
  expect_true(any(grepl("Linear SPH",capture.output(tmp(1,2,3,4,5,verbose=TRUE)))))
  tmp <- bcFuns(7)
  expect_true(any(grepl("Age-Effects SPH",capture.output(tmp(1,2,3,4,5,6,7,8,verbose=TRUE)))))
  tmp <- bcFuns(8)
  expect_true(any(grepl("Age-Effects BPH",capture.output(tmp(1,2,3,4,5,6,7,8,verbose=TRUE)))))
  tmp <- bcFuns(9)
  expect_true(any(grepl("Monastrysky",capture.output(tmp(1,2,3,4,verbose=TRUE)))))
  tmp <- bcFuns(10)
  expect_true(any(grepl("non-linear Monastrysky BPH",capture.output(tmp(1,2,3,4,verbose=TRUE)))))
  tmp <- bcFuns(11)
  expect_true(any(grepl("non-linear Monastrysky SPH",capture.output(tmp(1,2,3,4,5,verbose=TRUE)))))
  tmp <- bcFuns(12)
  expect_true(any(grepl("Watanabe and Kuroki",capture.output(tmp(1,2,3,4,5,verbose=TRUE)))))
  tmp <- bcFuns(13)
  expect_true(any(grepl("Fry",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(14)
  expect_true(any(grepl("Modified Fry",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(15)
  expect_true(any(grepl("Fry BPH",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(16)
  expect_true(any(grepl("Fry SPH",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(17)
  expect_true(any(grepl("Quadratic BPH",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(18)
  expect_true(any(grepl("Quadratic SPH",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(19)
  expect_true(any(grepl("Polynomial BPH",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(20)
  expect_true(any(grepl("Polynomial SPH",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(21)
  expect_true(any(grepl("Exponential BPH",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
  tmp <- bcFuns(22)
  expect_true(any(grepl("Exponential SPH",capture.output(tmp(5,4,3,2,1,1,verbose=TRUE)))))
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

test_that("Miscellaneous internals output",{
  msg <- "Hello Derek"
  tmp <- capture.output(RFishBC:::DONE(msg))
  expect_true(grepl(msg,tmp))
  tmp <- capture.output(RFishBC:::NOTE(msg))
  expect_true(grepl(msg,tmp))
  tmp <- capture.output(RFishBC:::RULE(msg))
  expect_true(grepl(msg,tmp))

  tmp <- iGetImage("Scale_1.jpg",windowSize=10,
                   deviceType="default",id="1",showInfo=TRUE,
                   pos.info="topleft",cex.info=1,col.info="yellow")
  grDevices::dev.off()
  expect_is(tmp,"list")
  expect_type(tmp$windowSize,"double")
  expect_equal(length(tmp$windowSize),2)
  expect_equal(tmp$windowSize[1],10)
  expect_type(tmp$pixW2H,"double")
  expect_equal(length(tmp$pixW2H),1)
  
  expect_true(isRData("Scale_1_DHO.rds"))
  expect_false(isRData("Scale_1.jpg"))
  
  ## check iOrderPts ... randomize point and see if they get ordered properly
  tmp <- dat1$pts
  tmp2 <- tmp[c(1,sample(2:6),7),]
  rownames(tmp2) <- 1:7
  tmp2 <- RFishBC:::iOrderPts(tmp2,edgeIsAnnulus=FALSE)
  expect_equal(tmp,tmp2)
  
  tmp <- dat2$pts
  tmp2 <- tmp[c(1,sample(2:14)),]
  rownames(tmp2) <- 1:14
  tmp2 <- RFishBC:::iOrderPts(tmp2,edgeIsAnnulus=TRUE)
  expect_equal(tmp,tmp2)
  
  tmp <- tmp2 <- dat1$pts[c(1,nrow(dat1$pts)),]
  rownames(tmp2) <- 1:2
  tmp2 <- RFishBC:::iOrderPts(tmp2,edgeIsAnnulus=FALSE)
  expect_equal(tmp,tmp2)
  
  tmp <- tmp2 <- dat2$pts[c(1,nrow(dat2$pts)),]
  rownames(tmp)[2] <- 1
  rownames(tmp2) <- 1:2
  tmp2 <- RFishBC:::iOrderPts(tmp2,edgeIsAnnulus=TRUE)
  expect_equal(tmp,tmp2)
  
  ## Check convert points to radii
  tmp <- data.frame(x=0,y=1:5)
  tmp2 <- RFishBC:::iPts2Rad(tmp,edgeIsAnnulus=TRUE,scalingFactor=1,
                             pixW2H=1,id=1,reading="DHO")
  expect_true(all(tmp2$agecap==4))
  expect_true(all(tmp2$radcap==4))
  expect_equal(tmp2$rad,1:4)
  tmp2 <- RFishBC:::iPts2Rad(tmp,edgeIsAnnulus=FALSE,scalingFactor=1,
                             pixW2H=1,id=1,reading="DHO")
  expect_true(all(tmp2$agecap==3))
  expect_true(all(tmp2$radcap==4))
  expect_equal(tmp2$rad,1:4)
  
  tmp <- data.frame(x=1:5,y=1:5)
  tmp2 <- RFishBC:::iPts2Rad(tmp,edgeIsAnnulus=TRUE,scalingFactor=1,
                             pixW2H=1,id=1,reading="DHO")
  expect_true(all(tmp2$agecap==4))
  expect_true(all(tmp2$radcap==4*sqrt(2)))
  expect_equal(tmp2$rad,(1:4)*sqrt(2))
  
  tmp <- data.frame(x=(1:5)/2,y=1:5)
  tmp2 <- RFishBC:::iPts2Rad(tmp,edgeIsAnnulus=TRUE,scalingFactor=1,
                             pixW2H=2,id=1,reading="DHO")
  expect_true(all(tmp2$agecap==4))
  expect_true(all(tmp2$radcap==4*sqrt(2)))
  expect_equal(tmp2$rad,(1:4)*sqrt(2))

  tmp <- data.frame(x=1:5,y=(1:5)/2)
  tmp2 <- RFishBC:::iPts2Rad(tmp,edgeIsAnnulus=TRUE,scalingFactor=1,
                             pixW2H=1/2,id=1,reading="DHO")
  expect_true(all(tmp2$agecap==4))
  expect_true(all(tmp2$radcap==4*sqrt(2)/2))
  expect_equal(tmp2$rad,(1:4)*sqrt(2)/2)
})  
