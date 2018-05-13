context("RFishBC MESSAGES")

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

test_that("digitizeRadii() results with scale-bar",{
  tmp <- combineData("Scale_1_DHO.rds")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),6)
  tmp <- combineData("Oto140306_DHO.rds")
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  tmp <- combineData(c("Scale_1_DHO.rds","Scale_1_DHO2.rds"))
  expect_s3_class(tmp,"data.frame")
  expect_equal(names(tmp),c("id","reading","agecap","ann","rad","radcap"))
  expect_equal(nrow(tmp),12)
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
