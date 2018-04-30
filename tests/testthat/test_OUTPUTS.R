context("RFishBC MESSAGES")

source("EXS_growthUtils.R")
source("EXS_collectRadii.R")

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

