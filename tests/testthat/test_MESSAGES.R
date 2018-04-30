context("RFishBC MESSAGES")

source("EXS_growthUtils.R")
source("EXS_collectRadii.R")

test_that("RFBCoptions() error messages",{
  expect_error(RFBCoptions(sepWindow="Derek"),"TRUE,FALSE")
  expect_error(RFBCoptions(windowSize=0),"value out of range")
  expect_error(RFBCoptions(windowSize=31),"value out of range")
  expect_error(RFBCoptions(popID="Derek"),"TRUE,FALSE")
  expect_error(RFBCoptions(scalingFactor=0),"value out of range")
  expect_error(RFBCoptions(scaleBar="Derek"),"TRUE,FALSE")
  expect_error(RFBCoptions(lwd.scaleBar=0),"value out of range")
  expect_error(RFBCoptions(lwd.scaleBar=11),"value out of range")
  expect_error(RFBCoptions(showTransect="Derek"),"TRUE,FALSE")
  expect_error(RFBCoptions(snap2Transect="Derek"),"TRUE,FALSE")
  expect_error(RFBCoptions(lwd.transect=0),"value out of range")
  expect_error(RFBCoptions(lwd.transect=11),"value out of range")
  expect_error(RFBCoptions(cex.sel=0),"value out of range")
  expect_error(RFBCoptions(cex.sel=11),"value out of range")
  expect_error(RFBCoptions(cex.show=0),"value out of range")
  expect_error(RFBCoptions(cex.show=11),"value out of range")
  expect_error(RFBCoptions(showInfo="Derek"),"TRUE,FALSE")
  expect_error(RFBCoptions(pos.info="Derek"),"value out of range")
  expect_error(RFBCoptions(cex.info=0),"value out of range")
  expect_error(RFBCoptions(cex.info=11),"value out of range")
  expect_error(RFBCoptions(showAnnuliLabels="Derek"),"TRUE,FALSE")
  expect_error(RFBCoptions(cex.ann=0),"value out of range")
  expect_error(RFBCoptions(cex.ann=11),"value out of range")
})


test_that("RFBCoptions() error messages",{
  expect_error(digitizeRadii(edgeIsAnnulus="derek"),"must be TRUE or FALSE")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE),
               "Must provide a")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarLength=0),
               "must be positive")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarLength=-1),
               "must be positive")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,
                             scaleBarLength="derek"),
               "must be numeric")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=FALSE,scaleBarLength=1),
               "Can not use")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,
                             scaleBarLength=1,scalingFactor=1),
               "Can not use both")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scalingFactor="derek"),
               "must be numeric")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scalingFactor=0),
               "must be positive")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scalingFactor=-1),
               "must be positive")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,windowSize="derek"),
               "must be numeric")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,windowSize=0),
               "must be positive")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,windowSize=-1),
               "must be positive")
  expect_error(digitizeRadii("testdata/small_ex.jpg",id=1,edgeIsAnnulus=TRUE),
               "The file MUST be in the current working directory")
})


test_that("bcFuns() messages",{
  expect_error(bcFuns(),"must be chosen")
  expect_error(bcFuns(0),"BCM number must be")
  expect_error(bcFuns(23),"BCM number must be")
  expect_error(bcFuns("Derek"),"must be one of")
  expect_error(bcFuns("TVG"),"not yet implemented")
  expect_error(bcFuns(5),"not yet implemented")

  ## List all choices for bcFuns() ( TVG is not included because it
  ## is not yet implemented)
  tmp <- c("DALE","FRALE","BI","LBI","BPH","LBPH","TVG","SPH","LSPH",
           "AE","AESPH","AEBPH","MONA","MONA-BPH","MONA-SPH","WAKU",
           "FRY","MF","ABI","FRY-BPH","ABPH","FRY-SPH","ASPH","QBPH",
           "QSPH","PBPH","PSPH","EBPH","ESPH")
  tmp <- tmp[-7]
  ## Do all choices return a message with the name of the function in it
  for (i in tmp) expect_message(bcFuns(i,verbose=TRUE),i)
})


test_that("gConvert() messages",{
  ## Bad type
  expect_error(gConvert(SMBassWB,in.pre="anu",out.type="anu"),
               "should be one of")
  ## Neither or both of in.var= or in.pre= 
  expect_error(gConvert(SMBassWB),"must use one of")
  expect_warning(gConvert(SMBassWB,in.pre="anu",in.var=c("anu1","anu2")),
                 "Both 'in.var='")
  ## Variable does not exist
  expect_error(gConvert(SMBassWB,in.var=c("anu1","derek")),
               "Not all 'in.var=' variables found")
  expect_error(gConvert(SMBassWB,in.pre="derek"),
               "No variables start with")
  ## Bad variable numbers
  expect_error(gConvert(SMBassWB,in.var=c(-1,10)),
               "Non-positive column number given")
  expect_error(gConvert(SMBassWB,in.var=c(10,100)),
               "Column numbers exceed number of columns")
})

test_that("addRadCap() messages",{
  ## Neither or both of in.var= or in.pre= 
  expect_error(addRadCap(SMBassWB),"must use one of")
  expect_warning(addRadCap(SMBassWB,in.pre="anu",in.var=c("anu1","anu2")),
                 "Both 'in.var='")
  ## Variable does not exist
  expect_error(addRadCap(SMBassWB,in.var=c("anu1","derek")),
               "Not all 'in.var='")
  expect_error(addRadCap(SMBassWB,in.pre="derek"),
               "No variables start with")
  ## Bad variable numbers
  expect_error(addRadCap(SMBassWB,in.var=c(-1,10)),
               "Non-positive column number given")
  expect_error(addRadCap(SMBassWB,in.var=c(10,100)),
               "Column numbers exceed number of columns")
})
