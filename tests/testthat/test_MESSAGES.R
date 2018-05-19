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


test_that("digitizeRadii() error messages",{
  expect_error(digitizeRadii("notRFishBC.rds",edgeIsAnnulus=TRUE,id="1"),
               "does not appear to be a")
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
                             scaleBarLength=1,scalingFactor=1.1),
               "Can not set both")
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



test_that("combineData() messages",{
  expect_error(combineData("small_ex.jpg"),"not an RData file")
  expect_error(combineData("notRFishBC.rds"),"does not appear to be from")
  expect_error(combineData("Scale_1_DHO.rds",outFormat="Derek"),
               "should be one of")
})



test_that("showDigitizedImage() messages",{
  expect_error(showDigitizedImage("small_ex.jpg"),"not an RData file")
  expect_error(showDigitizedImage("notRFishBC.rds"),"does not appear to be from")
  expect_error(showDigitizedImage(c("Scale_1_DHO.rds","Scale_2_DHO.rds")),
               "from different structure images")
  expect_error(showDigitizedImage(c("Scale_1_DHO.rds","Oto140306_DHO.rds")),
               "from different structure images")
  expect_error(showDigitizedImage("Oto140306_DHO.rds",showAnnuliLabels=FALSE,
                                  annuliLabels=1:3),"not needed when")
})



test_that("findScalingFactor() error messages",{
  expect_error(findScalingFactor("Scale_1_DHO.rds",knownLength=1),
               "does not appear to be a")
  expect_error(findScalingFactor("Scale_1.jpg"),"Must provide a 'knownLength'")
  expect_error(findScalingFactor("Scale_1.jpg",knownLength=-1),"must be positive")
  expect_error(findScalingFactor("Scale_1.jpg",knownLength=0),"must be positive")
})



test_that("listFiles() messages",{
  expect_error(listFiles("bmp"),"No files have a")
  expect_error(listFiles(c("rds","jpg")),"can take only one string")
  expect_error(listFiles("rds",path=c("c:","c:\\temp")),"can take only one string")
  expect_error(listFiles("rds",other="Derek"),"contain the patterns given in")
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


test_that("Specific BCFunctions messages",{
  expect_error(DahlLea("derek",5,10),"must be numeric")
  expect_error(DahlLea(100,"derek",10),"must be numeric")
  expect_error(DahlLea(100,5,"derek"),"must be numeric")
  expect_error(DahlLea(c(100,"derek"),c(5,8),c(10,10)),"must be numeric")
  expect_error(DahlLea(c(100,100),c("derek",8),c(10,10)),"must be numeric")
  expect_error(DahlLea(c(100,100),c(5,8),c("derek",10)),"must be numeric")
  expect_error(DahlLea(-1,5,10),"must be positive")
  expect_error(DahlLea(0,5,10),"must be positive")
  expect_error(DahlLea(100,-1,10),"must be positive")
  expect_error(DahlLea(100,0,10),"must be positive")
  expect_error(DahlLea(100,5,-1),"must be positive")
  expect_error(DahlLea(100,5,0),"must be positive")
  expect_error(DahlLea(c(100,0),c(5,8),c(10,10)),"must be positive")
  expect_error(DahlLea(c(100,100),c(0,8),c(10,10)),"must be positive")
  expect_error(DahlLea(c(100,100),c(5,8),c(0,10)),"must be positive")
  
  expect_error(FraserLee("derek",5,10,4),"must be numeric")
  expect_error(FraserLee(100,"derek",10,4),"must be numeric")
  expect_error(FraserLee(100,5,"derek",4),"must be numeric")
  expect_error(FraserLee(c(100,"derek"),c(5,8),c(10,10),4),"must be numeric")
  expect_error(FraserLee(c(100,100),c("derek",8),c(10,10),4),"must be numeric")
  expect_error(FraserLee(c(100,100),c(5,8),c("derek",10),4),"must be numeric")
  expect_error(FraserLee(-1,5,10,4),"must be positive")
  expect_error(FraserLee(0,5,10,4),"must be positive")
  expect_error(FraserLee(100,-1,10,4),"must be positive")
  expect_error(FraserLee(100,0,10,4),"must be positive")
  expect_error(FraserLee(100,5,-1,4),"must be positive")
  expect_error(FraserLee(100,5,0,4),"must be positive")
  expect_error(FraserLee(c(100,0),c(5,8),c(10,10),4),"must be positive")
  expect_error(FraserLee(c(100,100),c(0,8),c(10,10),4),"must be positive")
  expect_error(FraserLee(c(100,100),c(5,8),c(0,10),4),"must be positive")
  expect_error(FraserLee(100,5,10,"derek"),"must be numeric")
  expect_error(FraserLee(100,5,10,c(10,5)),"must be a single value")
  
  expect_error(SPH("derek",5,10,4,0.5),"must be numeric")
  expect_error(SPH(100,"derek",10,4,0.5),"must be numeric")
  expect_error(SPH(100,5,"derek",4,0.5),"must be numeric")
  expect_error(SPH(c(100,"derek"),c(5,8),c(10,10),4,0.5),"must be numeric")
  expect_error(SPH(c(100,100),c("derek",8),c(10,10),4,0.5),"must be numeric")
  expect_error(SPH(c(100,100),c(5,8),c("derek",10),4,0.5),"must be numeric")
  expect_error(SPH(-1,5,10,4,0.5),"must be positive")
  expect_error(SPH(0,5,10,4,0.5),"must be positive")
  expect_error(SPH(100,-1,10,4,0.5),"must be positive")
  expect_error(SPH(100,0,10,4,0.5),"must be positive")
  expect_error(SPH(100,5,-1,4,0.5),"must be positive")
  expect_error(SPH(100,5,0,4,0.5),"must be positive")
  expect_error(SPH(c(100,0),c(5,8),c(10,10),4,0.5),"must be positive")
  expect_error(SPH(c(100,100),c(0,8),c(10,10),4,0.5),"must be positive")
  expect_error(SPH(c(100,100),c(5,8),c(0,10),4,0.5),"must be positive")
  expect_error(SPH(100,5,10,"derek",0.5),"must be numeric")
  expect_error(SPH(100,5,10,c(10,5),0.5),"must be a single value")
  expect_error(SPH(100,5,10,0.5,"derek"),"must be numeric")
  expect_error(SPH(100,5,10,0.5,c(10,5)),"must be a single value")
  
  expect_error(BPH("derek",5,10,4,0.5),"must be numeric")
  expect_error(BPH(100,"derek",10,4,0.5),"must be numeric")
  expect_error(BPH(100,5,"derek",4,0.5),"must be numeric")
  expect_error(BPH(c(100,"derek"),c(5,8),c(10,10),4,0.5),"must be numeric")
  expect_error(BPH(c(100,100),c("derek",8),c(10,10),4,0.5),"must be numeric")
  expect_error(BPH(c(100,100),c(5,8),c("derek",10),4,0.5),"must be numeric")
  expect_error(BPH(-1,5,10,4,0.5),"must be positive")
  expect_error(BPH(0,5,10,4,0.5),"must be positive")
  expect_error(BPH(100,-1,10,4,0.5),"must be positive")
  expect_error(BPH(100,0,10,4,0.5),"must be positive")
  expect_error(BPH(100,5,-1,4,0.5),"must be positive")
  expect_error(BPH(100,5,0,4,0.5),"must be positive")
  expect_error(BPH(c(100,0),c(5,8),c(10,10),4,0.5),"must be positive")
  expect_error(BPH(c(100,100),c(0,8),c(10,10),4,0.5),"must be positive")
  expect_error(BPH(c(100,100),c(5,8),c(0,10),4,0.5),"must be positive")
  expect_error(BPH(100,5,10,"derek",0.5),"must be numeric")
  expect_error(BPH(100,5,10,c(10,5),0.5),"must be a single value")
  expect_error(BPH(100,5,10,0.5,"derek"),"must be numeric")
  expect_error(BPH(100,5,10,0.5,c(10,5)),"must be a single value")
  
  expect_error(BiolInt("derek",5,10,4,0.5),"must be numeric")
  expect_error(BiolInt(100,"derek",10,4,0.5),"must be numeric")
  expect_error(BiolInt(100,5,"derek",4,0.5),"must be numeric")
  expect_error(BiolInt(c(100,"derek"),c(5,8),c(10,10),4,0.5),"must be numeric")
  expect_error(BiolInt(c(100,100),c("derek",8),c(10,10),4,0.5),"must be numeric")
  expect_error(BiolInt(c(100,100),c(5,8),c("derek",10),4,0.5),"must be numeric")
  expect_error(BiolInt(-1,5,10,4,0.5),"must be positive")
  expect_error(BiolInt(0,5,10,4,0.5),"must be positive")
  expect_error(BiolInt(100,-1,10,4,0.5),"must be positive")
  expect_error(BiolInt(100,0,10,4,0.5),"must be positive")
  expect_error(BiolInt(100,5,-1,4,0.5),"must be positive")
  expect_error(BiolInt(100,5,0,4,0.5),"must be positive")
  expect_error(BiolInt(c(100,0),c(5,8),c(10,10),4,0.5),"must be positive")
  expect_error(BiolInt(c(100,100),c(0,8),c(10,10),4,0.5),"must be positive")
  expect_error(BiolInt(c(100,100),c(5,8),c(0,10),4,0.5),"must be positive")
  expect_error(BiolInt(100,5,10,"derek",0.5),"must be numeric")
  expect_error(BiolInt(100,5,10,c(10,5),0.5),"must be a single value")
  expect_error(BiolInt(100,5,10,0.5,"derek"),"must be numeric")
  expect_error(BiolInt(100,5,10,0.5,c(10,5)),"must be a single value")
  expect_error(BiolInt("derek",5,10,4,0.5),"must be numeric")
  expect_error(BiolInt(100,"derek",10,4,0.5),"must be numeric")
  expect_error(BiolInt(100,5,"derek",4,0.5),"must be numeric")
  expect_error(BiolInt(c(100,"derek"),c(5,8),c(10,10),4,0.5),"must be numeric")
  expect_error(BiolInt(c(100,100),c("derek",8),c(10,10),4,0.5),"must be numeric")
  expect_error(BiolInt(c(100,100),c(5,8),c("derek",10),4,0.5),"must be numeric")
  expect_error(BiolInt(-1,5,10,4,0.5),"must be positive")
  expect_error(BiolInt(0,5,10,4,0.5),"must be positive")
  expect_error(BiolInt(100,-1,10,4,0.5),"must be positive")
  expect_error(BiolInt(100,0,10,4,0.5),"must be positive")
  expect_error(BiolInt(100,5,-1,4,0.5),"must be positive")
  expect_error(BiolInt(100,5,0,4,0.5),"must be positive")
  expect_error(BiolInt(c(100,0),c(5,8),c(10,10),4,0.5),"must be positive")
  expect_error(BiolInt(c(100,100),c(0,8),c(10,10),4,0.5),"must be positive")
  expect_error(BiolInt(c(100,100),c(5,8),c(0,10),4,0.5),"must be positive")
  expect_error(BiolInt(100,5,10,"derek",0.5),"must be numeric")
  expect_error(BiolInt(100,5,10,c(10,5),0.5),"must be a single value")
  expect_error(BiolInt(100,5,10,0.5,"derek"),"must be numeric")
  expect_error(BiolInt(100,5,10,0.5,c(10,5)),"must be a single value")
  
  expect_error(ModFry("derek",5,10,4,0.5,0.1),"must be numeric")
  expect_error(ModFry(100,"derek",10,4,0.5,0.1),"must be numeric")
  expect_error(ModFry(100,5,"derek",4,0.5,0.1),"must be numeric")
  expect_error(ModFry(c(100,"derek"),c(5,8),c(10,10),4,0.5,0.1),"must be numeric")
  expect_error(ModFry(c(100,100),c("derek",8),c(10,10),4,0.5,0.1),"must be numeric")
  expect_error(ModFry(c(100,100),c(5,8),c("derek",10),4,0.5,0.1),"must be numeric")
  expect_error(ModFry(-1,5,10,4,0.5,0.1),"must be positive")
  expect_error(ModFry(0,5,10,4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,-1,10,4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,0,10,4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,5,-1,4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,5,0,4,0.5,0.1),"must be positive")
  expect_error(ModFry(c(100,0),c(5,8),c(10,10),4,0.5,0.1),"must be positive")
  expect_error(ModFry(c(100,100),c(0,8),c(10,10),4,0.5,0.1),"must be positive")
  expect_error(ModFry(c(100,100),c(5,8),c(0,10),4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,5,10,"derek",0.5,0.1),"must be numeric")
  expect_error(ModFry(100,5,10,c(10,5),0.5,0.1),"must be a single value")
  expect_error(ModFry(100,5,10,0.5,"derek",0.1),"must be numeric")
  expect_error(ModFry(100,5,10,0.5,c(10,5),0.1),"must be a single value")
  expect_error(ModFry("derek",5,10,4,0.5,0.1),"must be numeric")
  expect_error(ModFry(100,"derek",10,4,0.5,0.1),"must be numeric")
  expect_error(ModFry(100,5,"derek",4,0.5,0.1),"must be numeric")
  expect_error(ModFry(c(100,"derek"),c(5,8),c(10,10),4,0.5,0.1),"must be numeric")
  expect_error(ModFry(c(100,100),c("derek",8),c(10,10),4,0.5,0.1),"must be numeric")
  expect_error(ModFry(c(100,100),c(5,8),c("derek",10),4,0.5,0.1),"must be numeric")
  expect_error(ModFry(-1,5,10,4,0.5,0.1),"must be positive")
  expect_error(ModFry(0,5,10,4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,-1,10,4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,0,10,4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,5,-1,4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,5,0,4,0.5,0.1),"must be positive")
  expect_error(ModFry(c(100,0),c(5,8),c(10,10),4,0.5,0.1),"must be positive")
  expect_error(ModFry(c(100,100),c(0,8),c(10,10),4,0.5,0.1),"must be positive")
  expect_error(ModFry(c(100,100),c(5,8),c(0,10),4,0.5,0.1),"must be positive")
  expect_error(ModFry(100,5,10,"derek",0.5,0.1),"must be numeric")
  expect_error(ModFry(100,5,10,c(10,5),0.5,0.1),"must be a single value")
  expect_error(ModFry(100,5,10,0.5,"derek",0.1),"must be numeric")
  expect_error(ModFry(100,5,10,0.5,c(10,5),0.1),"must be a single value")
  expect_error(ModFry(100,5,10,0.5,0.1,"derek"),"must be numeric")
  expect_error(ModFry(100,5,10,0.5,10,c(0.1,0.3)),"must be a single value")
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
