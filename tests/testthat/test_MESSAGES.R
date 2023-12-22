context("RFishBC MESSAGES")

source("EXS_growthUtils.R")
source("EXS_collectRadii.R")

test_that("Internal Function messages",{
  expect_error(STOP("This is a test error."),"This is a test error")
  expect_warning(WARN("This is a test warning."),"This is a test warning")

  expect_error(iHndlFilenames("../test-all.R"),"which is NOT")
  expect_silent(iHndlFilenames("Scale_1_DHO.rds"))
  expect_equal(iHndlFilenames("Scale_1_DHO.rds"),"Scale_1_DHO.rds")
})


test_that("RFBCoptions() error messages",{
  expect_warning(RFBCoptions(Derek=TRUE),
                 "Ignoring options not defined in manager")
  expect_error(RFBCoptions(windowSize=0),
               "value out of range")
  expect_error(RFBCoptions(windowSize=31),
               "value out of range")
  expect_error(RFBCoptions(deviceType="quartz"),
               "default,X11")
  expect_error(RFBCoptions(popID="Derek"),
               "TRUE,FALSE")
  expect_error(RFBCoptions(scalingFactor=0),
               "value out of range")
  expect_error(RFBCoptions(scaleBar="Derek"),
               "TRUE,FALSE")
  expect_error(RFBCoptions(showScaleBarLength="Derek"),
               "TRUE,FALSE")
  expect_error(RFBCoptions(lwd.scaleBar=0),
               "value out of range")
  expect_error(RFBCoptions(lwd.scaleBar=11),
               "value out of range")
  expect_error(RFBCoptions(makeTransect="Derek"),
               "TRUE,FALSE")
  expect_error(RFBCoptions(snap2Transect="Derek"),
               "TRUE,FALSE")
  expect_error(RFBCoptions(lwd.transect=0),
               "value out of range")
  expect_error(RFBCoptions(lwd.transect=11),
               "value out of range")
  expect_error(RFBCoptions(connect="Derek"),
               "TRUE,FALSE")
  expect_error(RFBCoptions(lwd.connect=0),
               "value out of range")
  expect_error(RFBCoptions(lwd.connect=11),
               "value out of range")
  expect_error(RFBCoptions(cex.sel=0),
               "value out of range")
  expect_error(RFBCoptions(cex.sel=11),
               "value out of range")
  expect_error(RFBCoptions(cex.show=0),
               "value out of range")
  expect_error(RFBCoptions(cex.show=11),
               "value out of range")
  expect_error(RFBCoptions(showInfo="Derek"),
               "TRUE,FALSE")
  expect_error(RFBCoptions(pos.info="Derek"),
               "value out of range")
  expect_error(RFBCoptions(cex.info=0),
               "value out of range")
  expect_error(RFBCoptions(cex.info=11),
               "value out of range")
  expect_error(RFBCoptions(showAnnuliLabels="Derek"),
               "TRUE,FALSE")
  expect_error(RFBCoptions(cex.ann=0),
               "value out of range")
  expect_error(RFBCoptions(cex.ann=11),
               "value out of range")
  expect_error(RFBCoptions(offset.ann=-0.1),
               "value out of range")
  expect_error(RFBCoptions(offset.ann=11),
               "value out of range")
  expect_error(RFBCoptions(cex.scaleBar=0),
               "value out of range")
  expect_error(RFBCoptions(cex.scaleBar=11),
               "value out of range")
})

test_that("iGetopt() error messages",{
  expect_error(RFishBC:::iGetopt("Derek"),
               "is not the name of an option")
})

test_that("digitizeRadii() error messages",{
  expect_error(digitizeRadii("notRFishBC.rds",edgeIsAnnulus=TRUE,id="1"),
               "does not appear to be a")
  expect_error(digitizeRadii(c("Scale_1_DHO.rds","Scale_1_DHO2.rds"),id=1,
               edgeIsAnnulus=TRUE),
               "Lengths of image file names and IDs must be equal")
  ## Need to suppress warning to focus on error
  expect_error(suppressWarnings(
    digitizeRadii("testdata/small_ex.jpg",id=1,edgeIsAnnulus=TRUE)
    ),
               "which is NOT")
  
  ## Errors in options arguments  
  expect_error(digitizeRadii(),
               "must be TRUE or FALSE")
  expect_error(digitizeRadii(edgeIsAnnulus="derek"),
               "must be TRUE or FALSE")
  
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE),
               "Must provide a")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarLength=1),
               "Must provide a")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarUnits="mm",
                             scaleBarLength=0),"must be positive")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarUnits="mm",
                             scaleBarLength=-1),"must be positive")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarUnits="mm",
                             scaleBarLength="derek"),"must be numeric")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarUnits=7,
                             scaleBarLength=1),"must be a character")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=FALSE,scaleBarUnits="mm",
                             scaleBarLength=1),"Can not use")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarUnits="mm",
                             scaleBarLength=1,scalingFactor=1.1),
               "Can not set both")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=FALSE,scaleBarUnits="mm"),
               "Can not use")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarLength=1,
                             scaleBarUnits="mm",col.scaleBar=1:2),
               "Can use only one color in")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,scaleBar=TRUE,scaleBarLength=1,
                             scaleBarUnits="mm",lwd.scaleBar=1:2),
               "Can use only one value in")  
  
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
  
  expect_message(try(digitizeRadii(edgeIsAnnulus=TRUE,snap2Transect=TRUE,
                                   makeTransect=FALSE,col.transect=1:2),
                     silent=TRUE),
                 "changed to 'FALSE'")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,col.transect=1:2),
               "Can use only one color in")
  expect_error(digitizeRadii(edgeIsAnnulus=TRUE,lwd.transect=1:2),
               "Can use only one value in")
  
})



test_that("combineData() messages",{
  expect_error(combineData("small_ex.jpg"),
               "not an RData file")
  expect_error(combineData("notRFishBC.rds"),
               "does not appear to be from")
  expect_error(combineData("Scale_1_DHO.rds",formatOut="Derek"),
               "should be one of")
  expect_error(combineData("Scale_1_DHO.rds",typeOut="Derek"),
               "should be one of")
})



test_that("iCheckFiles() messages",{
  expect_warning(RFishBC:::iCheckFiles(c("small_ex.jpg","Scale_1_DHO.rds")),
                 "not an RData file")
  expect_warning(RFishBC:::iCheckFiles(c("notRFishBC.rds","Scale_1_DHO.rds")),
                 "does not appear to be from")
  expect_error(RFishBC:::iCheckFiles("small_ex.jpg",showWarnings=FALSE),
               "no files left in the provided list")
  expect_error(RFishBC:::iCheckFiles("notRFishBC.rds",showWarnings=FALSE),
               "no files left in the provided list")
})



test_that("showDigitizedImage() messages",{
  expect_error(showDigitizedImage(c("Scale_1_DHO.rds","Scale_2_DHO.rds")),
               "from different structure images")
  expect_error(showDigitizedImage(c("Scale_1_DHO.rds","Oto140306_DHO.rds")),
               "from different structure images")
  expect_error(showDigitizedImage("Oto140306_DHO.rds",showAnnuliLabels=FALSE,
                                  annuliLabels=1:3),
               "not needed when")
  expect_error(showDigitizedImage("Oto140306_DHO.rds",col.scaleBar=c("blue","red")),
               "Can use only one")
  expect_error(showDigitizedImage("Oto140306_DHO.rds",lwd.scaleBar=1:2),
               "Can use only one")
  expect_error(showDigitizedImage("Oto140306_DHO.rds",pch.show=c(1,"arrows")),
               "'arrows' cannot be used")
  expect_warning(showDigitizedImage("Oto140306_DHO.rds",pch.show=1:3),
                 "was recycled")
  expect_warning(showDigitizedImage("Oto140306_DHO.rds",col.show=1:3),
                 "was recycled")
  expect_warning(showDigitizedImage("Oto140306_DHO.rds",cex.show=1:3),
                 "was recycled")
  expect_warning(showDigitizedImage("Oto140306_DHO.rds",
                                    pch.show="arrows",col.show=1:3),
                 "was recycled")
  expect_warning(showDigitizedImage("Oto140306_DHO.rds",
                                    pch.show="arrows",cex.show=1:3),
                 "was recycled")
  expect_warning(showDigitizedImage("Oto140306_DHO.rds",
                                    annuliLabels=1:5,col.ann=1:3),
                 "was recycled")
  expect_warning(showDigitizedImage("Oto140306_DHO.rds",
                                    annuliLabels=1:5,cex.ann=1:3),
                 "was recycled")
  grDevices::dev.off()
})



test_that("saveDigitizedImage() messages",{
  expect_error(saveDigitizedImage("Oto140306_DHO.rds",fileType="rad"),
               "should be one of")
})



test_that("findScalingFactor() error messages",{
  expect_error(findScalingFactor("Scale_1_DHO.rds",knownLength=1),
               "does not appear to be a")
  expect_error(findScalingFactor("Scale_1.jpg"),
               "Must provide a 'knownLength'")
  expect_error(findScalingFactor("Scale_1.jpg",knownLength=-1),
               "must be positive")
  expect_error(findScalingFactor("Scale_1.jpg",knownLength=0),
               "must be positive")
})



test_that("listFiles() messages",{
  expect_error(listFiles("bmp"),
               "No files have a")
  expect_error(listFiles(c("rds","jpg")),
               "can take only one string")
  expect_error(listFiles("rds",path=c("c:","c:\\temp")),
               "can take only one string")
  expect_error(listFiles("rds",other="Derek"),
               "contain the patterns given in")
  expect_error(listFiles("rds",other="scale",ignore.case=FALSE),
               "contain the patterns given in")
})



test_that("getID() messages",{
  expect_error(getID(listFiles("jpg",other="Oto")),"not found in all items of")
  tmp <- c(listFiles("jpg",other="Oto"),listFiles("jpg",other="Scale"))
  expect_error(getID(tmp),"not found in all items of")
  tmp <- c("Ruffe_456.jpg","Ruffe_456.jpg","Ruffe_567.jpg")
  expect_warning(getID(tmp),"All returned IDs are not unique")
})



test_that("bcFuns() messages",{
  expect_error(bcFuns(),
               "must be chosen")
  expect_error(bcFuns(0),
               "BCM number must be")
  expect_error(bcFuns(23),
               "BCM number must be")
  expect_error(bcFuns("Derek"),
               "must be one of")
  expect_error(bcFuns("TVG"),
               "not yet implemented")
  expect_error(bcFuns(5),
               "not yet implemented")
  expect_error(bcFuns(1:3),
               "Only one value may be given to")
  expect_error(bcFuns(c("DALE","FRALE")),
               "Only one value may be given to")
})


test_that("backCalc() messages",{
  ## Same messages as bcFuns()
  expect_error(backCalc(),
               "must be chosen")
  expect_error(backCalc(BCM=0),
               "BCM number must be")
  expect_error(backCalc(BCM=23),
               "BCM number must be")
  expect_error(backCalc(BCM="Derek"),
               "must be one of")
  ## Some back-calculation models not yet implemented
  expect_error(backCalc(BCM="TVG"),
               "not yet implemented")
  expect_error(backCalc(BCM=5),
               "not yet implemented")
  expect_error(backCalc(BCM="PBPH"),
               "not yet implemented")
  expect_error(backCalc(BCM=19),
               "not yet implemented")
  expect_error(backCalc(BCM="PSPH"),
               "not yet implemented")
  expect_error(backCalc(BCM=20),
               "not yet implemented")
  ## Too many selections
  expect_error(backCalc(SMBassWB,BCM=1:3,inFormat="wide"),
               "Only one value may be given to")
  expect_error(backCalc(SMBassWB,BCM=c("DALE","FRALE"),inFormat="wide"),
               "Only one value may be given to")
  ## Missing parameters
  expect_error(backCalc(SMBassWB,BCM=3,inFormat="wide"),
               "Variable with length-at-capture data must be given in 'lencap'")
  expect_error(backCalc(SMBassWB,lencap,BCM=3,inFormat="wide"),
               "value must be provided for 'L0p'")
  expect_error(backCalc(SMBassWB,lencap,BCM=3,inFormat="wide",L0p=1),
               "value must be provided for 'R0p'")
  expect_error(backCalc(SMBassWB,lencap,BCM=12,inFormat="wide"),
               "value must be provided for 'L0p'")
  expect_error(backCalc(SMBassWB,lencap,BCM=12,inFormat="wide",L0p=1),
               "value must be provided for 'R0'")
  expect_error(backCalc(SMBassWB,lencap,BCM=13,inFormat="wide"),
               "value must be provided for 'L0'")
  expect_error(backCalc(SMBassWB,lencap,BCM=13,inFormat="wide",L0=1),
               "value must be provided for 'R0'")
  expect_error(backCalc(SMBassWB,lencap,BCM=13,inFormat="wide",L0=1,R0=1),
               "value must be provided for 'a'")
  expect_error(backCalc(SMBassWB,lencap,BCM=14,inFormat="wide"),
               "value must be provided for 'L0p'")
  expect_error(backCalc(SMBassWB,lencap,BCM=14,inFormat="wide",L0p=1),
               "value must be provided for 'R0p'")
  expect_error(backCalc(SMBassWB,lencap,BCM=14,inFormat="wide",L0p=1,R0p=1),
               "value must be provided for 'a'")
  ## Missing or bad inFormat or inFormat
  expect_error(backCalc(SMBassWB,lencap,BCM=3),
               "'inFormat' must be")
  expect_error(backCalc(SMBassWB,lencap,BCM=3,inFormat="derek"),
               "'inFormat' must be")
  expect_error(backCalc(SMBassWB,lencap,BCM=3,inFormat="wide",outFormat="derek"),
               "'outFormat' must be 'wide' or 'long'")
})


test_that("aStandard() messages",{
  expect_error(aStandard(),
               "must be one of")
  expect_error(aStandard("Derek"),
               "must be one of")
  expect_error(aStandard(c("Walleye","Bluegill")),
               "have only one name in")
})


test_that("gConvert() messages",{
  ## Bad type
  expect_error(gConvert(SMBassWB,in.pre="rad",out.type="anu"),
               "should be one of")
  ## Neither or both of in.var= or in.pre= 
  expect_error(gConvert(SMBassWB),
               "must use one of")
  expect_warning(gConvert(SMBassWB,in.pre="rad",in.var=c("rad1","rad2")),
                 "Both 'in.var='")
  ## Variable does not exist
  expect_error(gConvert(SMBassWB,in.var=c("rad1","derek")),
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
  expect_error(addRadCap(SMBassWB),
               "must use one of")
  expect_warning(addRadCap(SMBassWB,in.pre="rad",in.var=c("rad1","rad2")),
                 "Both 'in.var='")
  ## Variable does not exist
  expect_error(addRadCap(SMBassWB,in.var=c("rad1","derek")),
               "Not all 'in.var='")
  expect_error(addRadCap(SMBassWB,in.pre="derek"),
               "No variables start with")
  ## Bad variable numbers
  expect_error(addRadCap(SMBassWB,in.var=c(-1,10)),
               "Non-positive column number given")
  expect_error(addRadCap(SMBassWB,in.var=c(10,100)),
               "Column numbers exceed number of columns")
})
