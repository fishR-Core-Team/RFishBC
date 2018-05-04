## Use this to create the RData files for the vignettes.

setwd("C:/aaaWork/Programs/GitHub/RFishBC/vignettes/MeasureRadii")

## Process Scales
junk <- digitizeRadii("Scale_1.jpg",id="1",reading="DHO",edgeIsAnnulus=FALSE)
## choose Scale_1.jpg and set id to 1
digitizeRadii(reading="DHO2",edgeIsAnnulus=FALSE,popID=TRUE)
digitizeRadii("Scale_1.jpg",id="1",reading="DHO3",edgeIsAnnulus=FALSE)
digitizeRadii("Scale_2.jpg",id="2",reading="DHO",edgeIsAnnulus=FALSE)

#### Some tests of these functions
showDigitizedImage("Scale_1_DHO.RData")
showDigitizedImage(c("Scale_1_DHO.RData","Scale_1_DHO2.RData","Scale_1_DHO3.RData"))
showDigitizedImage()  # choose one file and then choose the three
combineData("Scale_1_DHO.RData")
combineData(c("Scale_1_DHO.RData","Scale_1_DHO2.RData","Scale_1_DHO3.RData"))
combineData()  # choose one file and then choose the two

## Process the otolith
digitizeRadii("Oto140306.jpg",id="140306",reading="DHO",
              description="Used to demonstrate use of scale-bar.",
              scaleBar=TRUE,scaleBarLength=1,edgeIsAnnulus=TRUE,
              windowSize=12)
showDigitizedImage("Oto140306_DHO.RData",cex.ann=0.7)
showDigitizedImage("Oto140306_DHO.RData",annuliLabels=1:6)
showDigitizedImage("Oto140306_DHO.RData",annuliLabels=c(2,5))

## Open the otolith and just get the scaling factor from the scale-bar
## then use this to supply the scaling factor rather than use the scale-bar
## siee ift he results are basically the same as above
(SF <- findScalingFactor("Oto140306.jpg",knownLength=1,windowSize=12) )

digitizeRadii("Oto140306.jpg",id="140306",reading="DHO2",
              description="Testing provided scaling factor.",
              scaleBar=FALSE,scalingFactor=SF,edgeIsAnnulus=TRUE,
              windowSize=12)


#### Copy this to test suite so that an error is thrown if something changed
####    Need to do this because everything is interactive
file.copy(listFiles(".RData"),"../../tests/testthat/testdata/",overwrite=TRUE)
