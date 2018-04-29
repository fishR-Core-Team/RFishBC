## Use this to create the RData files for the vignettes.

setwd("C:/aaaWork/Programs/GitHub/RFishBC/vignettes/MeasureRadii")

## Process Scales
junk <- digitizeRadii("Scale_1.jpg",id="1",reading="DHO",edgeIsAnnulus=FALSE)
## choose Scale_1.jpg and set id to 1
digitizeRadii(reading="DHO2",edgeIsAnnulus=FALSE,popID=TRUE)
digitizeRadii("Scale_1.jpg",id="1",reading="DHO3",edgeIsAnnulus=FALSE)
digitizeRadii("Scale_2.jpg",id="2",reading="DHO",edgeIsAnnulus=FALSE)

showDigitizedImage("Scale_1_DHO.RData")
showDigitizedImage(c("Scale_1_DHO.RData","Scale_1_DHO2.RData","Scale_1_DHO3.RData"))

showDigitizedImage()
combineData()

## Process the otolith
digitizeRadii("Oto140306.jpg",id="140306",reading="DHO",
              description="Used to demonstrate use of scale-bar.",
              scaleBar=TRUE,scaleBarLength=1,edgeIsAnnulus=TRUE,
              windowSize=12)
showDigitizedImage("Oto140306_DHO.RData",cex.ann=0.7)
