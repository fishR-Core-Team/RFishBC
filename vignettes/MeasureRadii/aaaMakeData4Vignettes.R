## Use this to create the RData files for the vignettes.

setwd("C:/aaaWork/Programs/GitHub/RFishBC/vignettes/MeasureRadii")

## Process Scales
junk <- digitizeRadii("Scale_1.jpg",id="1",reading="DHO",edgeIsAnnulus=FALSE)
## choose Scale_1.jpg and set id to 1
digitizeRadii(reading="DHO2",edgeIsAnnulus=FALSE,popID=TRUE)
digitizeRadii("Scale_1.jpg",id="1",reading="DHO3",edgeIsAnnulus=FALSE)
digitizeRadii("Scale_2.jpg",id="2",reading="DHO",edgeIsAnnulus=FALSE)
## choosing muliple files at once
### by selecting files initially
( imgs <- listFiles(ext=".jpg",other="Scale") )
( ids <- getID(imgs) )
digitizeRadii(imgs,id=ids,reading="DHO4",edgeIsAnnulus=FALSE)
### by selecting files in a dialog box (select scale_1 and scale_2)
digitizeRadii(reading="DHO5",edgeIsAnnulus=FALSE)


#### Some tests of these functions
showDigitizedImage("Scale_1_DHO.rds")
showDigitizedImage(c("Scale_1_DHO.rds","Scale_1_DHO2.rds","Scale_1_DHO3.rds"))
showDigitizedImage()      # choose one file and then choose the three
showDigitizedImage(junk)  # uses the object created above
combineData("Scale_1_DHO.rds")
combineData(c("Scale_1_DHO.rds","Scale_1_DHO2.rds","Scale_1_DHO3.rds"))
combineData()             # choose one file and then choose the three
combineData(junk)         # uses the object created above

## Process the otolith
digitizeRadii("Oto140306.jpg",id="140306",reading="DHO",
              description="Used to demonstrate use of scale-bar.",
              scaleBar=TRUE,scaleBarLength=1,edgeIsAnnulus=TRUE,
              windowSize=12)
showDigitizedImage("Oto140306_DHO.rds",cex.ann=0.7)
showDigitizedImage("Oto140306_DHO.rds",annuliLabels=1:6)
showDigitizedImage("Oto140306_DHO.rds",annuliLabels=c(2,5))

## Open the otolith and just get the scaling factor from the scale-bar
## then use this to supply the scaling factor rather than use the scale-bar
## see if the results are basically the same as above
(SF <- findScalingFactor("Oto140306.jpg",knownLength=1,windowSize=12) )

digitizeRadii("Oto140306.jpg",id="140306",reading="DHO2",
              description="Testing provided scaling factor.",
              scaleBar=FALSE,scalingFactor=SF,edgeIsAnnulus=TRUE,
              windowSize=12)


#### Copy this to test suite so that an error is thrown if something changed
####    Need to do this because everything is interactive
file.copy(listFiles(".rds"),"../../tests/testthat/",overwrite=TRUE)
