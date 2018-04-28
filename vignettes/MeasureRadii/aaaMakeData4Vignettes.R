## Use this to create the RData files for the vignettes.

setwd("C:/aaaWork/Programs/GitHub/RFishBC/vignettes/MeasureRadii")

## Process Scales
digitizeRadii("Scale_1.jpg",id="1",reading="DHO",edgeIsAnnulus=FALSE)
digitizeRadii("Scale_1.jpg",id="1",reading="DHO2",edgeIsAnnulus=FALSE)
digitizeRadii("Scale_1.jpg",id="1",reading="DHO3",edgeIsAnnulus=FALSE)
digitizeRadii("Scale_2.jpg",id="2",reading="DHO",edgeIsAnnulus=FALSE)

## Process the otolith
digitizeRadii("Oto140306.jpg",id="140306",reading="DHO",
              description="Used to demonstrate use of scale-bar.",
              scaleBar=TRUE,scaleBarLength=1,edgeIsAnnulus=TRUE,
              windowSize=12)
