## Use this to create the RData files for the vignettes.
devtools::load_all(".")
setwd("C:/aaaWork/Programs/GitHub/RFishBC/vignettes")

## Process Scales
junk <- digitizeRadii("Scale_1.jpg",id="1",reading="DHO",edgeIsAnnulus=FALSE,
                      windowSize=10,addNote=TRUE)
## choose Scale_1.jpg and set id to 1 ... use a different transect
digitizeRadii(reading="OHD",edgeIsAnnulus=FALSE,popID=TRUE)
## use yet another different transect
digitizeRadii("Scale_1.jpg",id="1",reading="ODH",edgeIsAnnulus=FALSE)
## a different fish
digitizeRadii("Scale_2.jpg",id="2",reading="DHO",edgeIsAnnulus=FALSE)
## choosing muliple files at once
### by selecting files initially
( imgs <- listFiles(ext=".jpg",other="Scale") )
( ids <- getID(imgs) )
digitizeRadii(imgs,id=ids,reading="MULT",edgeIsAnnulus=FALSE)
### by selecting files in a dialog box (select scale_1 and scale_2)
digitizeRadii(reading="MULT2",edgeIsAnnulus=FALSE)
### using selected files, but aborting the first one (to make sure it goes to the second)
digitizeRadii(imgs,id=ids,reading="ABORT",edgeIsAnnulus=FALSE)

### Treat this as if it is a spring-caught age-1 fish (only annulus is the edge)
###   Mark the transect and then no annuli
junk2 <- digitizeRadii("Scale_3.jpg",id="3",reading="DHO",edgeIsAnnulus=TRUE)
### Treat this as if it is a fall-caught age-0 fish (no annulus to measure)
###   Mark the transect and then no annuli
junk3 <- digitizeRadii("Scale_3.jpg",id="3",reading="TEMP",edgeIsAnnulus=FALSE)


#### Some tests of these functions
showDigitizedImage("Scale_1_DHO.rds")
showDigitizedImage(c("Scale_1_DHO.rds","Scale_1_OHD.rds","Scale_1_ODH.rds"))
showDigitizedImage("Scale_1_DHO.rds",pch.show="arrows")
showDigitizedImage(c("Scale_1_DHO.rds","Scale_1_OHD.rds","Scale_1_ODH.rds"),
                   pch.show="arrows",col.show=1:3)
showDigitizedImage(c("Scale_1_DHO.rds","Scale_1_OHD.rds","Scale_1_ODH.rds"),
                   pch.show="arrows",col.show=1:3,connect=FALSE)
showDigitizedImage()      # choose one file and then choose the three (of same fish)
showDigitizedImage(junk)  # uses the object created above
combineData("Scale_1_DHO.rds")
combineData(c("Scale_1_DHO.rds","Scale_1_OHD.rds","Scale_1_ODH.rds"))
combineData()             # choose one file and then choose the three
combineData(junk)         # uses the object created above
combineData(junk2)        # should show agecap=1 and 1 annulus
combineData(junk3)        # should show agecap=0 and no annuli
combineData(junk,typeOut="inc")
combineData(junk2,typeOut="inc")        # should show agecap=1 and 1 annulus
combineData(junk3,typeOut="inc")        # should show agecap=0 and no annuli
combineData(junk,formatOut="wide")
combineData(junk2,formatOut="wide")        # should show agecap=1 and 1 annulus
combineData(junk3,formatOut="wide")        # should show agecap=0 and no annuli
combineData(junk,typeOut="inc",formatOut="wide")
combineData(junk2,typeOut="inc",formatOut="wide")        # should show agecap=1 and 1 annulus
combineData(junk3,typeOut="inc",formatOut="wide")        # should show agecap=0 and no annuli
combineData(c("Scale_1_DHO.rds","Scale_3_DHO.rds","Scale_3_TEMP.rds"),formatOut="wide")


## Process the otolith (use scale bar)
digitizeRadii("Oto140306.jpg",id="140306",reading="DHO",
              description="Used to demonstrate use of scale-bar.",
              scaleBar=TRUE,scaleBarLength=1,scaleBarUnits="mm",
              edgeIsAnnulus=TRUE,windowSize=12)
showDigitizedImage("Oto140306_DHO.rds",pch.show="+",col.show="blue",
                   col.connect="white",col.ann="black",cex.ann=1,
                   annuliLabels=c(1:6,8,10,13))
showDigitizedImage("Oto140306_DHO.rds",pch.show=c(1,19),col.show=c("blue","red"),
                   col.connect="white",col.ann="black",cex.ann=1,
                   annuliLabels=c(1:6,8,10,13))
showDigitizedImage("Oto140306_DHO.rds",annuliLabels=c(1:6,8,10,13),
                   connect=FALSE,col.ann=c(rep("black",8),"white"),
                   col.show=c(rep("black",11),rep("white",2)),cex.ann=1)
showDigitizedImage("Oto140306_DHO.rds",cex.ann=0.7)
showDigitizedImage("Oto140306_DHO.rds",annuliLabels=1:6)
showDigitizedImage("Oto140306_DHO.rds",annuliLabels=c(2,5))

## Open the otolith and just get the scaling factor from the scale-bar
## then use this to supply the scaling factor rather than use the scale-bar
## see if the results are basically the same as above
( SF <- findScalingFactor("Oto140306.jpg",knownLength=1,windowSize=12) )

digitizeRadii("Oto140306.jpg",id="140306",reading="OHD",
              description="Testing provided scaling factor.",
              scaleBar=FALSE,scalingFactor=SF,edgeIsAnnulus=TRUE,
              windowSize=12)

## Show one with a curved growth trajectory
digitizeRadii("DWS_Oto_89765.jpg",id="89765",reading="DHO",
              description="Curved growth trajectory",edgeIsAnnulus=TRUE,
              windowSize=12,makeTransect=FALSE)
showDigitizedImage("DWS_Oto_89765_DHO.rds",cex.ann=0.7)
showDigitizedImage("DWS_Oto_89765_DHO.rds",cex.ann=0.7,connect=FALSE,
                   annuliLabels=c(1:10,12,14,16,18),col.ann=c("yellow","white"))
showDigitizedImage("DWS_Oto_89765_DHO.rds",connect=FALSE,
                   annuliLabels=c(1,5,10,15,19),
                   col.ann=c("yellow","white","red","green","blue"),
                   cex.ann=c(1,1.25,1.5,1.75,2))

#### Adding notes
addNote("Scale_1_ODH.rds","Test note as argument")
addNote("Scale_1_OHD.rds")
findNotes(listFiles(ext=".rds",other="Scale"))


#### Try loop for showDigitizedImages ... 1 shows graphic, 2 creates jpg file
tmp <- listFiles(".rds")
for (i in tmp) {
  showDigitizedImage(i)
  invisible(readline(prompt="Press [enter] to continue"))
} 

for (i in tmp) {
  d <- showDigitizedImage(i)
  nm <- paste0(tools::file_path_sans_ext(i),"_graph.jpg")
  dev.copy(jpeg,nm,width=d$windowSize[1],height=d$windowSize[2],units="in",res=72)
  dev.off()
} 


#### Move these to a dead directory so that they don't appear in the vignettes
fns <- c(listFiles(".rds",other="MULT"),
         listFiles(".rds",other="TEMP"),
         listFiles(".rds",other="ABORT"),
         listFiles(".rds",other="WIAFS"))
file.copy(fns,"zzzTempRdsFiles/",overwrite=TRUE)
file.remove(fns)

#### Copy this to test suite so that an error is thrown if something changed
####    Need to do this because everything is interactive
file.copy(listFiles(".rds"),"../tests/testthat/",overwrite=TRUE)
