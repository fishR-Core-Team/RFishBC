## ----eval=FALSE, echo=FALSE----------------------------------------------
#  ## Run this code to actually build the vignette for the package
#  devtools::build_vignettes()
#  devtools::install()

## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  setwd("c:/work/aging/Kiyi2014")

## ----message=FALSE-------------------------------------------------------
library(RFishBC)
library(dplyr)    # for mutate(), inner_join()
library(tidyr)    # for spread()

## ----eval=FALSE----------------------------------------------------------
#  digitizeRadii("Scale_1.jpg",ID="1",reading="DHO",edgeIsAnnulus=FALSE)

## ------------------------------------------------------------------------
RFBCoptions()$addTransect

## ----eval=FALSE----------------------------------------------------------
#  digitizeRadii("Scale_1.jpg",ID="1",reading="DHO",edgeIsAnnulus=FALSE,
#                lwd.transect=3,scaleBar=TRUE,scaleBarLength=0.6)

## ----eval=FALSE----------------------------------------------------------
#  RFBC(reading="DHO",edgeIsAnnulus=FALSE,edgeIsAnnulus=FALSE,
#       lwd.transect=3,scaleBar=TRUE,scaleBarLength=0.6)

## ----eval=FALSE----------------------------------------------------------
#  digitizeRadii("Scale_1.jpg",ID="1")

## ----eval=FALSE----------------------------------------------------------
#  df <- combineData("Scale_1_DHO.RData")

## ----echo=FALSE----------------------------------------------------------
## This loads the file from the external data folder
fn <- "Scale_1_DHO.RData"
load(fn)
df <- combineData(fn)

## ------------------------------------------------------------------------
df

## ----fig.height=4,fig.width=4*446/381------------------------------------
showDigitizedImage("Scale_1_DHO.RData")

## ----eval=FALSE----------------------------------------------------------
#  digitizeRadii("Scale_1.jpg",ID="1",reading="DHO2",edgeIsAnnulus=FALSE)

## ----fig.height=4,fig.width=4*446/381------------------------------------
showDigitizedImage("Scale_1_DHO.RData")
showDigitizedImage("Scale_1_DHO2.RData",add=TRUE,
                   col.transect="red",col.show="yellow")

## ------------------------------------------------------------------------
( fns <- listFiles("jpg") )

## ----eval=FALSE----------------------------------------------------------
#  RFBCoptions(reading="DHO",edgeIsAnnulus=FALSE)
#  digitizeRadii(fns[1],ID="1")
#  digitizeRadii(fns[2],ID="2")

## ----results='hide'------------------------------------------------------
( fns2 <- listFiles("RData") )

## ----echo=FALSE----------------------------------------------------------
fns2 <- fns2[-2]
fns2

## ------------------------------------------------------------------------
( dfrad <- combineData(fns2) )

## ------------------------------------------------------------------------
dffish <- read.csv("FishData.csv",stringsAsFactors=FALSE) %>%
  mutate(ID=as.character(ID)) %>% 
  inner_join(dfrad,by="ID")
dffish

## ------------------------------------------------------------------------
dffish2 <- spread(dffish,key=ann,value=rad,sep="rad")
dffish2

## ----eval=FALSE----------------------------------------------------------
#  write.csv(dffish,file="Kiyi2014_BCs.csv",quote=FALSE,row.names=FALSE)

