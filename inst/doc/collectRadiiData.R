## ----eval=FALSE, echo=FALSE----------------------------------------------
#  ## Run this code to actually build the vignette for the package
#  devtools::build_vignettes()
#  devtools::install()

## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
library(RFishBC)
library(dplyr)    # for mutate(), inner_join()

## ----eval=FALSE----------------------------------------------------------
#  fn <- digitizeRadii("Scale_1.png",ID="1",reading="DHO")

## ----eval=FALSE----------------------------------------------------------
#  df <- combineData("Scale_1_DHO.RData")

## ----echo=FALSE----------------------------------------------------------
## This loads the file from the external data folder
fn <- "Scale_1_DHO.RData"
load(fn)
df <- combineData(fn)

## ------------------------------------------------------------------------
df

## ------------------------------------------------------------------------
showDigitizedImage("Scale_1_DHO.RData")

## ----eval=FALSE----------------------------------------------------------
#  digitizeRadii("Scale_1.png",ID="1",reading="DHO2")

## ------------------------------------------------------------------------
showDigitizedImage("Scale_1_DHO.RData")
showDigitizedImage("Scale_1_DHO2.RData",add=TRUE,
           col.transect="red",col.pts="yellow")

## ------------------------------------------------------------------------
( fns <- listFiles("png") )

## ----eval=FALSE----------------------------------------------------------
#  digitizeRadii(fns[1],ID="1",reading="DHO")
#  digitizeRadii(fns[2],ID="2",reading="DHO")

## ----results='hide'------------------------------------------------------
( fns2 <- listFiles("RData") )

## ----echo=FALSE----------------------------------------------------------
fns2 <- fns2[-2]
fns2

## ------------------------------------------------------------------------
dfrad <- combineData(fns2)
dfrad

## ------------------------------------------------------------------------
dffish <- read.csv("FishData.csv") %>%
  mutate(ID=as.character(ID)) %>% 
  inner_join(dfrad,by="ID")
dffish

