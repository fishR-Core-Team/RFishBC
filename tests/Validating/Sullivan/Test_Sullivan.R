## Testing RFishBC with images and data from Chris Sullivan

## Load required packages
devtools::load_all("C:/aaaWork/Programs/GitHub/RFishBC")
library(dplyr)

## Set working directory to where the images are
setwd("C:/aaaWork/Programs/GitHub/RFishBC/tests/Validating/Sullivan")

## Get list of images and IDs
( nms <- listFiles("jpg") )
( ids <- getID(nms,IDpattern="\\_.*") )

## Do the digitizing
#### Set common options prior to working with each structure image
###### I used makeTransect=FALSE b/c image already had a transect
###### I increased the window size to better see the image.
RFBCoptions(reading="DHO",description="Test of RFishBC",
            makeTransect=FALSE,edgeIsAnnulus=FALSE,
            scaleBar=TRUE,scaleBarLength=0.5,windowSize=15)
#### Work through each image
###### Watch console for directions at to what to do next.
digitizeRadii(nms,ids)

## Combine the radial measurements data ... round to 3 digits to match Chris
( nms2 <- listFiles("rds") )
( dat <- combineData(nms2,formatOut="wide") %>%
    mutate_at(vars(contains("rad")),round,digits=3))


## Load the information from Chris, change the id variable name so that it can
##   be joined, change a few other names for conveniencd, combine with my radial
##   measurements, and compute % error for each
fish <- readxl::read_excel("Incremental_Measurements_fromSullivan.xlsx") %>%
  rename(id=`Fish ID`,tl=`Length of fish (mm)`,age=`Consensus Age`,
         otorad=`Otolith Radius (mm)`,
         RAM1=`Radius to Annual Mark (RAM) 1`,RAM2=`RAM 2`,
         RAM3=`RAM 3`,RAM4=`RAM 4`,RAM5=`RAM 5`,RAM6=`RAM 6`) %>%
  inner_join(dat,by="id") %>%
  mutate(PE1=(rad1-RAM1)/RAM1*100,PE2=(rad2-RAM2)/RAM2*100,
         PE3=(rad3-RAM3)/RAM3*100,PE4=(rad4-RAM4)/RAM4*100,
         PE5=(rad5-RAM5)/RAM5*100,PE0=(radcap-otorad)/otorad*100) %>%
  as.data.frame()
fish

fish %>% summarize_at(vars(contains("PE")),mean,na.rm=TRUE)

## Can look at the selections again ... select a file
showDigitizedImage(col.scaleBar="blue")
