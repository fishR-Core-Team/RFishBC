## Get some real data
data(SMBassWB2,package="RFishBC")
data(SMBassWB1,package="RFishBC")
SMBassWB <- dplyr::inner_join(SMBassWB1,SMBassWB2,by="id")
SMBassWB_FL <- backCalc(SMBassWB,lencap,BCM="FRALE",inFormat="wide",digits=0)

## Make some fake (but easy) data
bctmp <- data.frame(id=1:3,agecap=1:3,lencap=c(11,12,13),radcap=1:3,
                    anu1=c(1,1,1),anu2=c(NA,2,2),anu3=c(NA,NA,3))
## Make some fake (but easy) data (but with "plus growth")
bctmp2 <- data.frame(id=1:3,agecap=1:3,lencap=c(11,12,13),radcap=1:3+0.1,
                     anu1=c(1,1,1),anu2=c(1.1,2,2),anu3=c(NA,2.1,3),
                     anu4=c(NA,NA,3.1))
