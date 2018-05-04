## Run script in aaaMakeData4Vignettes.R to create these objects ... make sure
## to run the last line that copies the RData files to the tests folder.

load("testdata/Scale_1_DHO.RData")
dat1 <- dat

load("testdata/Oto140306_DHO.RData")
dat2 <- dat

rm(dat)
