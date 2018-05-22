StdIntLit <- read.csv("data-raw/StdIntLit.csv",stringsAsFactors=FALSE)
str(StdIntLit)
save(StdIntLit,file="data/StdIntLit.RData")
